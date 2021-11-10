# PACOTES -----------------------------------------------------------------
pkg <- c('ggthemes'
         # , 'dygraphs'
         , 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})

# FUNÇÕES ----------------------------------------------------------------
# Recebe demonstrativos, os consolida em um único Data Frame e cria algumas variáveis de interesse
demonstrativos.consolidados <- function(
  DRE
  ,DFC
  ,BPP
  ,BPA
){
  
  # plyr::join_all(
  #   dfs = list(DRE, DFC, BPP, BPA)
  #   ,type = 'full'
  #   ,by = c('CNPJ_CIA', 'CD_CVM', 'DENOM_CIA', 'DT_INI_EXERC', 'DT_FIM_EXERC')
  # ) -> demonstrativos
  
  DRE %>%
    full_join(DFC, by = c('CD_CVM','DT_FIM_EXERC'), suffix = c('','\\.y')) %>%
    full_join(BPP, by = c('CD_CVM','DT_FIM_EXERC'), suffix = c('','\\.y')) %>%
    full_join(BPA, by = c('CD_CVM','DT_FIM_EXERC'), suffix = c('','\\.y')) %>%
    select(-ends_with('\\.y')) -> demonstrativos
  
  demonstrativos %>%
    group_by(CNPJ_CIA, CD_CVM, DENOM_CIA, DENOM_COMERC, SETOR_ATIV, DT_INI_EXERC, DT_FIM_EXERC) %>%
    transmute(
      # APENAS ANO
      ANO = year(DT_FIM_EXERC)
      
      # DRE
      ,receita.liquida = `3.01_receita de venda de bens e/ou serviços` #Acredito que é esta receita que deve-se utilizar, mas talvez esteja enganado
      ,lucro.bruto = `3.03_resultado bruto`
      ,lucro.liquido = `3.11_lucro/prejuízo consolidado do período`
      ,EBIT = `3.05_resultado antes do resultado financeiro e dos tributos`
      ,TAX = `3.08_imposto de renda e contribuição social sobre o lucro`
      
      # DFC
      ,DA = rowSums(across(contains('deprecia')), na.rm = T) #Não encontrei depreciação na DRE, então peguei no DFC
                                                             #Conferi medidas de EBITDA estimados com outros encontrados na internet e está certo assim 
      
      # BPP
      ,passivo.circulante = `2.01_passivo circulante`
      ,PL = `2.03_patrimônio líquido consolidado`
      ,divida = `2.01.04_empréstimos e financiamentos` + `2.02.01_empréstimos e financiamentos`
      
      # BPA
      ,caixa = `1.01.01_caixa e equivalentes de caixa`
      ,ativo.circulante = `1.01_ativo circulante`
      ,estoques = `1.01.04_estoques`
      
      # Variáveis derivadas
      ,EBITDA = EBIT + DA
      ,NOPLAT = EBIT - TAX
      ,capital.investido = PL + divida
      ,divida.liquida = abs(divida) - caixa
      
    ) %>% return(.)
  
}

# Recebe um Data Frame de demonstrativos consolidados e retorna os fundamentos de cada empresa
fundamentos <- function(
  demonstrativos_consolidados
  ,prices = NA
  ,dividends = NA 
  ,number.stocks = NA
  ,keep_data = F
){
  
  demonstrativos_consolidados %>%
    group_by(CNPJ_CIA, CD_CVM, DENOM_CIA, DENOM_COMERC, SETOR_ATIV, DT_INI_EXERC, DT_FIM_EXERC, ANO) %>%
    transmute(
      # INFO AÇÕES
      prices = {{prices}}
      ,dividends = {{dividends}}
      ,number.stocks = {{number.stocks}} #Essa provavelmente não é a melhor maneira de fazer isso (preços etc)
      
      # MÚLTIPLOS DE MERCADO
      ,PE_ratio = prices/(lucro.liquido/number.stocks)
      ,P_VPA = prices/(PL/number.stocks)
      ,dividend.yield = dividends/prices
      ,EV = prices*number.stocks + caixa - abs(divida)
      ,EV_EBITDA = EV/EBITDA
      
      # INDICADORES FUNDAMENTALISTAS
      ,ROE = lucro.liquido/PL
      ,ROIC = NOPLAT/capital.investido
      
      # ENDIVIDAMENTO E LIQUIDEZ
      ,divida.liquida = ifelse(abs(divida) - caixa > 0, yes = abs(divida) - caixa, no = 0)
      ,divida.liquida_PL = ifelse(divida.liquida > 0, yes = divida.liquida/PL, no = 0)
      ,liquidez_corrente = ativo.circulante/passivo.circulante
      ,liquidez_seca = (ativo.circulante - estoques)/passivo.circulante
      
      # MARGENS
      ,margem_bruta = lucro.bruto/receita.liquida
      ,margem_ebitda = EBITDA/receita.liquida
      ,margem_operac = EBIT/receita.liquida
      ,margem_liquida = lucro.liquido/receita.liquida
      
    ) -> fundamentos
  
  if(keep_data){
    fundamentos %>%
      full_join(demonstrativos_consolidados, by = c('CD_CVM', 'DT_INI_EXERC', 'DT_FIM_EXERC', 'ANO'), suffix = c('','\\.y')) %>%
      select(-ends_with('\\.y')) -> fundamentos
  }
  
  return(fundamentos)
  
}


# FUNÇÕES EM CONSTRUÇÃO ---------------------------------------------------
# Preços
addPrices <- function(
  demonstrativos_consolidados
  ,df.prices
  ,df.names.tickers.cvm
){
  
  df.prices %>% 
    full_join(df.names.tickers.cvm, by = 'ticker') %>% 
    mutate(ANO = year(ref.date))-> df.prices
  
  full_join(demonstrativos_consolidados, df.prices
            , by = c('CD_CVM', 'ANO')) %>% return(.)
  
}

# TESTES ------------------------------------------------------------------
demonstrativos.consolidados(
  DRE = teste$`DF Consolidado - Demonstração do Resultado`
  ,DFC = teste$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)`
  ,BPP = teste$`DF Consolidado - Balanço Patrimonial Passivo`
  ,BPA = teste$`DF Consolidado - Balanço Patrimonial Ativo`
) -> lalala

tickers.teste <- paste0(c('ITSA4', 'VALE3', 'PETR4', 'WEGE3'), '.SA')

BatchGetSymbols::BatchGetSymbols(
  tickers = tickers.teste
  ,first.date = lalala$DT_INI_EXERC %>% min(.)
  ,last.date = lalala$DT_FIM_EXERC %>% max(.)
) -> prices.teste 

prices.teste$df.tickers -> df.tickers

tibble(
  CD_CVM = c('ITAUSA' = 7617, 'WEG' = 5410, 'PETROBRAS' = 9512, 'VALE' = 4170)
  ,ticker = tickers.teste
) -> cvm


lalala %>%
  addPrices(
    df.prices = df.tickers
    ,df.names.tickers.cvm = cvm
  ) -> lalala

View(lalala)

# teste$`DF Consolidado - Demonstração do Resultado`$`3.99_lucro por ação - (reais / ação)` %>% unique(.)
# teste$`DF Consolidado - Demonstração do Resultado`$`3.99.01_lucro básico por ação` %>% unique(.)
# teste$`DF Consolidado - Demonstração do Resultado` %>%
#   select(DT_INI_EXERC, DENOM_COMERC, starts_with('3.99'), `3.11_lucro/prejuízo consolidado do período`) %>% 
#   View(.)

lalala %>% 
  fundamentos(
    # prices = 'price.close'
    keep_data = F) -> dsds

View(dsds)

lalala %>%
  ggplot(
    aes(x = ref.date
        ,y = price.close
    )) + 
  geom_line(size = .8, colour = 'black', alpha = .125) + 
  geom_smooth(size = 1.2, colour = 'black') + 
  facet_wrap(~ DENOM_COMERC) +
  scale_y_continuous(labels = label_dollar(suffix = '')) + 
  labs(
    title = 'Preço de Fechamento - R$ (2010 - 2020)'
    ,x = 'Período'
    ,y = 'Preço por ação'
  ) +
  ggthemes::theme_wsj() #Para publicar no WSJ (feio)


lalala %>%
  mutate(DENOM_COMERC = fct_reorder(DENOM_COMERC, EBITDA)) %>% 
  ggplot(
    aes(x = DT_FIM_EXERC
        ,y = EBITDA
    )) + 
  geom_line(size = 1.2) + 
  facet_grid(cols = vars(DENOM_COMERC)) + 
  scale_y_continuous(labels = label_dollar(suffix = ' B', scale = 1/1000000000)) + 
  labs(
    title = 'EBITDA - R$ B (2010 - 2020)'
    ,x = 'Período'
    ,y = 'EBITDA'
    ) +
  ggthemes::theme_wsj() #Para publicar no WSJ (feio)


dsds %>% 
  mutate(DENOM_COMERC = fct_reorder(DENOM_COMERC, divida.liquida)) %>% 
  ggplot(
    aes(x = DT_FIM_EXERC
        ,y = divida.liquida
    )) + 
  geom_bar(stat = 'identity') + 
  # geom_line(size = 1.2) + 
  facet_grid(cols = vars(DENOM_COMERC)) + 
  scale_y_continuous(labels = label_dollar(suffix = ' B', scale = 1/1000000000)) + 
  labs(
    title = 'Dívida Líquida - R$ B (2010 - 2020)'
    ,x = 'Período'
    ,y = 'Dívida Líquida'
  ) +
  ggthemes::theme_economist() #Para publicar no The Economist


dsds %>%
  mutate(DENOM_COMERC = fct_reorder(DENOM_COMERC, ROE)) %>% 
  ggplot(
    aes(x = DT_FIM_EXERC
        ,y = ROE
    )) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~ DENOM_COMERC) + 
  gghighlight::gghighlight(ROE > median(ROE)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = 'Return on Equity - % (2010 - 2020)'
    ,x = 'Período'
    ,y = 'ROE'
  ) +
  ggthemes::theme_solarized()





