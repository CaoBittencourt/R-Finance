# PACOTES -----------------------------------------------------------------
pkg <- c('ggthemes', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})

# FUNÇÕES ----------------------------------------------------------------
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
      # DRE
      receita.liquida = `3.01_receita de venda de bens e/ou serviços` #Acredito que é esta receita que deve-se utilizar, mas talvez esteja enganado
      ,lucro.bruto = `3.03_resultado bruto`
      ,lucro.liquido = `3.11_lucro/prejuízo consolidado do período`
      ,EBIT = `3.05_resultado antes do resultado financeiro e dos tributos`
      ,TAX = `3.08_imposto de renda e contribuição social sobre o lucro`
      # ,DA = `3.04.03_perdas pela não recuperabilidade de ativos` #Não sei se é isso as depreciações e amortizações, mas é o mais próximo que achei
      # ,DA = `3.04.02.04_depreciação e amortização`
      
      # DFC
      ,DA = rowSums(across(contains('deprecia')), na.rm = T)
      
      # BPP
      ,passivo.circulante = `2.01_passivo circulante`
      ,PL = `2.03_patrimônio líquido consolidado`
      ,divida = `2.01.04_empréstimos e financiamentos` + `2.02.01_empréstimos e financiamentos`
      # ,dividas.curto.prazo = `2.03.01_capital social realizado`
      # ,dividas.longo.prazo = `2.02.01_empréstimos e financiamentos` + `2.01.04_empréstimos e financiamentos`
      
      # BPA
      ,caixa = `1.01.01_caixa e equivalentes de caixa`
      ,ativo.circulante = `1.01_ativo circulante`
      ,estoques = `1.01.04_estoques`
      
      # Variáveis derivadas
      ,EBITDA = EBIT + DA
      ,NOPLAT = EBIT - TAX
      # ,capital.investido = PL + dividas.curto.prazo + dividas.longo.prazo
      ,capital.investido = PL + divida
      ,divida.liquida = abs(divida) - caixa
      
    ) %>% return(.)
  
}

fundamentos <- function(
  demonstrativos_consolidados
  ,prices = NA
  ,dividends = NA 
  ,number.stocks = NA
  ,keep_data = F
  # Ver como juntar corretamente as informações de preço, dividendos e ações com os demonstrativos
  # Provavlmente fazer isso separadamente (em outra função)
){
  
  demonstrativos_consolidados %>%
    group_by(CNPJ_CIA, CD_CVM, DENOM_CIA, DENOM_COMERC, SETOR_ATIV, DT_INI_EXERC, DT_FIM_EXERC) %>%
    transmute(
      # INFO AÇÕES
      prices = prices
      ,dividends = dividends
      ,number.stocks = number.stocks
      
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
      full_join(demonstrativos_consolidados, by = c('CD_CVM','DT_FIM_EXERC'), suffix = c('','\\.y')) %>%
      select(-ends_with('\\.y')) -> fundamentos
  }
  
  return(fundamentos)
  
}


# FUNÇÕES EM CONSTRUÇÃO ---------------------------------------------------


BatchGetSymbols::BatchGetSymbols(tickers = bov$tickers)
BatchGetSymbols::GetIbovStocks() -> bov


# TESTES ------------------------------------------------------------------
demonstrativos.consolidados(
  DRE = teste$`DF Consolidado - Demonstração do Resultado`
  ,DFC = teste$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)`
  ,BPP = teste$`DF Consolidado - Balanço Patrimonial Passivo`
  ,BPA = teste$`DF Consolidado - Balanço Patrimonial Ativo`
) -> lalala

lalala %>% fundamentos(keep_data = T) -> dsds

list(
  'DRE' = teste$`DF Consolidado - Demonstração do Resultado`
  ,'BPP' = teste$`DF Consolidado - Balanço Patrimonial Passivo`
  ,'BPA' = teste$`DF Consolidado - Balanço Patrimonial Ativo`
  ,'DFC' = teste$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)`
  ,'ALL' = lalala
  ,'FUNDAMENTOS' = dsds
) %>% lapply(nrow)


lalala %>%
  ggplot(
    aes(x = DT_FIM_EXERC
        ,y = EBITDA
    )) + 
  geom_line(size = 1.2) + 
  facet_grid(cols = vars(DENOM_COMERC)) + 
  ggthemes::theme_economist()

dsds %>% 
  ggplot(
    aes(x = DT_FIM_EXERC
        ,y = divida.liquida_PL
    )) + 
  geom_bar(stat = 'identity') + 
  # geom_line(size = 1.2) + 
  facet_grid(cols = vars(DENOM_COMERC)) + 
  ggthemes::theme_economist()
