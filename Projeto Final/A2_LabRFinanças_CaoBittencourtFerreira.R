# PACOTES -----------------------------------------------------------------
pkg <- c(
  'BatchGetSymbols', 'GetDFPData2' #Dados financeiros (preços e demonstrativos)
  ,'ggthemes', 'scales' #Visualização
  # , 'simfinR' #, 'simfinapi'
  ,'lubridate' ,'tidyverse' #Manipulação de dados e datas
  )

# Ativa e/ou instala os pacotes 
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Citação dos pacotes
# lapply(pkg, function(x)
#   {citation(package = x)})


# DADOS -------------------------------------------------------------------
# 1. Escopo
# Empresas ainda ativas
get_info_companies(cache_folder = tempdir()) %>%
  filter(SIT_REG == 'ATIVO') -> bovespa.empresas.ativas.info

# Dados disponíveis (de maneira conveniente) a partir de 2010 (cf. http://dados.cvm.gov.br/dados/CIA_ABERTA/)
# => Escopo máximo = 10 anos
anos.escopo <- 10 
ano.passado <- year(today()) - 1
ano.inicial <- ano.passado - anos.escopo

# Setores de atividade
bovespa.empresas.ativas.info %>% 
  select(CD_CVM, DENOM_SOCIAL, DENOM_COMERC, SETOR_ATIV) %>% 
  mutate(DENOM_COMERC = ifelse(is.na(DENOM_COMERC)
                               , yes = DENOM_SOCIAL
                               , no = DENOM_COMERC)) -> SETOR_ATIV_CVM

# Teste: ITSA, VALE, PETR, WEG
# Obs: não é necessário nomear o vetor (apenas o fiz por conveniência)
empresas.teste <- c('ITAUSA' = 7617, 'WEG' = 5410, 'PETROBRAS' = 9512, 'VALE' = 4170)


# Demonstrativos financeiros
get_dfp_data(
  companies_cvm_codes = empresas.teste
  ,first_year = ano.inicial
  ,last_year = ano.passado
  ,type_docs = c('DRE', 'BPA', 'BPP', 'DFC_MI')
  ,type_format = 'con'
  ,use_memoise = T
  ,cache_folder = tempdir()) -> teste


# LIMPEZA DOS DADOS -------------------------------------------------------
teste.backup <- teste
# teste.backup -> teste

# 2. Limpeza e modificação dos dados
# Obs: algumas empresas mudam de nome durante o período (cnpj e código da cvm permanecem os mesmos) => Homogeneizar os nomes segundo a nomenclatura mais recente 
lapply(teste, function(df){
  
  df %>% 
    # CNPJ e Código da CVM são sempre os mesmos
    group_by(CD_CVM, CNPJ_CIA) %>% 
    # Toma-se o nome mais recente para atualizar os demais
    mutate(DENOM_CIA = DENOM_CIA[which.max(DT_FIM_EXERC)])}) -> teste 

# Nomes das contas
lapply(teste, function(df){
  
  df %>%
    mutate(
      DS_CONTA = str_squish(DS_CONTA) # trim_ws fora e dentro
      ,DS_CONTA = str_to_lower(DS_CONTA) # letra minuscula
    ) 
  
}) -> teste

# If MIL => Multiplicar valores das contas por mil
# Obs: checar depois se existem outras escalas além de milhares de R$
lapply(teste, function(df){
  
  df %>%
    mutate(VL_CONTA = case_when(ESCALA_MOEDA == 'MIL'
                                ~ 1000*VL_CONTA,
                                TRUE ~ VL_CONTA)) %>%
    select(-ESCALA_MOEDA)
  
  #   group_by(ESCALA_MOEDA) %>% 
  #   tally(.)
  
}) -> teste

# NA = 0
lapply(teste, function(df){
  
  df %>%
    mutate(VL_CONTA = replace_na(VL_CONTA, 0)) # NA = 0
  
}) -> teste

# Remover colunas indesejadas + adicionar o Nível de especificidade das contas 
lapply(teste, function(df){
  
  df %>%
    mutate(
      LVL_CONTA = str_count(CD_CONTA, '\\.') # Nível de especificidade das contas
    ) %>% 
    select(
      CNPJ_CIA, CD_CVM, DENOM_CIA, # ID Companhia
      DT_INI_EXERC, DT_FIM_EXERC, # Datas
      CD_CONTA, LVL_CONTA, DS_CONTA, VL_CONTA # Contas e Valores
    ) 
  
  
}) -> teste

# Adicionar setor de atividade
lapply(teste, function(df){
  
  df %>% 
    pull(CD_CVM) %>% 
    unique(.) -> empresas
  
  SETOR_ATIV_CVM %>%
    filter(CD_CVM %in% empresas) %>%
    summarise(across(.fns = unique)) -> SETOR_ATIV_CVM
  
  full_join(df, SETOR_ATIV_CVM) %>% return(.)
  
}) -> teste

# Dataframe Final: Girar em valores das contas (wide) + Nível de especificidade das contas
lapply(teste, function(df, nivel_especificidade = 50){
  
  df %>% 
    filter(LVL_CONTA <= nivel_especificidade) %>% # apenas as contas dentro do nivel de especificididade indicado
    select(-LVL_CONTA) %>% # remoção do nível de especificidade para facilitar operações com o df
    pivot_wider(names_from = c(CD_CONTA, DS_CONTA),
                values_from = VL_CONTA,
                names_sort = T) # cada conta = uma coluna
  
}) -> teste


# FUNÇÕES ----------------------------------------------------------------
# 1. DEMONSTRATIVOS CONSOLIDADOS
# Recebe 4 demonstrativos
# Retorna-os consolidados em um único Data Frame apenas com as variáveis de interesse
demonstrativos.consolidados <- function(
  DRE #Via GetDFPData2
  ,DFC #Via GetDFPData2
  ,BPP #Via GetDFPData2
  ,BPA #Via GetDFPData2
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
      ,receita.liquida = `3.01_receita de venda de bens e/ou serviços`
      ,lucro.bruto = `3.03_resultado bruto`
      ,lucro.liquido = `3.11_lucro/prejuízo consolidado do período`
      ,EBIT = `3.05_resultado antes do resultado financeiro e dos tributos`
      ,TAX = `3.08_imposto de renda e contribuição social sobre o lucro`
      
      # DFC
      ,DA = rowSums(across(contains('deprecia')), na.rm = T)
      #Não encontrei depreciação e amortização na DRE, então utilizei os valores do DFC
      #Conferi medidas de EBITDA estimados com outros encontrados na internet e está certo assim 
      
      # BPP
      ,passivo.circulante = `2.01_passivo circulante`
      ,PL = `2.03_patrimônio líquido consolidado`
      ,divida = `2.01.04_empréstimos e financiamentos` + `2.02.01_empréstimos e financiamentos` #Dívida circulante e não circulante (curto prazo e longo prazo)
      
      # BPA
      ,ativo.total = `1_ativo total`
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

# 2. ADICIONAR PREÇOS AOS DEMONSTRATIVOS CONSOLIDADOS
# Recebe um Data Frame de demonstrativos consolidados + 
# um Data Frame de preços +
# um Data Frame identificando os tickers com os códigos da CVM para cada um
# Retorna um Data Frame de demonstrativos consolidados com preços para cada empresa
addPrices <- function(
  demonstrativos_consolidados #Via GetDFPData2 + Função de demonstrativos consolidados
  ,df.prices #Via BatchGetSymbols
  ,df.names.tickers.cvm #No formato [CD_CVM | ticker]
){
  
  df.prices %>% 
    full_join(df.names.tickers.cvm, by = 'ticker') %>% 
    mutate(ANO = year(ref.date))-> df.prices
  
  full_join(demonstrativos_consolidados, df.prices
            , by = c('CD_CVM', 'ANO')) %>% return(.)
  
}


# 3. INDICADORES DE ANÁLISE FUNDAMENTALISTA
# Recebe um Data Frame de demonstrativos consolidados
# Retorna os fundamentos de cada empresa
# Obs: os preços, dividendos e número de ações serão integrados em trabalhos futuros
# (devido à falta do número de ações e dividendos, mas os preços já estão corretamente integrados)
fundamentos <- function(
  demonstrativos_consolidados #Via GetDFPData2 + Função de demonstrativos consolidados
  ,prices = NA
  ,dividends = NA 
  ,number.stocks = NA
  ,keep_data = F #Manter os dados dos demonstrativos consolidados? Default = F para minimizar o tamanho do Data Frame 
){
  
  demonstrativos_consolidados %>%
    group_by(CNPJ_CIA, CD_CVM, DENOM_CIA, DENOM_COMERC, SETOR_ATIV, DT_INI_EXERC, DT_FIM_EXERC, ANO) %>%
    transmute(
      # INFO AÇÕES
      prices = {{prices}}
      ,dividends = {{dividends}}
      ,number.stocks = {{number.stocks}} #Essa provavelmente não é a melhor maneira de fazer isso (preços etc)
      
      # MÚLTIPLOS DE MERCADO
      ,PE_ratio = prices/(lucro.liquido/number.stocks) #Falta número de ações
      ,P_VPA = prices/(PL/number.stocks) #Falta número de ações
      ,dividend.yield = dividends/prices #Falta dividendos
      ,EV = prices*number.stocks + caixa - abs(divida) #Falta número de ações
      ,EV_EBITDA = EV/EBITDA
      
      # INDICADORES FUNDAMENTALISTAS
      ,ROI = lucro.liquido/ativo.total
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


# TESTES (DADOS) ------------------------------------------------------------------
# Demonstrativos consolidados
demonstrativos.consolidados(
  DRE = teste$`DF Consolidado - Demonstração do Resultado`
  ,DFC = teste$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)`
  ,BPP = teste$`DF Consolidado - Balanço Patrimonial Passivo`
  ,BPA = teste$`DF Consolidado - Balanço Patrimonial Ativo`
) -> demonstrativos.teste

# Preços
tickers.teste <- paste0(c('ITSA4', 'VALE3', 'PETR4', 'WEGE3'), '.SA')

BatchGetSymbols::BatchGetSymbols(
  tickers = tickers.teste
  ,first.date = demonstrativos.teste$DT_INI_EXERC %>% min(.)
  ,last.date = demonstrativos.teste$DT_FIM_EXERC %>% max(.)
) -> prices.teste 

prices.teste$df.tickers -> df.tickers

tibble(
  CD_CVM = empresas.teste
  ,ticker = tickers.teste
) -> cvm

demonstrativos.teste %>%
  addPrices(
    df.prices = df.tickers
    ,df.names.tickers.cvm = cvm
  ) -> demonstrativos.teste

# Demonstrativos consolidados com preços
View(demonstrativos.teste)

# Indicadores de análise fundamentalista
demonstrativos.teste %>% 
  fundamentos(
    keep_data = F) -> fundamentos.teste

View(fundamentos.teste)


# TESTES (VISUALIZAÇÃO) ------------------------------------------------
# Preços de fechamento
demonstrativos.teste %>%
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
  ggthemes::theme_wsj()

# EBITDA
demonstrativos.teste %>%
  mutate(DENOM_COMERC = fct_reorder(DENOM_COMERC, EBITDA)) %>% 
  ggplot(
    aes(x = DT_FIM_EXERC
        ,y = EBITDA
    )) + 
  geom_line(size = 1.2) + 
  facet_wrap(~ DENOM_COMERC, ncol = 4) +
  scale_y_continuous(labels = label_dollar(suffix = ' B', scale = 1/1000000000)) + 
  labs(
    title = 'EBITDA - R$ B (2010 - 2020)'
    ,x = 'Período'
    ,y = 'EBITDA'
  ) +
  ggthemes::theme_wsj()

# Dívida Líquida
fundamentos.teste %>% 
  mutate(DENOM_COMERC = fct_reorder(DENOM_COMERC, divida.liquida)) %>% 
  ggplot(
    aes(x = DT_FIM_EXERC
        ,y = divida.liquida
    )) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~ DENOM_COMERC, ncol = 4) +
  scale_y_continuous(labels = label_dollar(suffix = ' B', scale = 1/1000000000)) + 
  labs(
    title = 'Dívida Líquida - R$ B (2010 - 2020)'
    ,x = 'Período'
    ,y = 'Dívida Líquida'
  ) +
  ggthemes::theme_solarized_2()
