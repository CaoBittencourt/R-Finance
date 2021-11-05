# # Objetivo: valuation e visualizações diversas por empresa e setor
# Pseudo-código: gather data => clean data => for each sector => for each company apply valuation function(s) => visualize 
# Obs:o pacote 'GetDFPData2' organiza os demonstrativos de todas as empresas em um mesmo dataframe (uma lista de df_s, correspondendo a cada um dos demonstrativos financeiros)
# => lapply(lista_dfs, val_function(group_by(setor,empresa)))


# PACOTES -----------------------------------------------------------------
pkg <- c('quantmod', 'BatchGetSymbols',  
         'PerformanceAnalytics',
         'tidyquant', 'GetDFPData2', #'GetBCBData', #'GetHFData ', 
         'ggthemes', 'scales', 'glue', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# lapply(pkg, function(x)
#   {citation(package = x)})


# DADOS -------------------------------------------------------------------
# 1. Escopo
# Empresas ainda ativas
get_info_companies(cache_folder = tempdir()) %>%
  filter(SIT_REG == 'ATIVO') -> bovespa.empresas.ativas.info

# Dados disponíveis (de maneira conveniente) a partir de 2010 (vide http://dados.cvm.gov.br/dados/CIA_ABERTA/)
# => Escopo máximo = 10 anos
# Empresas com mais de 10 anos de atividade
# Obs: existem métodos de avaliação apropriados para empresas novas (ver Damodaran)
# => Talvez modificar o escopo depois
anos.escopo <- 10 
ano.passado <- year(today()) - 1
ano.inicial <- ano.passado - anos.escopo

bovespa.empresas.ativas.info %>%
  mutate(across(.cols = starts_with('DT'),
                .fns = dmy)) %>%
  group_by(CD_CVM) %>%
  filter(year(today()) - year(DT_CONST) >= anos.escopo) -> bovespa.empresas.ativas.info

# Setores de Atuação
bovespa.empresas.ativas.info %>%
  pull(SETOR_ATIV) %>% unique(.) -> setores

# Teste: Setores com mais de 4 empresas
bovespa.empresas.ativas.info %>%
  group_by(SETOR_ATIV) %>% 
  tally(.) %>% 
  filter(n >= 4) %>%
  arrange(n) %>%
  pull(SETOR_ATIV) -> setores.teste


# bovespa.empresas.ativas.info %>%
#   group_by(CATEG_REG) %>% 
#   tally(.)

# bovespa.empresas.ativas.info %>% 
#   # filter(is.na(TP_MERC)) %>% 
#   filter(CATEG_REG == 'Categoria A',
#          TP_MERC == 'BOLSA') %>% 
#   # group_by(TP_MERC, CATEG_REG) %>%
#   tally(.)


# lapply(setores[1], function(setor){
#   
#   bovespa.empresas.ativas.info %>%
#     filter(SETOR_ATIV == setor) %>%
#     pull(CD_CVM) %>%
#     get_dfp_data(companies_cvm_codes = .,
#                  first_year = 2019,
#                  last_year = 2020,
#                  type_docs = '*',
#                  type_format = 'con',
#                  use_memoise = T,
#                  cache_folder = tempdir())
#   
# })


# get_dfp_data(companies_cvm_codes = bovespa.empresas.ativas.info$CD_CVM,
#              first_year = 2015,
#              last_year = 2020,
#              type_docs = '*',
#              type_format = 'con',
#              use_memoise = T,
#              cache_folder = tempdir())

# 1x Setor aleatório
setores.teste %>%
  sample(1) -> setor.teste

# 4x Companhias aleatórias
bovespa.empresas.ativas.info %>% 
  filter(SETOR_ATIV == setor.teste) %>%
  pull(CD_CVM) %>% sample(4) %>% 
  get_dfp_data(first_year = ano.inicial,
               last_year = ano.passado,
               type_docs = c('DRE', 'BPA', 'BPP', 'DFC_MD'),
               type_format = 'con',
               use_memoise = T,
               cache_folder = tempdir()) -> teste

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
    mutate(LVL_CONTA = str_count(CD_CONTA, '\\.')) %>% # Nível de especificidade das contas
    select(CNPJ_CIA, CD_CVM, DENOM_CIA, # ID Companhia
           DT_REFER, DT_INI_EXERC, DT_FIM_EXERC, # Datas
           GRUPO_DFP, CD_CONTA, LVL_CONTA, DS_CONTA, VL_CONTA) # Contas e Valores
  
  
}) -> teste

# Dataframe Final: Girar em valores das contas (wide) + Nível de especificidade das contas
lapply(teste, function(df, nivel_especificidade = 2){
  
  df %>% 
    filter(LVL_CONTA <= nivel_especificidade) %>% # apenas as contas dentro do nivel de especificididade indicado
    select(-LVL_CONTA) %>% # remoção do nível de especificidade para facilitar operações com o df
    pivot_wider(names_from = c(CD_CONTA
                               , DS_CONTA
    ),
    values_from = VL_CONTA,
    names_sort = T) # cada conta = uma coluna
  
}) -> teste

# Obs: por vezes, verifica-se irregularidades nos nomes das contas (além whitespace)
# Solução: usar código das contas (ver se existem irregularidades também) e/ou usar contains("parte do nome da conta") ao manipular os dados

# depois disso, GetBatchSymbols/quantmod/etc para pegar as cotações dentro do escopo => df_cotacoes

# ALGORITMOS DE VALUATION -------------------------------------------------
# 1. FLUXO DE CAIXA DESCONTADO (FDC/DCF)
# Ver planilhas Damodaran
# Obs: possivelmente será necessário utilizar a taxa selic ou outras (mas já existem diversos APIs para isso também; e.g. o mesmo autor do pacote GetDFPData2 tem outros para esses dados)

# 2. MÚLTIPLOS DE MERCADO 
# 2.1 Preço por lucro
# P/L = Preço / lucro por ação

# 2.2. Múltiplo do Valor patrimonial
# P/VPA = Preço de mercado/patrimônio líquido por ação
# Valores importantes:
# P/VPA > 1 => mercado valoriza a empresa além da equivalência patrimonial (i.e. acrescenta um bônus ao preço)
# P/VPA < 1 => mercado valoriza a empresa abaixo da equivalência patrimonial (i.e. acrescenta uma penalidade ao preço)
# P/VPA = 1 => mercado valoriza a empresa exatamente na equivalência patrimonial (i.e. sem bönus nem penalidade ao preço)

# 2.3. DIVIDEND YIELD
# Dividend yield = Dividendo pago por ação / Preço por unidade de ação

# 2.4. ENTERPRISE VALUE POR EBITDA
# EV/EBITDA = (Enteprise Value/Earnings Before Taxes Interest Depreciation and Amortization).
# EV = valor de mercado da empresa = preço da ação * número de ações (i.e. o quanto o consenso do mercado diz que a empresa vale) + DÍVIDAS (BP) - CAIXA (BP)
# EBITDA = lucratividade da empresa (desconsiderando variáveis que não têm a ver com a sua operação)
# Quanto menor o EV/EBITDA, melhor (comparar setorialmente apenas)

# 3. INDICADORES IMPORTANTES
# 3.1. Retorno da empresa
# ROE (Return on Equity) = capacidade da empresa de agregar valor (gerar retorno) para o capital próprio investido
# ROE = LUCRO LÍQUIDO (DRE) / PATRIMONIO LIQUIDO (BP) = LL/PL
# Valores importantes:
# ROE > 10% = "boa empresa" 
# ROE > 15% = "empresa muito boa" 
# E assim por diante (comparar setorialmente)

# 3.2. ROIC (Return on Invested Capital) = medida de desempenho financeiro da empresa (considerando todo o capital investido, incluindo o de terceiros)
# ROIC = Resultado Operacional / Valor Contábil do Capital Investido.


# 4. Endividamento/Liquidez da empresa
# 4.1. Dívida Líquida
# DL = DIVIDA TOTAL - CAIXA (BP)

# 4.2. Dívida Líquida sobre Patrimônio Liquido
# DL/PL = alavangem/endividamento
# Valores importantes:
# DL/PL = 50% = alavancagem de 0.5 (metade da empresa não pertence à empresa)
# DL/PL = 100% = empresa 100% alavancada (tudo na empresa é dívida)
# Geralmente, quando menor melhor (e.g. DL/PL < 50%)

# 4.3. Liquidez corrente
# LC = Ativo circulante (BP) / Passivo circulante (BP)
# LC >= 1 => empresa capaz de liquidar as suas operações de curto prazo
# LC < 1 => empresa incapaz de liquidar as suas operações de curto prazo

# 4.4. Liquidez seca
# LC = (Ativo circulante (BP) - Estoques (BP)) / Passivo circulante (BP)
# LC >= 1 => empresa capaz de liquidar as suas operações de curto prazo
# LC < 1 => empresa incapaz de liquidar as suas operações de curto prazo

# 5. MARGENS
# 5.1. Margem Bruta = rentabilidade após os custos (quanto de cada R$1,00 de receita sobreviveu às despesas da firma e virou lucro efetivamente)
# MB = Lucro Bruto (DRE) / Receita Líquida (DRE) = LB/RL

# 5.2. Margem EBITDA = rentabilidade após os custos e desconsiderando juros, impostos e depreciações (quanto de cada R$1,00 de receita sobreviveu às despesas da firma e virou EBITDA efetivamente)
# ME = EBITDA (DRE) / Receita Líquida (DRE) = EBITDA/RL

# 5.3. Margem Operacional = lucro operacional obtido para cada unidade de venda realizada (i.e. quão rentável é a *operação* da firma)
# MO = Resultado operacional (DRE) / Receita Líquida (DRE)
# Geralmente, margem maior, firma mais lucrativa
# Exceção: quando a empresa diminui a margem para ganhar espaço no mercado (e.g. expansão, aquisição, marketing etc)

# 5.4. Margem Líquida = idem margem bruta, mas considerando apenas quando % de cada R$1,00 converteu-se em lucro *líquido* (tendo em vista todas as demais despesas da firma, e.g. impostos, depreciações)
# ML = Lucro Liquido (DRE) / Receita Líquida (DRE) = LL/RL
# Geralmente, quanto maior melhor (regra de bolso: > 10%)


# AVALIAÇÃO POR SETOR -----------------------------------------------------
# 1. SETORES
# adicionar coluna de setor do df_info (viz. SETOR_ATIV) para os dfs_demonstrativos (e.g. pela coluna do cnpj ou cvm)

# 2. APLIAÇÃO DOS MÉTODOS DE AVALIAÇÃO
# group_by SETOR_ATIV => aplicar métodos funções de valuation acima


# SIMULAÇÃO DE UMA CARTEIRA DE INVESTIMENTOS ------------------------------
# 1. Selecionar as n "melhores ações" por setor e montar uma carteira
# 2. Utilizar os pacotes "PerformanceAnalytics", "quantmod", "BatchGetSymbols" e afins para simular a performance da carteira em um período
# Obs: provavelmente fazer uma função para sugerir empresas e montar a carteira (e.g. baseado em parâmetros como número de empresas, risco, número de setores, empresas por setor, métodos de valuation preferidos etc)


# VISUALIZAÇÕES DIVERSAS --------------------------------------------------
# Exemplo
teste$`DF Consolidado - Demonstração do Resultado` %>%
  rename(Ano = DT_FIM_EXERC,
         EBITDA = `3.05_Resultado Antes do Resultado Financeiro e dos Tributos`) %>%
  ggplot(aes(x = Ano,
             y = EBITDA)) +
  geom_line(size = 1.2) +
  facet_wrap(facets = vars(DENOM_CIA)) + 
  scale_y_continuous(labels = scales::label_dollar(prefix = 'R$',
                                                   suffix = ' Bi',
                                                   scale = 1/1000000000)) +
  labs(title = glue('EBITDA (R$ Bi, {ano.inicial}-{ano.passado})'),
       caption = 'Fonte: CVM (dados importados com o pacote GetDFPData2).') +
  ggthemes::theme_economist()


# BIBLIOGRAFIA (alguns exemplos) ------------------------------------------
# 1. Valuation (Damodaran)
# https://www.amazon.com.br/Valuation-Avaliar-Empresas-Escolher-Melhores/dp/8521620497

# 2. Planilhas (Damodaran)
# https://pages.stern.nyu.edu/~adamodar/

# 3. Material da disciplina eletiva de Avaliação de Empresas
# https://ss.cursos.fgv.br/d2l/le/content/84568/Home?itemIdentifier=D2L.LE.Content.ContentObject.ModuleCO-778475

# 4. Tidy Valuation (DCF)
# https://www.business-science.io/finance/2020/02/21/tidy-discounted-cash-flow.html





teste$`DF Consolidado - Balanço Patrimonial Ativo` %>%
  filter(grepl('estoque',DS_CONTA)) %>% 
  group_by(CD_CVM, DT_FIM_EXERC) %>% 
  select(CD_CONTA, DS_CONTA, VL_CONTA) %>% View(.)


select(
  .data = teste$`DF Consolidado - Demonstração do Resultado`
  ,contains('3_11_lucro/preju')
) / 
  select(
    .data = teste$`DF Consolidado - Balanço Patrimonial Passivo`
    ,contains('2_03_patrim')
  )


roe <- function(
  DRE = teste$`DF Consolidado - Demonstração do Resultado`
  ,BPP = teste$`DF Consolidado - Balanço Patrimonial Passivo`
  ,net_profit = '`3_11_lucro/prejuízo consolidado do período`'
  ,net_worth = '`2_03_patrimônio líquido consolidado`'
){
  
  
  DRE %>% 
    group_by(CD_CVM, DT_FIM_EXERC) %>% 
    select(eval(net_profit)) %>% return(.)
  
  # DRE %>%
  #   group_by(CD_CVM, DT_FIM_EXERC) %>%
  #   select_(net_profit) -> DF.LL
  # 
  # BPP %>%
  #   group_by(CD_CVM, DT_FIM_EXERC) %>%
  #   select_(net_worth) -> DF.PL
  # 
  # merge(DF.LL, DF.PL) -> DF.ROE
  # 
  # DF.ROE %>% 
  #   mutate_(ROE = as.name(net_profit)/as.name(net_worth)) %>%
  #   return(.)
  # 
}

roe()

teste$sym('`DF Consolidado - Balanço Patrimonial Ativo`')

Map(
  function(num, div){return(num/div)}
  , num = list(
    'Liquidez_Corrente' = teste$`DF Consolidado - Balanço Patrimonial Ativo`$`1_01_ativo circulante`
    , 'Liquidez_Seca' = (teste$`DF Consolidado - Balanço Patrimonial Ativo`$`1_01_ativo circulante` - teste$`DF Consolidado - Balanço Patrimonial Ativo`$`1_01_04_estoques`)
  )
  , div = list(
    teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2_01_passivo circulante`
    ,teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2_01_passivo circulante`
  )
) %>% bind_cols(.)

