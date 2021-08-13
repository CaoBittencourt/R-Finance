# # Objetivo: valuation e visualizações diversas por empresa e setor
# Pseudo-código: for each sector => for each company apply valuation function(s)

# 1. PACOTES
pkg <- c('quantmod', 'BatchGetSymbols', 
         'PerformanceAnalytics',
         'tidyquant', 'GetDFPData2',
         'ggthemes', 'scales', 'glue', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# 2. DADOS
# # Escopo
# Empresas ainda ativas
get_info_companies(cache_folder = tempdir()) %>%
  filter(SIT_REG == 'ATIVO') -> bovespa.empresas.ativas.info

# Dados disponíveis (de maneira conveniente) a partir de 2010 (vide http://dados.cvm.gov.br/dados/CIA_ABERTA/)
# => Escopo máximo = 10 anos
# Empresas com mais de 10 anos de atividade
anos.escopo <- 10 
ano.passado <- year(today()) - 1
ano.inicial <- ano.passado - anos.escopo

bovespa.empresas.ativas.info %>%
  mutate(across(.cols = starts_with('DT'),
                .fns = dmy)) %>%
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

# 1 Setor aleatório
setores.teste %>%
  sample(1) -> setor.teste

# 4 Companhias aleatórias
bovespa.empresas.ativas.info %>% 
  filter(SETOR_ATIV == setor.teste) %>%
  pull(CD_CVM) %>% sample(4) %>% 
  get_dfp_data(first_year = ano.inicial,
               last_year = ano.passado,
               type_docs = c('DRE', 'BPA', 'BPP', 'DFC_MD'),
               type_format = 'con',
               use_memoise = T,
               cache_folder = tempdir()) -> teste

# teste.backup <- teste
# teste.backup -> teste

# # Limpeza e modificação dos dados
# Obs: algumas empresas mudam de nome durante o período (cnpj e código da cvm permanecem os mesmos) => Homogeneizar os nomes segundo a nomenclatura mais recente 
lapply(teste, function(df){
  
  df %>% 
    # CNPJ e Código da CVM são sempre os mesmos
    group_by(CD_CVM, CNPJ_CIA) %>% 
    # Toma-se o nome mais recente para atualizar os demais
    mutate(DENOM_CIA = DENOM_CIA[which.max(DT_FIM_EXERC)])}) -> teste 

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

# Remover colunas indesejadas + adicionar o Nível de especificidade das contas 
lapply(teste, function(df){
  
  df %>%
    mutate(LVL_CONTA = str_count(CD_CONTA, '\\.')) %>% # Nível de especificidade das contas
    select(CNPJ_CIA, CD_CVM, DENOM_CIA, # ID Companhia
           DT_REFER, DT_INI_EXERC, DT_FIM_EXERC, # Datas
           GRUPO_DFP, CD_CONTA, LVL_CONTA, DS_CONTA, VL_CONTA) # Contas e Valores
  
  
}) -> teste

# Dataframe Final: Girar em valores das contas (wide) + Nível de especificidade das contas
lapply(teste, function(df, nivel_especificidade = 1){
  
  df %>% 
    filter(LVL_CONTA <= nivel_especificidade) %>% # apenas as contas dentro do nivel de especificididade indicado
    select(-LVL_CONTA) %>% # remoção do nível de especificidade para facilitar operações com o df
    pivot_wider(names_from = c(CD_CONTA, DS_CONTA),
                values_from = VL_CONTA,
                names_sort = T) # cada conta = uma coluna
  
}) -> teste

# 3. ALGORITMOS DE VALUATION
# 4. VALUATION SETORIAL
# 5. VISUALIZAÇÕES DIVERSAS
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




