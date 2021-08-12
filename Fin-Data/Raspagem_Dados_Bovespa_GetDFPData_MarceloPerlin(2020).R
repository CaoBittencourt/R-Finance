# # Objetivo: valuation e visualizações diversas por empresa e setor
# Pseudo-código: for each sector => for each company apply valuation function(s)

# 1. PACOTES
pkg <- c('quantmod', 'BatchGetSymbols', 'PerformanceAnalytics',
         'tidyquant', 'GetDFPData2', 'glue', 'tidyverse')

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
bovespa.empresas.ativas.info %>%
  mutate(across(.cols = starts_with('DT'),
                .fns = dmy)) %>%
  filter(year(today()) - year(DT_CONST) >= 10) -> bovespa.empresas.ativas.info

# Setores de Atuação
bovespa.empresas.ativas.info %>%
  select(SETOR_ATIV) %>% unique(.) -> setores

bovespa.empresas.ativas.info %>%
  group_by(CATEG_REG) %>% 
  tally(.)

bovespa.empresas.ativas.info %>% 
  # filter(is.na(TP_MERC)) %>% 
  filter(CATEG_REG == 'Categoria A',
         TP_MERC == 'BOLSA') %>% 
  # group_by(TP_MERC, CATEG_REG) %>%
  tally(.)

  pull(DENOM_SOCIAL)


# search_company('Weg')
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



get_dfp_data(companies_cvm_codes = c(9067,13986),
             first_year = 2010,
             last_year = year(today()) - 1,
             type_docs = c('DRE', 'BPA', 'BPP', 'DFC_MD'),
             type_format = 'con',
             use_memoise = T,
             cache_folder = tempdir()) -> teste

teste$`DF Consolidado - Demonstração do Resultado` %>%
  filter(CD_CONTA == 3.11) %>%
  group_by(CD_CVM, DENOM_CIA, CNPJ_CIA, DT_INI_EXERC, DS_CONTA) %>%
  summarise(LL = unique(VL_CONTA)) %>% View()
  ggplot(aes(x = DT_INI_EXERC,
             y = LL)) +
  geom_line() + 
  facet_wrap(facets = vars(CD_CVM))

?get_dfp_data()
# # Limpeza e modificação dos dados
# Obs: algumas empresas estão "repetidas" (troca de nome): os cnpjs e os códigos da cvm são os mesmos, mas o nome não => Homogeneizar os nomes segundo a nomenclatura mais recente 
# If MIL => Multiplicar valores das contas por mil
# Remover colunas indesejadas
# Girar em valores das contas (wide)
# Obs: ver o que fazer com aqueles outros dados que estão nos demonstrativos, mas que não fazem parte deles (e.g. LPA etc)
# Obs: esses dados estão mal calculados eu acho



# 3. ALGORITMOS DE VALUATION
# 4. VALUATION SETORIAL
# 5. VISUALIZAÇÕES DIVERSAS


