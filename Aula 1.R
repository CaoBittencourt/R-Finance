# 1. PACOTES
pkg <- c('tidyverse', 'Quandl', 'tidyquant', 'quantmod',
         'BETS', 'BatchGetSymbols', 'rvest')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
    {install.packages(x); require(x)})

# 2. DADOS
# API Key do Quandl
# Quandl.api_key('YyscyKkD8_DPQskT-xZp')
# Obs: Quandl não tem dados específicos das ações do Ibovespa de graça
# => Usar quantmod


# Setores da Bolsa
# trading.view_setores <- 'https://www.tradingview.com/markets/stocks-brazilia/sectorandindustry-sector/'
# status.invest_setores <- 'https://statusinvest.com.br/acoes'
fundamentus <- 'https://www.fundamentus.com.br/detalhes.php?papel='
oceans.14 <- 'https://www.oceans14.com.br/acoes/'

# read_html(trading.view_setores) %>%
#   html_nodes('.tv-screener__symbol') %>% 
#   html_text(.) %>%
#   str_to_lower(.) %>%
#   str_replace_all(' ','-') -> setores

read_html(fundamentus) %>%
  html_node('.clearfix') %>%
  html_table(.) -> bovespa

paste0(fundamentus, bovespa[1,1]) %>%
  read_html(.) %>%
  html_node('.clearfix') %>%
  html_table(.) %>% 
  select(1,2) %>%
  rename(Papel = 1) %>%
  filter(str_detect(Papel,'(?i)Setor') |
         str_detect(Papel,'(?i)Papel')) %>%
  mutate(Papel = str_remove(Papel, '.?')) %>% 
  pivot_wider()

# read_html(oceans.14) %>%
#   html_nodes('.table-responsive') %>%
#   html_table(.) -> bovespa

# read_html('https://www.tradingview.com/screener/') %>%
#   html_nodes('.tv-data-table__tbody') %>%
#   html_table(.) -> bovespa

# # Tickers por Setores
# lapply(setores, function(x)
#   paste0(trading.view_setores, setores) %>%
#     html_nodes(.) %>%
#     html_nodes('.apply-common-tooltip') %>%
#     html_text(.)) #%>%
#   # html_table(.)
# 
# paste0(trading.view_setores, setores[1]) %>%
#   read_html(.) %>%
#   html_table('.tv-layout-width',
#              header = T)


