# 1. PACOTES
pkg <- c('Quandl', 'tidyquant', 'quantmod', 'BETS',
         'BatchGetSymbols', 'rvest', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# 2. DADOS
# API Key do Quandl
# Quandl.api_key('YyscyKkD8_DPQskT-xZp')
# Obs: Quandl não tem dados específicos das ações do Ibovespa de graça
# => Usar quantmod


# # Método Automático
# Tentativas mal sucedidas de raspagem: status.investing, investing.com, oceans.14
# ---------------------------------------------------------------------
# trading.view_setores <- 'https://www.tradingview.com/markets/stocks-brazilia/sectorandindustry-sector/'
# status.invest_setores <- 'https://statusinvest.com.br/acoes'
# oceans.14 <- 'https://www.oceans14.com.br/acoes/'
# investing.com <- 'https://br.investing.com/stock-screener/?sp=country::32|sector::a|industry::a|equityType::a|exchange::a%3Ceq_market_cap;1'


# read_html(trading.view_setores) %>%
#   html_nodes('.tv-screener__symbol') %>% 
#   html_text(.) %>%
#   str_to_lower(.) %>%
#   str_replace_all(' ','-') -> setores

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

# read_html(investing.com) %>%
#   html_node('#resultsContainer') %>%
#   html_table(.) %>% View(.)

# read_html(oceans.14) %>%
#   html_nodes('.table-responsive') %>%
#   html_table(.) -> bovespa


# read_html(oceans.14) %>%
#   html_node('.panel-body') %>%
#   html_table(.) 

# read_html(paste0('https://br.investing.com/stock-screener/?sp=country::32|sector::a|industry::a|equityType::a|exchange::a%3Ceq_market_cap;', i)) %>%
# html_node('.js-download-stockscreener-data')


# ---------------------------

# # Método "Automático"
# Tentativas bem sucedidas de raspagem: fundamentus

get.stock.names <- function(link = 'https://www.fundamentus.com.br/detalhes.php?papel='){
  
  read_html(link) %>%
    html_node('.clearfix') %>%
    html_table(.) %>%
    return(.)
  
}


get.sector <- function(link.setor = 'https://www.fundamentus.com.br/resultado.php?setor=',
                       link.papel = 'https://www.fundamentus.com.br/detalhes.php?papel=',
                       n.setor = 1:43,
                       get.sector.names = T){
  
  lapply(n.setor, function(i)
    read_html(paste0(link.setor, i)) %>%
      html_nodes('td') %>%
      html_nodes('a') %>%
      html_text(.) %>% 
      as_tibble(.) %>%
      rename(Papel = 1) %>%
      mutate(Setor = factor(i))) %>%
    bind_rows(.) -> setores
  
  if(get.sector.names == T){
    
    setores %>%
      group_by(Setor) %>%
      summarise(Papel = first(Papel)) -> setores.nomes
    
    
    lapply(setores.nomes$Papel, function(papel){
      read_html(paste0(link.papel, papel)) %>%
        html_nodes('td') %>%
        html_nodes('a') %>%
        html_text(.) %>%
        first(.) %>%
        as_tibble(.)}) %>%
      bind_rows(.) %>% 
      mutate(Setor = row_number()) -> nomes
    
    merge(setores, nomes) -> setores}
  
  return(setores)
  
}


get.subsector <- function(link.segmentos = 'https://www.fundamentus.com.br/resultado.php?segmento=',
                          link.papel = 'https://www.fundamentus.com.br/detalhes.php?papel=',
                          n.subsetor = 1:45,
                          get.subsector.names = T){
  
  lapply(n.subsetor, function(i)
    read_html(paste0(link.segmentos, i)) %>%
      html_nodes('td') %>%
      html_nodes('a') %>%
      html_text(.) %>% 
      as_tibble(.) %>%
      rename(Papel = 1) %>%
      mutate(Subsetor = factor(i))) %>%
    bind_rows(.) -> segmentos
  
  if(get.subsector.names == T){
    
    segmentos %>%
      group_by(Subsetor) %>%
      summarise(Papel = first(Papel)) -> segmentos.nomes
    
    
    lapply(segmentos.nomes$Papel, function(papel){
      read_html(paste0(link.papel, papel)) %>%
        html_nodes('td') %>%
        html_nodes('a') %>%
        html_text(.) %>%
        last(.) %>%
        as_tibble(.)}) %>%
      bind_rows(.) %>% 
      mutate(Subsetor = row_number()) -> nomes
    
    merge(segmentos, nomes) -> segmentos}
  
  return(segmentos)
  
}

get.sector(n.setor = 1:3) %>%
  merge()



