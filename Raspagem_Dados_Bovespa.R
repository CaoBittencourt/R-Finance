# 1. PACOTES
pkg <- c('quantmod', 'BatchGetSymbols', 'PerformanceAnalytics',
         'tidyquant', 'rvest', 'glue', 'tidyverse')

lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# 2. DADOS
# 2.1. EMPRESAS POR SETORES

url.bovespa <- 'http://bvmf.bmfbovespa.com.br/'
url.bovespa.empresas <- glue('{url.bovespa}cias-listadas/empresas-listadas/')

# Pesquisa Alfanumérico
filtro.alfanum <- c(LETTERS, 0:9)

lapply(filtro.alfanum, function(filtro){
  
  glue('{url.bovespa.empresas}BuscaEmpresaListada.aspx?Letra={filtro}&idioma=pt-br') %>%
    read_html(.) %>% 
    html_elements('td') %>% 
    html_elements('a') %>% 
    html_attr('href') %>% 
    unique(.) %>%
    {glue('{url.bovespa.empresas}{.}')}
  
  }) %>% unlist(.) -> url.bovespa.empresas.pesquisa

lapply(url.bovespa.empresas.pesquisa, function(url.empresa){
  
  url.empresa %>% 
    read_html(encoding = 'Latin1') %>%
    html_elements('#ctl00_contentPlaceHolderConteudo_MenuEmpresasListadas1_lblNomeEmpresa') %>%
    html_text(.)
  
}) %>% unlist(.) -> bovespa.empresas.nomes



# Conteúdo das Páginas



  
  
  

# 2.2. PREÇOS
# 2.3. DEMONSTRATIVOS FINANCEIROS