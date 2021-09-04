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

# Links das Páginas (Pesquisa Alfanumérica)
filtro.alfanum <- c(LETTERS, 0:9)

lapply(filtro.alfanum[1:3], function(filtro){
  
  glue('{url.bovespa.empresas}BuscaEmpresaListada.aspx?Letra={filtro}&idioma=pt-br') %>%
    read_html(.) %>% 
    html_elements('td') %>% 
    html_elements('a') %>% 
    html_attr('href') %>% 
    unique(.) %>%
    {glue('{url.bovespa.empresas}{.}')}
  
}) %>%  unlist(.) -> url.bovespa.empresas.pesquisa

# Conteúdo das Páginas (Web Scraping dos dados empresariais)
# web_scraping.B3_Saulo.Guerra <- function(url.empresa){
  
  url.empresa %>%
    read_html(encoding = 'Latin1') -> empresa.pagina
  
  empresa.pagina %>% 
    html_elements('#ctl00_contentPlaceHolderConteudo_MenuEmpresasListadas1_lblNomeEmpresa') %>% 
    html_text(.) -> empresa
  
  empresa.pagina %>% 
    html_elements('iframe') %>% 
    .[2] %>% html_attr('src') %>% 
    str_remove_all('../../') %>%
    {glue(url.bovespa, .)} %>% 
    read_html(.) -> iframe
  
  iframe %>% 
    html_elements(xpath = "//td[contains(., 'Classificação Setorial:')]/following-sibling::td[1]") %>% 
    html_text(.) -> setores
  
  iframe %>% 
    html_element(xpath = ".//td[contains(., 'nio Líquido')]/following-sibling::td[1]") %>%
    html_text(.) -> patrimonio.liquido
  
  iframe %>% 
    html_element(xpath = ".//td[contains(., 'Lucro (Prejuízo) do Período')]/following-sibling::td[1]") %>%
    html_text(.) -> lucro.liquido
  
  iframe %>% 
    html_element(xpath = ".//td[contains(., 'Atividades Operacionais')]/following-sibling::td[1]") %>%
    html_text(.) -> atividades.operacionais
  
  iframe %>% 
    html_element(xpath = ".//td[contains(., 'Total de Ações')]/following-sibling::td[1]") %>%
    html_text(.) -> qtd.acoes
  
  iframe %>% 
    html_elements('#divDadosEconomicoFinanceiros h3') %>% 
    .[1] %>% html_text(.) %>% 
    str_remove_all('\\r|\\n') %>% 
    str_trim(.) %>%
    endsWith('mil') -> mil
  
  tibble(empresa, setores, 
         patrimonio.liquido, lucro.liquido,
         atividades.operacionais, qtd.acoes, mil) %>%
    return(.)
  
}

# web_scraping.B3_Saulo.Guerra(url.bovespa.empresas.pesquisa[4])

# extrai_informacoes <- function(link){
  conteudo <- link %>% 
    read_html(encoding = 'Latin1')
  
  nome.empresa <- conteudo %>% 
    html_nodes('#ctl00_contentPlaceHolderConteudo_MenuEmpresasListadas1_lblNomeEmpresa') %>% 
    html_text()
  
  conteudo.iframe <- conteudo %>% #a partir do conteúdo da página
    html_nodes('iframe') %>% #seleciona a tag iframe
    .[2] %>% #não bastasse 1, são 2 iframes na página, queremos apenas o seguindo
    html_attr('src') %>% # o atributo src contém a página que foi inserida no iframe
    str_replace_all(., '../../', '') %>% #substituimos o endereçamento relativo
    paste0(url.bovespa, .) %>% #colamos com base.url para criar endereçamento absoluto
    read_html() #finalmente pegamos o conteúdo do iframe!
  
  setores <- conteudo.iframe %>% 
    html_nodes(xpath="//td[contains(., 'Classificação Setorial:')]/following-sibling::td[1]") %>% 
    html_text()
  
  patrimonio.liquido <- conteudo.iframe %>% 
    html_node(xpath=".//td[contains(., 'nio Líquido')]/following-sibling::td[1]") %>%
    html_text()
  
  lucro.liquido <- conteudo.iframe %>% 
    html_node(xpath=".//td[contains(., 'Lucro (Prejuízo) do Período')]/following-sibling::td[1]") %>%
    html_text()
  
  atividades.operacionais <- conteudo.iframe %>% 
    html_node(xpath=".//td[contains(., 'Atividades Operacionais')]/following-sibling::td[1]") %>%
    html_text()
  
  total.acoes <- conteudo.iframe %>% 
    html_node(xpath=".//td[contains(., 'Total de Ações')]/following-sibling::td[1]") %>%
    html_text()
  
  mil <- conteudo.iframe %>% 
    html_nodes('#divDadosEconomicoFinanceiros h3') %>% 
    .[1] %>% 
    html_text() %>% 
    str_replace_all('\\r|\\n', '') %>% 
    trimws() %>% 
    endsWith('mil')  
  
  resultado <- data.frame(
    nome.empresa, setores, 
    patrimonio.liquido, lucro.liquido,
    atividades.operacionais, total.acoes, mil,
    stringsAsFactors = FALSE) 
  
  return(resultado)
}

# extrai_informacoes(url.bovespa.empresas.pesquisa[4])


# 2.2. PREÇOS
# 2.3. DEMONSTRATIVOS FINANCEIROS