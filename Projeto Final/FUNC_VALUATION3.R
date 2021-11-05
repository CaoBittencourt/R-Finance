# PACOTES -----------------------------------------------------------------


# VARIÁVEIS PARA RATIOS ------------------------------------------------------------------

# 2. MÚLTIPLOS DE MERCADO 
# 2.1 Preço por lucro por ação
lucro_liquido <- teste$`DF Consolidado - Demonstração do Resultado`$`3_11_lucro/prejuízo consolidado do período`
preço_ficticio <- rnorm(n = length(lucro_liquido), mean = 25, sd = 5) %>% round(4) 
n_ficticio <- seq(10000000, 15000000, (15000000 - 10000000)/length(lucro_liquido))[-1]  

PE_ratio <- preço_ficticio/(lucro_liquido/n_ficticio)

# 2.2. Múltiplo do Valor patrimonial
# P/VPA = Preço da ação/patrimônio líquido por ação
# VPA = Patrimônio Líquido / Número de ações no mercado
# => P/VPA = Preço da ação/(Patrimônio Líquido / Número de ações no mercado)
# Valores importantes:
# P/VPA > 1 => mercado valoriza a empresa além da equivalência patrimonial (i.e. acrescenta um bônus ao preço)
# P/VPA < 1 => mercado valoriza a empresa abaixo da equivalência patrimonial (i.e. acrescenta uma penalidade ao preço)
# P/VPA = 1 => mercado valoriza a empresa exatamente na equivalência patrimonial (i.e. sem bönus nem penalidade ao preço)
PL <- teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2_03_patrimônio líquido consolidado`

p_vpa <- preço_ficticio/(PL/n_ficticio)

# 2.3. DIVIDEND YIELD
# Dividend yield = Dividendo pago por ação / Preço por unidade de ação
DPS_ficticio <- rnorm(n = length(lucro_liquido), mean = 2.5, sd = 0.7) %>% round(4) 

dividend_yield <- DPS_ficticio/preço_ficticio

# 2.4. ENTERPRISE VALUE POR EBITDA
# EV/EBITDA = (Enteprise Value/Earnings Before Taxes Interest Depreciation and Amortization).
# EV = valor de mercado da empresa = preço da ação * número de ações (i.e. o quanto o consenso do mercado diz que a empresa vale) + DÍVIDAS (BP) - CAIXA (BP)
# EBITDA = lucratividade da empresa (desconsiderando variáveis que não têm a ver com a sua operação)
# Quanto menor o EV/EBITDA, melhor (comparar setorialmente apenas)
divida <- teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2_01_04_empréstimos e financiamentos` + teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2_02_01_empréstimos e financiamentos`
caixa <- teste$`DF Consolidado - Balanço Patrimonial Ativo`$`1_01_01_caixa e equivalentes de caixa`

enterprise_value(price = preço_ficticio, n_stocks = n_ficticio, debt = divida, cash = caixa) 

# ROE (Return on Equity) = capacidade da empresa de agregar valor (gerar retorno) para o capital próprio investido
# ROE = LUCRO LÍQUIDO (DRE) / PATRIMONIO LIQUIDO (BP) = LL/PL
# Valores importantes:
# ROE > 10% = "boa empresa" 
# ROE > 15% = "empresa muito boa" 
# E assim por diante (comparar setorialmente)

ROE <- lucro_liquido/PL

# 3.2. ROIC (Return on Invested Capital) = medida de desempenho financeiro da empresa (considerando todo o capital investido, incluindo o de terceiros)
# ROIC = NOPLAT / Valor Contábil do Capital Investido.
# NOPLAT: sigla que vem do inglês "Net Operating Profit Less Adjusted Taxes". 
# Ou seja, ele representa o lucro operacional (ou EBIT) menos os impostos.
# Capital investido: capital total alocado pela empresa, ou seja, a soma do capital dos acionistas com o capital de terceiros.
EBIT <- teste$`DF Consolidado - Demonstração do Resultado`$`3_05_resultado antes do resultado financeiro e dos tributos`
NOPLAT <- EBIT - teste$`DF Consolidado - Demonstração do Resultado`$`3_08_imposto de renda e contribuição social sobre o lucro`
# capital_acionistas <- teste$`DF Consolidado - Balanço Patrimonial Passivo`
capital_terceiros <- teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2_02_01_empréstimos e financiamentos`
# OUTRAS FÓRMULAS ---------------------------------------------------------
# EV
enterprise.value <- function(
  price # Preço da ação
  ,n_stocks # Número de ações
  ,debt # Divida Total (BP, Passivo)
  ,cash # Caixa Total (BP, Ativo)
){
  
  enterprise.value <- price*n_stocks + cash - abs(debt)
  return(enterprise.value)
  
}

# 4.1. Dívida Líquida
# DL = DIVIDA TOTAL - CAIXA (BP)

net_debt <- function(
  total_debt # Dívida Total (BPP)
  ,cash # Caixa (BPA)
  ,return_abs_value_debt = F
){
  
  net_debt <- abs(total_debt) - cash
  
  if(return_abs_value_debt){
    net_debt <- - net_debt # Negativo porque é passivo (dívida)
  }
  
  return(net_debt)
  
}




# CÁLCULOS ----------------------------------------------------------------
enterprise_value(price = preço_ficticio, n_stocks = n_ficticio, debt = divida, cash = caixa) 


function(
  DRE
  ,BPP
  ,BPA
  ,prices = NULL
  ,dividends = NULL 
  ,number_stocks = NULL
  # Depois fazer uma versão que raspa o preços, dividendos e número de ações (1 por ano)
){
  # Caso preços fornecidos
  # Caso dividendos fornecidos
  
  # Identificação das empresas e demonstrativos (empresa, ano)
  DRE %>% select(CNPJ_CIA, CD_CVM, DENOM_CIA, DT_FIM_EXERCICIO) -> ID
  
  # ENTERPRISE VALUE
  if(!is_empty(prices) & !is_empty(dividends) & !is_empty(number_stocks)){
    enterprise_value(price = prices, n_stocks = number_stocks, debt = divida, cash = caixa) -> EV
  }
  
  # RATIOS
  list(
    
  ) -> numeradores
  
  numeradores %>% append(c( 'n.EV' = EV)) -> numeradores
  
  list(
    
  ) -> divisores
  
  divisores %>% append(c(EBITDA)) -> divisores
  
  Map(
    function(num, div){num/div}
    , num = numeradores
    , div = divisores
  ) %>% bind_rows(.) -> fundamentos
  
  merge(ID, fundamentos) %>% return(.)
  
  
}


function(
  DRE
  ,BPP
  ,BPA
  ,prices = NULL
  ,dividends = NULL 
  ,number_stocks = NULL
  # Depois fazer uma versão que raspa o preços, dividendos e número de ações (1 por ano)
){
  # Caso preços fornecidos
  # Caso dividendos fornecidos
  
  # Identificação das empresas e demonstrativos (empresa, ano)
  DRE %>% select(CNPJ_CIA, CD_CVM, DENOM_CIA, DT_FIM_EXERCICIO) -> ID
  
  # ENTERPRISE VALUE
  if(!is_empty(prices) & !is_empty(dividends) & !is_empty(number_stocks)){
    enterprise_value(price = prices, n_stocks = number_stocks, debt = divida, cash = caixa) -> EV
  }
  
  # RATIOS
  list(
    
  ) -> numeradores
  
  numeradores %>% append(c( 'n.EV' = EV)) -> numeradores
  
  list(
    
  ) -> divisores
  
  divisores %>% append(c(EBITDA)) -> divisores
  
  Map(
    function(num, div){num/div}
    , num = numeradores
    , div = divisores
  ) %>% bind_rows(.) -> fundamentos
  
  merge(ID, fundamentos) %>% return(.)
  
  
}
