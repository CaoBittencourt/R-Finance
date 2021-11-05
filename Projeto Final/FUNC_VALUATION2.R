# PACOTES ----------------------------------------------------------------



# [DEFINIÇÔES] MÚLTIPLOS DE MERCADO ----------------------------------------------------
# 2. MÚLTIPLOS DE MERCADO 
# 2.1 Preço por lucro

# 2.2. Múltiplo do Valor patrimonial
# P/VPA = Preço da ação/patrimônio líquido por ação
# VPA = Patrimônio Líquido / Número de ações no mercado
# => P/VPA = Preço da ação/(Patrimônio Líquido / Número de ações no mercado)
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

enterprise.value <- function(
  price # Preço da ação
  ,n_stocks # Número de ações
  ,debt # Divida Total (BP, Passivo)
  ,cash # Caixa Total (BP, Ativo)
){
  
  enterprise.value <- price*n_stocks + cash - abs(debt)
  return(enterprise.value)
  
}

# [DEFINIÇÔES] INDICADORES FUNDAMENTALISTAS --------------------------------------------
# 3. INDICADORES IMPORTANTES
# 3.1. Retorno da empresa
# ROE (Return on Equity) = capacidade da empresa de agregar valor (gerar retorno) para o capital próprio investido
# ROE = LUCRO LÍQUIDO (DRE) / PATRIMONIO LIQUIDO (BP) = LL/PL
# Valores importantes:
# ROE > 10% = "boa empresa" 
# ROE > 15% = "empresa muito boa" 
# E assim por diante (comparar setorialmente)

# 3.2. ROIC (Return on Invested Capital) = medida de desempenho financeiro da empresa (considerando todo o capital investido, incluindo o de terceiros)
# ROIC = NOPLAT / Valor Contábil do Capital Investido.
# NOPLAT: sigla que vem do inglês "Net Operating Profit Less Adjusted Taxes". 
# Ou seja, ele representa o lucro operacional (ou EBIT) menos os impostos.
# Capital investido: capital total alocado pela empresa, ou seja, a soma do capital dos acionistas com o capital de terceiros.

# [DEFINIÇÔES] LIQUIDEZ E DÍVIDAS ------------------------------------------------------
# 4. Endividamento/Liquidez da empresa
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

# 4.2. Dívida Líquida sobre Patrimônio Liquido
# DL/PL = alavangem/endividamento
# Valores importantes:
# DL/PL = 50% = alavancagem de 0.5 (metade da empresa não pertence à empresa)
# DL/PL = 100% = empresa 100% alavancada (tudo na empresa é dívida)
# Geralmente, quando menor melhor (e.g. DL/PL < 50%)

debt_worth <- function(
  total_debt # Dívida Total (BPP)
  ,cash # Caixa (BPA)
  ,net_worth # Patrimônio líquido
){
  
  net_debt <- abs(total_debt) - cash
  # Dívida Líquida < 0 => DL/PL < 0 (i.e. não tem dívidas e sobra caixa)
  debt_worth <- ifelse(net_debt < 0, yes = 0, no = net_debt/net_worth) 
  return(debt_worth)
  
}

# 4.3. Liquidez corrente
# LC = Ativo circulante (BP) / Passivo circulante (BP)
# LC >= 1 => empresa capaz de liquidar as suas operações de curto prazo
# LC < 1 => empresa incapaz de liquidar as suas operações de curto prazo

# 4.4. Liquidez seca (FAZER DEPOIS)
# LC = (Ativo circulante (BP) - Estoques (BP)) / Passivo circulante (BP)
# LC >= 1 => empresa capaz de liquidar as suas operações de curto prazo
# LC < 1 => empresa incapaz de liquidar as suas operações de curto prazo

# [DEFINIÇÔES] MARGENS -----------------------------------------------------------------
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


# [DEFINIÇÔES] MODELOS ------------------------------------------------------------------


# [EXEMPLO] MÚLTIPLOS DE MERCADO ----------------------------------------------------
# 2. MÚLTIPLOS DE MERCADO 
# 2.1 Preço por lucro
pe_ratio <- function(
  price # Preço da ação
  ,NPPS_12m # Net profit per share (12 months) = LPA 12m
){
  
  p_l <- price/NPPS_12m
  return(p_l)
  
}

tibble(
  p = c(50,55,20,25)
  ,l = c(8,7,5,4)
) %>% mutate(`p/l` = pe_ratio(p,l))

# 2.2. Múltiplo do Valor patrimonial
# P/VPA = Preço da ação/patrimônio líquido por ação
# VPA = Patrimônio Líquido / Número de ações no mercado
# => P/VPA = Preço da ação/(Patrimônio Líquido / Número de ações no mercado)
# Valores importantes:
# P/VPA > 1 => mercado valoriza a empresa além da equivalência patrimonial (i.e. acrescenta um bônus ao preço)
# P/VPA < 1 => mercado valoriza a empresa abaixo da equivalência patrimonial (i.e. acrescenta uma penalidade ao preço)
# P/VPA = 1 => mercado valoriza a empresa exatamente na equivalência patrimonial (i.e. sem bönus nem penalidade ao preço)

p_vpa <- function(
  price # Preço da ação
  ,net_worth_per_share # Net Worth per share = Patrimônio líquido por ação
){
  
  p_vpa <- price/net_worth_per_share
  return(p_vpa)
  
}

tibble(
  p = c(25, 30, 35)
  ,PL = c(50000000, 65000000, 77000000)
  ,N = c(10000000, 15000000, 15500000)
) %>% mutate(
  p_vpa = p_vpa(
    price = p, net_worth_per_share = PL/N
  )
)

# 2.3. DIVIDEND YIELD
# Dividend yield = Dividendo pago por ação / Preço por unidade de ação

dividend_yield <- function(
  DPS # Dividendo por ação
  ,price # Preço da ação
){
  
  dividend_yield <- DPS/price
  return(dividend_yield)
  
}

tibble(
  p = c(14,12,15,13)
  ,dividend = c(1.5,2.2,2,1.2)
) %>% mutate(
  yield = dividend_yield(
    DPS = dividend, price = p
  )
)

# 2.4. ENTERPRISE VALUE POR EBITDA
# EV/EBITDA = (Enteprise Value/Earnings Before Taxes Interest Depreciation and Amortization).
# EV = valor de mercado da empresa = preço da ação * número de ações (i.e. o quanto o consenso do mercado diz que a empresa vale) + DÍVIDAS (BP) - CAIXA (BP)
# EBITDA = lucratividade da empresa (desconsiderando variáveis que não têm a ver com a sua operação)
# Quanto menor o EV/EBITDA, melhor (comparar setorialmente apenas)

enterprise_value <- function(
  price # Preço da ação
  ,n_stocks # Número de ações
  ,debt # Divida Total (BP, Passivo)
  ,cash # Caixa Total (BP, Ativo)
){
  
  enterprise_value <- price*n_stocks + cash - debt
  return(enterprise_value)
  
}

ev_ebitda <- function(
  enterprise_value
  ,ebitda
){
  
  ev_ebitda <- enterprise_value/ebitda
  return(ev_ebitda)
  
}

tibble(
  p = c(25,24,26,27)
  ,N = c(10000000,10050000,12000000,12050000)
  ,divida = c(50000000,70000000,45000000,60000000)
  ,caixa = c(25000000,50000000,80000000,20000000)
  ,EBITDA = c(120000000,200000000,180000000,220000000)
) %>% mutate(
  enterprise_value = enterprise_value(price = p, n_stocks = N, debt = divida, cash = caixa)
  ,ev_ebitda = ev_ebitda(enterprise_value, EBITDA)
)

# [EXEMPLO] INDICADORES FUNDAMENTALISTAS --------------------------------------------
# 3. INDICADORES IMPORTANTES
# 3.1. Retorno da empresa
# ROE (Return on Equity) = capacidade da empresa de agregar valor (gerar retorno) para o capital próprio investido
# ROE = LUCRO LÍQUIDO (DRE) / PATRIMONIO LIQUIDO (BP) = LL/PL
# Valores importantes:
# ROE > 10% = "boa empresa" 
# ROE > 15% = "empresa muito boa" 
# E assim por diante (comparar setorialmente)

roe <- function(
  net_profit # Lucro líquido (DRE)
  ,net_worth # PL = Patrimônio líquido (BP) = Ativos (BPA) - Passivos (BPP)
){
  
  roe <- net_profit/net_worth
  return(roe)
  
}

tibble(
  ativos = c(10000000000,12000000000,12500000000) #BPA
  ,passivos = c(-8000000000,-1000000000,-1025000000) #BPP
  ,lucro_liquido = c(180000000,450000000,795800000) #DRE
) %>% mutate(
  pl = ativos + passivos
  ,roe = roe(net_profit = lucro_liquido, net_worth = pl)
)


# 3.2. ROIC (Return on Invested Capital) = medida de desempenho financeiro da empresa (considerando todo o capital investido, incluindo o de terceiros)
# ROIC = NOPLAT / Valor Contábil do Capital Investido.
# NOPLAT: sigla que vem do inglês "Net Operating Profit Less Adjusted Taxes". 
# Ou seja, ele representa o lucro operacional (ou EBIT) menos os impostos.
# Capital investido: capital total alocado pela empresa, ou seja, a soma do capital dos acionistas com o capital de terceiros.

roic <- function(
  NOPLAT
  ,invested_capital
){
  
  roic <- NOPLAT/invested_capital
  return(roic)
  
}

tibble(
  EBIT = c(50000000,70000000,77000000)
  ,taxes = c(50000000*.15,70000000*.15,77000000*.15)
  ,capital_acionistas = c(100000000,150000000,120000000)
  ,capital_terceiros = c(100000000*1.25,150000000*1.3,120000000*1.28)
) %>% mutate(
  NOPLAT = EBIT - taxes
  ,invest = capital_acionistas + capital_terceiros
  ,roic = roic(NOPLAT, invest)
) 


# [EXEMPLO] LIQUIDEZ E DÍVIDAS ------------------------------------------------------
# 4. Endividamento/Liquidez da empresa
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

tibble(
  debt = c(-10000000,-50000000,-25000000)
  ,caixa = c(20000000,30000000,5000000)
) %>% mutate(
  net_debt = net_debt(total_debt = debt, cash = caixa, return_abs_value_debt = F)
)

# 4.2. Dívida Líquida sobre Patrimônio Liquido
# DL/PL = alavangem/endividamento
# Valores importantes:
# DL/PL = 50% = alavancagem de 0.5 (metade da empresa não pertence à empresa)
# DL/PL = 100% = empresa 100% alavancada (tudo na empresa é dívida)
# Geralmente, quando menor melhor (e.g. DL/PL < 50%)

debt_worth <- function(
  net_debt # Dívida líquida
  ,net_worth # Patrimônio líquido
){
  
  # Dívida Líquida < 0 => DL/PL < 0 (i.e. não tem dívidas e sobra caixa)
  debt_worth <- ifelse(net_debt < 0, yes = 0, no = net_debt/net_worth) 
  return(debt_worth)
  
}

tibble(
  debt = c(-10000000,-50000000,-25000000)
  ,caixa = c(1400000,1500000,2500000)
  ,ativos = c(120000000,130000000,100000000)
  ,passivos = c(80000000,70000000,75000000)
) %>% mutate(
  net_debt = net_debt(total_debt = debt, cash = caixa, return_abs_value_debt = F)
  ,PL = ativos - passivos
  ,alavancagem = debt_worth(net_debt = net_debt, net_worth = PL)
)

# 4.3. Liquidez corrente
# LC = Ativo circulante (BP) / Passivo circulante (BP)
# LC >= 1 => empresa capaz de liquidar as suas operações de curto prazo
# LC < 1 => empresa incapaz de liquidar as suas operações de curto prazo

current_liquidity <- function(
  assets #Ativo (BPA)
  ,liabilities #Passivo (BPP)
){
  
  current_liquidity <- assets/liabilities
  return(current_liquidity)
  
}

tibble(
  ativos = c(120000000,130000000,100000000)
  ,passivos = c(80000000,70000000,75000000)
) %>% mutate(
  LC = current_liquidity(ativos, passivos)
  ,DESC_LC = case_when(
    LC >= 1 ~ 'Empresa líquida'
    ,LC < 1 ~ 'Empresa não líquida'
  )
)


# # 4.4. Liquidez seca (FAZER DEPOIS)
# # LC = (Ativo circulante (BP) - Estoques (BP)) / Passivo circulante (BP)
# # LC >= 1 => empresa capaz de liquidar as suas operações de curto prazo
# # LC < 1 => empresa incapaz de liquidar as suas operações de curto prazo
# current_liquidity <- function(
#   assets #Ativo (BPA)
#   ,liabilities #Passivo (BPP)
# ){
#   
#   current_liquidity <- assets/liabilities
#   return(current_liquidity)
#   
# }
# 
# tibble(
#   ativos = c(120000000,130000000,100000000)
#   ,passivos = c(80000000,70000000,75000000)
# ) %>% mutate(
#   LC = current_liquidity(ativos, passivos)
#   ,DESC_LC = case_when(
#     LC >= 1 ~ 'Empresa líquida'
#     ,LC < 1 ~ 'Empresa não líquida'
#   )
# )



# liquidez <- function(
#   BP_ativo
#   ,BP_passivo
#   ,ativo_circ = '1_01'
#   # ,estoques = '1_02_01_05'
#   ,estoques = 'estoques'
#   ,passivo_circ = '2_01'
# ){
#   
#   tibble(
#     LIQUIDEZ_CORRENTE = BP_ativo[ativo_circ]/BP_passivo[passivo_circ]
#     ,LIQUIDEZ_SECA = (BP_ativo[ativo_circ] - BP_ativo[estoques])/BP_passivo[passivo_circ]
#   ) %>% return(.)
#   
# }
# 
# liquidez(
#   teste$`DF Consolidado - Balanço Patrimonial Ativo`,
#   teste$`DF Consolidado - Balanço Patrimonial Passivo`
# )
# [EXEMPLO] MARGENS -----------------------------------------------------------------
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






# [EXEMPLO] MODELOS ------------------------------------------------------------------



# [MÉTODO INTELIGENTE] RATIOS ---------------------------------------------
# PE RATIO: num = Preço da ação, div = lucro por ação

# Empresa A
tibble(
  # Ações
  price = c(10, 12, 15, 13, 14, 16, 18)
  ,dividend = c(1.5, 2, .93, 1, 1.2, 2.2, 1.7)
  ,n_stocks = c(1000000, 1000500, 1200000, 15000000, 2000000, 2100000, 24000000)
  
  # BPA
  ,ativos = c(10000000, 12000000, 15000000, 20000000, 30000000, 25000000, 33000000)
  ,cash = c(10000000*.25, 12000000*.22, 15000000*.20, 20000000*.28, 30000000*.3, 25000000*.18, 33000000*.15)
  
  # BPP
  ,passivos = c(10000000*1.05, 12000000*1.025, 15000000*1, 20000000*.95, 30000000*.89, 25000000*.84, 33000000*.77)
  ,debt = c(10000000*1.05*.35, 12000000*1.025*.55, 15000000*1*.49, 20000000*.95*.37, 30000000*.89*.23, 25000000*.84*.2, 33000000*.77*.15)
  
  # DRE
  # ,EBIT = 
  # ,EBITDA 
  
) %>% mutate(
  # net_profit.per_share = 
) -> LALALA


# Lista de numeradores para os diferentes ratios (indicadores, margens etc)
numeradores <- list(
  'PE' = c(1,2)
  , 'P_VPA' = c(2,2)
  , 'ev_ebitda' = c(3,1)
  , 'MO' = c(4,5)
)

# Lista de denominadores para os diferentes ratios (indicadores, margens etc)
divisores <- list(
  'PE' = c(12,2)
  , 'P_VPA' = c(2,2)
  , 'ev_ebitda' = c(3,1)
  , 'MO' = c(4,5)
)


# mapply para calcular todas as ratios num.i/div.i para todo i nas listas
Map(
  num = numeradores,
  div = divisores,
  function(num,div){num/div}
) %>% bind_rows(.)

mratios <- function(
  numeradores
  , divisores
){
  Map(
    num = numeradores,
    div = divisores,
    function(num,div){num/div}
  ) %>% bind_rows(.) %>% return(.)
}


LALALA %>% 
  select(price, dividend) -> lalala

mratios(list('a' = 1,'v' = 1,'c' = 1),list('a' = 2,'v' = 2,'c' = 2))
