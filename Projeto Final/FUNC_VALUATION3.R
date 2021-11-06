# PACOTES -----------------------------------------------------------------


# VARIÁVEIS PARA RATIOS ------------------------------------------------------------------

# 2. MÚLTIPLOS DE MERCADO 
# 2.1 Preço por lucro por ação
lucro.liquido <- teste$`DF Consolidado - Demonstração do Resultado`$`3.11_lucro/prejuízo consolidado do período`
preço.ficticio <- rnorm(n = length(lucro.liquido), mean = 25, sd = 5) %>% round(4) 
n.ficticio <- seq(10000000, 15000000, (15000000 - 10000000)/length(lucro.liquido))[-1]  

PE.ratio <- preço.ficticio/(lucro.liquido/n.ficticio)

# 2.2. Múltiplo do Valor patrimonial
# P/VPA = Preço da ação/patrimônio líquido por ação
# VPA = Patrimônio Líquido / Número de ações no mercado
# => P/VPA = Preço da ação/(Patrimônio Líquido / Número de ações no mercado)
# Valores importantes:
# P/VPA > 1 => mercado valoriza a empresa além da equivalência patrimonial (i.e. acrescenta um bônus ao preço)
# P/VPA < 1 => mercado valoriza a empresa abaixo da equivalência patrimonial (i.e. acrescenta uma penalidade ao preço)
# P/VPA = 1 => mercado valoriza a empresa exatamente na equivalência patrimonial (i.e. sem bönus nem penalidade ao preço)
PL <- teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2.03_patrimônio líquido consolidado`

p_vpa <- preço.ficticio/(PL/n.ficticio)

# 2.3. DIVIDEND YIELD
# Dividend yield = Dividendo pago por ação / Preço por unidade de ação
DPS.ficticio <- rnorm(n = length(lucro.liquido), mean = 2.5, sd = 0.7) %>% round(4) 

dividend.yield <- DPS.ficticio/preço.ficticio

# 2.4. ENTERPRISE VALUE POR EBITDA
# EV/EBITDA = (Enteprise Value/Earnings Before Taxes Interest Depreciation and Amortization).
# EV = valor de mercado da empresa = preço da ação * número de ações (i.e. o quanto o consenso do mercado diz que a empresa vale) + DÍVIDAS (BP) - CAIXA (BP)
# EBITDA = lucratividade da empresa (desconsiderando variáveis que não têm a ver com a sua operação)
# Quanto menor o EV/EBITDA, melhor (comparar setorialmente apenas)
divida <- teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2.01.04_empréstimos e financiamentos` + teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2.02.01_empréstimos e financiamentos`
caixa <- teste$`DF Consolidado - Balanço Patrimonial Ativo`$`1.01.01_caixa e equivalentes de caixa`

# ROE (Return on Equity) = capacidade da empresa de agregar valor (gerar retorno) para o capital próprio investido
# ROE = LUCRO LÍQUIDO (DRE) / PATRIMONIO LIQUIDO (BP) = LL/PL
# Valores importantes:
# ROE > 10% = "boa empresa" 
# ROE > 15% = "empresa muito boa" 
# E assim por diante (comparar setorialmente)

ROE <- lucro.liquido/PL

# 3.2. ROIC (Return on Invested Capital) = medida de desempenho financeiro da empresa (considerando todo o capital investido, incluindo o de terceiros)
# ROIC = NOPLAT / Valor Contábil do Capital Investido.
# NOPLAT: sigla que vem do inglês "Net Operating Profit Less Adjusted Taxes". 
# Ou seja, ele representa o lucro operacional (ou EBIT) menos os impostos.
# Capital investido: capital total alocado pela empresa, ou seja, a soma do capital dos acionistas com o capital de terceiros.
EBIT <- teste$`DF Consolidado - Demonstração do Resultado`$`3.05_resultado antes do resultado financeiro e dos tributos`
NOPLAT <- EBIT - teste$`DF Consolidado - Demonstração do Resultado`$`3.08_imposto de renda e contribuição social sobre o lucro`
dividas.curto.prazo <- teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2.03.01_capital social realizado`
dividas.longo.prazo <- teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2.02.01_empréstimos e financiamentos` + teste$`DF Consolidado - Balanço Patrimonial Passivo`$`2.01.04_empréstimos e financiamentos`
capital.investido <- PL + dividas.curto.prazo + dividas.longo.prazo

ROIC <- NOPLAT/capital.investido

# OUTRAS FÓRMULAS ---------------------------------------------------------
# EV
enterprise.value <- function(
  price # Preço da ação
  ,n.stocks # Número de ações
  ,debt # Divida Total (BP, Passivo)
  ,cash # Caixa Total (BP, Ativo)
){
  
  enterprise.value <- price*n.stocks + cash - abs(debt)
  return(enterprise.value)
  
}

enterprise.value(price = preço.ficticio, n.stocks = n.ficticio, debt = divida, cash = caixa) 
# Não sei se isso é depreciação e amortização, mas é o que tem
EBITDA <- EBIT + teste$`DF Consolidado - Demonstração do Resultado`$`3.04.03_perdas pela não recuperabilidade de ativos`

# 4.1. Dívida Líquida
# DL = DIVIDA TOTAL - CAIXA (BP)

net_debt <- function(
  total_debt # Dívida Total (BPP)
  ,cash # Caixa (BPA)
  ,return_abs_value_debt = T
){
  
  net_debt <- abs(total_debt) - cash
  
  if(return_abs_value_debt){
    net_debt <- (-net_debt) # Negativo porque é passivo (dívida)
  }
  
  return(net_debt)
  
}

divida_liquida <- net_debt(total_debt = divida, cash = caixa)



# CÁLCULOS ----------------------------------------------------------------
demonstrativos.consolidados <- function(
  DRE
  ,DFC
  ,BPP
  ,BPA
  ,prices = NA
  ,dividends = NA 
  ,number.stocks = NA
  # Depois fazer uma versão que raspa o preços, dividendos e número de ações (1 por ano)
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
    group_by(CNPJ_CIA, CD_CVM, DENOM_CIA, DT_INI_EXERC, DT_FIM_EXERC) %>%
    transmute(
      # Argumentos
      prices = prices
      ,dividends = dividends
      ,number.stocks = number.stocks
      # DRE
      ,receita.liquida = `3.01_receita de venda de bens e/ou serviços` #Acredito que é esta receita que deve-se utilizar, mas talvez esteja enganado
      ,lucro.bruto = `3.03_resultado bruto`
      ,lucro.liquido = `3.11_lucro/prejuízo consolidado do período`
      ,EBIT = `3.05_resultado antes do resultado financeiro e dos tributos`
      ,TAX = `3.08_imposto de renda e contribuição social sobre o lucro`
      ,DA = rowSums(across(contains('deprecia')), na.rm = T)
      # ,DA = `3.04.03_perdas pela não recuperabilidade de ativos` #Não sei se é isso as depreciações e amortizações, mas é o mais próximo que achei
      # ,DA = `3.04.02.04_depreciação e amortização`
      # BPP
      ,passivo.circulante = `2.01_passivo circulante`
      ,PL = `2.03_patrimônio líquido consolidado`
      ,divida = `2.01.04_empréstimos e financiamentos` + `2.02.01_empréstimos e financiamentos`
      # ,dividas.curto.prazo = `2.03.01_capital social realizado`
      # ,dividas.longo.prazo = `2.02.01_empréstimos e financiamentos` + `2.01.04_empréstimos e financiamentos`
      # BPA
      ,caixa = `1.01.01_caixa e equivalentes de caixa`
      ,ativo.circulante = `1.01_ativo circulante`
      ,estoques = `1.01.04_estoques`
      # Variáveis derivadas
      ,EBITDA = EBIT + DA
      ,NOPLAT = EBIT - TAX
      # ,capital.investido = PL + dividas.curto.prazo + dividas.longo.prazo
      ,capital.investido = PL + divida
      ,divida.liquida = abs(divida) - caixa
      ,EV = prices*number.stocks + caixa - abs(divida)
    ) %>% return(.)

}

fundamentos <- function(
  demonstrativos_consolidados
){
  
  demonstrativos_consolidados %>%
    group_by(CNPJ_CIA, CD_CVM, DENOM_CIA, DT_INI_EXERC, DT_FIM_EXERC) %>%
    transmute(
      # MÚLTIPLOS DE MERCADO
      PE_ratio = prices/(lucro.liquido/number.stocks)
      ,P_VPA = prices/(PL/number.stocks)
      ,dividend.yield = dividends/prices
      ,EV_EBITDA = EV/EBITDA
      
      # INDICADORES FUNDAMENTALISTAS
      ,ROE = lucro.liquido/PL
      ,ROIC = NOPLAT/capital.investido
      
      # DÍVIDA E LIQUIDEZ
      ,divida.liquida = ifelse(abs(divida) - caixa > 0, yes = abs(divida) - caixa, no = 0)
      ,divida.liquida_PL = ifelse(divida.liquida > 0, yes = divida.liquida/PL, no = 0)
      ,liquidez_corrente = ativo.circulante/passivo.circulante
      ,liquidez_seca = (ativo.circulante - estoques)/passivo.circulante
      
      # MARGENS
      ,margem_bruta = lucro.bruto/receita.liquida
      ,margem_ebitda = EBITDA/receita.liquida
      ,margem_operac = EBIT/receita.liquida
      ,margem_liquida = lucro.liquido/receita.liquida
      
    ) %>% return(.)
  
}

teste$`DF Consolidado - Demonstração do Resultado` %>% glimpse(.)

demonstrativos.consolidados(
  DRE = teste$`DF Consolidado - Demonstração do Resultado`
  ,DFC = teste$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)`
  ,BPP = teste$`DF Consolidado - Balanço Patrimonial Passivo`
  ,BPA = teste$`DF Consolidado - Balanço Patrimonial Ativo`
) -> lalala

list(
  'DRE' = teste$`DF Consolidado - Demonstração do Resultado`
  ,'BPP' = teste$`DF Consolidado - Balanço Patrimonial Passivo`
  ,'BPA' = teste$`DF Consolidado - Balanço Patrimonial Ativo`
  ,'DFC' = teste$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)`
  , 'ALL' = lalala
) %>% lapply(nrow)

demonstrativos.consolidados(
  DRE = teste$`DF Consolidado - Demonstração do Resultado`
  ,DFC = teste$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)`
  ,BPP = teste$`DF Consolidado - Balanço Patrimonial Passivo`
  ,BPA = teste$`DF Consolidado - Balanço Patrimonial Ativo`
) %>% 
  ggplot(
    aes(x = DT_FIM_EXERC
        ,y = EBITDA)
  ) + 
  geom_line(size = 1.2) + 
  facet_grid(cols = vars(DENOM_CIA)) + 
  ggthemes::theme_economist()

demonstrativos.consolidados(
  DRE = teste$`DF Consolidado - Demonstração do Resultado`
  ,DFC = teste$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)`
  ,BPP = teste$`DF Consolidado - Balanço Patrimonial Passivo`
  ,BPA = teste$`DF Consolidado - Balanço Patrimonial Ativo`
) %>% fundamentos(.) %>% View(.) 
  ggplot(
    aes(x = DT_FIM_EXERC
        ,y = margem_operac)
  ) + 
  geom_bar(stat = 'identity') + 
  # geom_line(size = 1.2) + 
  facet_grid(cols = vars(DENOM_CIA)) + 
  ggthemes::theme_economist()


# PROBLEMA: MÚLTIPLOS NOMES DE DEPRECIAÇÃO E AMORTIZAÇÃO POR FIRMA
# SOLUÇÃO:
# 1. QUANDO OS NOMES NÃO FOREM IDÊNTICOS, AS VARIÁVEIS SEPARADAS SÃO PREENCHIDAS COM NA
# 2. MAS CADA LINHA É ÚNICA (I.E. SÓ EXISTE UM VALOR =/= NA POR LINHA, I.E. POR FIRMA)
# 3. OU SEJA, BASTA SOMAR TOMAS AS VARIÁVEIS QUE CONTÉM A PALAVRA DEPRECIAÇÃO, INDEPENDENTEMENTE DAS PEQUENAS VARIAÇÕES NA NOMENCLATURA
# teste$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)` %>%
#   select(contains('deprecia')) %>% 
#   mutate_if(is.numeric, ~ replace(., is.na(.), 0)) %>% 
#   mutate_if(is.numeric, ~ replace(., . > 0, 1)) %>%
#   transmute(sum = rowSums(across(where(is.numeric))))


