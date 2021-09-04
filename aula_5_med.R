
# fontes
# https://rpubs.com/amrofi/decompose_x11_varejoms
# https://rpubs.com/joshyazman/yougov-decomposition-predict413-fall2017
# https://rpubs.com/Possato/ARIMA_ajuste_tutorial
# https://www.r-bloggers.com/2020/05/basic-linear-regressions-for-finance/

#install.packages("quantmod")
require(quantmod)
library(lubridate)

## Banco Bradesco S.A.
getSymbols('BBDC4.SA',src='yahoo', from = Sys.Date() - years(1), to = Sys.Date())

chartSeries(BBDC4.SA)

## Itaúsa - Investimentos Itaú S.A.
getSymbols('ITUB4.SA',src='yahoo', from = Sys.Date() - years(1), to = Sys.Date())


chartSeries(ITUB4.SA)

## podemos também observar os gráficos das duas ao mesmo tempo
chartSeries(c(BBDC4.SA, ITUB4.SA))


### https://en.wikipedia.org/wiki/Bollinger_Bands
# Bollinger Bands consist of:
# a. an N-period moving average (MA)
# b. an upper band at K times an N-period standard deviation above the moving average (MA + Kσ)
# c. a lower band at K times an N-period standard deviation below the moving average (MA − Kσ)
# Typical values for N and K are 20 and 2, respectively.


###  Procura usar uma ideia clássica e sólida. A ideia de que os 
# preços não se afastam por muito tempo de uma média de valores,
#procurando retornar a essa média de tempos em tempos. Logo, 
#a banda é construída projetando uma quantidade de desvios 
#padrões acima e abaixo desta média.
chartSeries(BBDC4.SA, subset = 'last 12 months', theme="white",TA="addVo();addBBands(20,2);addCCI()") 


chartSeries(ITUB4.SA, subset = 'last 12 months', theme="white",TA="addVo();addBBands();addCCI()") 


# detonar os NA

ITUB4.SA<-ITUB4.SA[apply(ITUB4.SA,1,function(x) all(!is.na(x))),]


# Analisar a composição de uma ação pelo pacote dygraph
library(dygraphs)



dygraph(ITUB4.SA$ITUB4.SA.Close)

ITAU <- ts(ITUB4.SA$ITUB4.SA.Close, frequency = 7)


decomposicao.ad <- decompose(ITAU)  # aditiva
decomposicao.mult <- decompose(ITAU, type = "multiplicative")  # multiplicativa

# grafico da decomposicao aditiva
plot(decomposicao.ad)



# grafico da decomposicao multiplicativa
plot(decomposicao.mult)


# obtendo a tendencia
install.packages("fpp2")
library(fpp2)
# ma computes a simple moving average smoother of a given time series.
# ordem 12
tend <- ma(ITAU, 12)
print(cbind(tend, decomposicao.ad$trend, decomposicao.mult$trend))

# plot conjunto com função autoplot do fpp2
autoplot(ITAU, series = "Dados") + autolayer(tendencia, series = "tend") + xlab("Ano") + 
  ylab("Índice") + ggtitle("Itau") + 
  scale_colour_manual(values = c(Dados = "grey50", tend = "red"), breaks = c("Dados", 
                                                                             "tend"))

# ARIMA
# Método estatístico que utiliza autoregressão e médias móveis
# para previsão de séries temporais. Um modelo linear é construído 
# incluindo um número especificado de termos e os dados 
# são preparados por um nível de diferenciação afim de 
# tornar este estacionário.
# 
# Podemos usar um valor 0 para desligar um parâmetro, dessa
# forma, aquela função em questão não será feita, por exemplo,
# se no parâmetro d definirmos 0 não será realizada uma 
# diferenciação nos dados. Neste exemplo teríamos um modelo ARMA.
# 
# AR: Autoregression : Um modelo que usa a relação dependende 
# entre uma observação e alguns lags.
# 
# I: Integrated : Uso de diferenciação nas observações brutas,
#exemplo: subtração do valor de uma observação com sua 
#observação anterior. O objetivo é transformar a série 
# temporal em estacionária.
# 
# MA: Moving Average : Um modelo que usa a dependência 
# entre a observação e o erro residual a partir de um modelo 
# de média móvel aplicado a lags.
# 
# residual error = expected − predicted
# Isso é diferente de Moving Average Smoothing.
# 
# Erros residuais contém estruturas temporais que podem 
# ser modeladas.
# Existem sinais complexos nos erros residuais.
# Um modelo que preve o erro residual pode ser usado 
# para ajustar os próximos erros e melhorar um modelo 
# que aprende com o histórico.
# O modelo arima contém alguns parâmetros
# 
# # ARIMA(p,d,q)
# p: O número de lags que foram devem ser incluídos no modelo.
# d: O número de vezes que as observações serão diferenciadas.
# q: O tamanho de uma janela de média móvel. Também chamada de ordem de média móvel.


# Para o ajuste do ARIMA em ambiente R, pode-se recorrer ao
# pacote forecast.

library(forecast)


plot.ts(ITAU) # Série temporal configurada anteriormente

# Para a verificação das funções de autocorrelação e autocorrelação parcial, pode-se proceder da seguinte forma:
  
ggtsdisplay(ITAU)

# A partir dos gráficos:
#   
#   ACF - função de autocorrelação
# PACF - função de autocorrelação parcial
# Para a determinação da ordem de autoregressão (p), 
# do grau de diferenciação (d) e da componente de médias móveis
# (q), deve-se realizar a observação do comportamento dos “lags” 
# manifestados por cada gráfico de autocorrelação (ACF e ÁCF).
# “Nos gráficos gerados, as linhas tracejadas azuis são os 
# limites de significância ou intervalos de confiança; 
# sempre que houver uma ultrapassagem, diz-se que há, 
# ali, um lag com significância”
# 
# A determinação dos valores dos termos pode não ser exata, uma vez que o 
# autor da análise pode incorrer em diversos erros 
# e gerar diversos tipos de modelos. 
# Devido a isso, é necessária a utilizaçaõ de uma métrica 
# com o o intuito de se aferir a qualidade do modelo gerado.
# 
# entendendo essa dificuldade, dentre as ferramentas 
# disponibilizadas pelo pacote forecast(), está a função auto.arima().
# 
# AUTO-ARIMA
# essa função realiza a verificação dos possíveis modelos
# gerados a partir da série temporal em questão, 
# visando ao ajuste ideal.

## Itaúsa - Investimentos Itaú S.A.
getSymbols('ITUB4.SA',src='yahoo', from = Sys.Date() - years(2), to = Sys.Date()- years(1))

# retirar o NA por precaucao
ITUB4.SA<-ITUB4.SA[apply(ITUB4.SA,1,function(x) all(!is.na(x))),]

# amostra treino
modelo_treino <- ts(ITUB4.SA$ITUB4.SA.Close) 


getSymbols('ITUB4.SA',src='yahoo', from = Sys.Date()- years(1), to = Sys.Date())

# retirar o NA por precaucao
ITUB4.SA<-ITUB4.SA[apply(ITUB4.SA,1,function(x) all(!is.na(x))),]

# Amostra para validação
modelo_teste <- ts(ITUB4.SA$ITUB4.SA.Close) 

# auto arima. Tente o arima.
modelo <- auto.arima(modelo_treino, stepwise = FALSE) # Criação do modelo
# approximation = FALSE testei e ficou pior


modelo


checkresiduals(modelo)


accuracy(modelo$fitted, modelo_treino)

# Os termos resultantes da acurácia acima são os seguintes:
  
# RMSE: Erro quadrático médio
# MAE: Erro médio absoluto
# MAPE: Erro percentual médio
# Agora, se realizará a previsão para os próximos 15 períodos, o que permitirá a visualização da acurácia do modelo para a realização de previsões.

previsao <- forecast(modelo, h = 15)

head(previsao$lower) # Limite inferior

head(previsao$upper) # Limite superior

## gráfico
autoplot(previsao, predict.colour = "red") + labs(x = "Tempo", y = "ITAU", 
                                                  title = "Previsao usando o forecast") +
      theme_test() + geom_vline(xintercept = 248, lty = "dashed")



accuracy(modelo_teste, as.numeric(previsao$mean))

# https://colab.research.google.com/drive/1VmWPXdHBC-exe9wUcEsg_Ld3g-j15x4I?usp=sharing
# https://docs.google.com/document/d/1-sMFfHDfLSwEUVWpbifzEGgxIi7H4qJNF2oY3K9hEnc/edit?usp=sharing
  