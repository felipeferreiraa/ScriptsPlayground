library(RCurl)
library(tidyverse)
library(survey)
library(convey)
library(PNADcIBGE)
library(ecoseries)
library(ipeadatar)
library(devtools)
library(RCurl)
library(readr)
library(timeDate)
library(ggplot2)
library(forecast)
library(tseries) #Manipular ST (Trapletti and Hornik, 2017)
library(TSA) #Manipular ST (Chan and Ripley, 2012)
library(lmtest) 
library(data.table)

#Dados Maranhão - Teste

#Dados
dados <- read.table("https://raw.githubusercontent.com/icaroagostino/ARIMA/master/dados/MA.txt", header=T)
attach(dados) #tranformando em objeto
MA <- ts(MA, start = 2007, frequency = 12) #tranformando em Séries Temporal

#Visualização
ggtsdisplay(MA, main="Saldo de emprego - MA")

#Ajuste do Modelo
ARIMA_fit <- auto.arima(MA)
ARIMA_fit #sai o modelo ajustado
coeftest(ARIMA_fit)
#Verificação dos résiduos

checkresiduals(ARIMA_fit)

#Previsão

autoplot(forecast(ARIMA_fit, h = 12))


#Taxa de Desemprego

#Dados

dados <- read.table("https://raw.githubusercontent.com/felipeferreiraa/c-digos/master/TO.txt", header=T)


data<-ipeadata(c("PNADC_TXDES_UF"))

data <- data.table(periodo = c("2012-01-01", "2012-04-01", "2012-07-01", 
                             "2012-10-01", "2013-01-01", "2013-04-01",
                             "2013-07-01", "2013-10-01", "2014-01-01",
                             "2014-04-01", "2014-07-01", "2014-10-01",
                             "2015-01-01", "2015-04-01", "2015-07-01",
                             "2015-10-01", "2016-01-01", "2016-04-01",
                             "2016-07-01", "2016-10-01", "2017-01-01",
                             "2017-04-01", "2017-07-01", "2017-10-01",
                             "2018-01-01", "2018-04-01", "2018-07-01",
                             "2018-10-01", "2019-01-01", "2019-04-01",
                             "2019-07-01", "2019-10-01", "2020-01-01"),
                   Toca = c(8.4, 7.7, 7.2, 7.6, 9.3, 8.2, 6.2, 6.4, 8.5,
                          7.7, 7.5, 6.3, 8.7, 7.6, 9.2, 9.0, 10.7, 11.2,
                          10.8, 13.1, 12.6, 11.7, 11.8, 10.5, 11.0, 11.3,
                          9.8, 10.4, 12.3, 11.4, 10.5, 9.1, 11.2))
                                                

dados_to  <- data %>% 
  filter(tcode == 17)
attach(dados)

TO <- ts(TO, start = 2012, frequency = 4) #tranformando em Séries Temporal

#Visualização
ggtsdisplay(TO, main="Saldo de emprego - TO")

#modelo

model <- auto.arima(TO)
model
#Ajuste do Modelo
ARIMA_fit <- auto.arima(TO)
ARIMA_fit #sai o modelo ajustado
coeftest(model)

autoplot(forecast(ARIMA_fit, h = 4))



#Modelo de saldos de empregos toca



