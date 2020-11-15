library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(odbc)
library(sidrar)
library(viridis)
library(hrbrthemes)
library(survival)
library(survminer)


setwd("~/Área de Trabalho/UFT/Boletim - PET/Dados Agricultura")

#Produ??o - Janeiro
Dados <- get_sidra(x = 6588,
                   variable = 35,
                   period = "202001",
                   geo = "State",
                   header = TRUE,
                   format = 1)

Dados_TO <- Dados %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Produ??o - Fevereiro

Dados2 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202002",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO2 <- Dados2 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

#Produ??o - Mar?o

Dados3 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202003",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO3 <- Dados3 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

#Produ??o - Abril

Dados4 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202004",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO4 <- Dados4 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Produ??o - Maio

Dados5 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202005",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO5 <- Dados5 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 

#Produ??o - Junho

Dados6 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202006",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO6 <- Dados6 %>% 
  filter(`Unidade da Federa??o (C?digo)` == 17) %>% 
  filter(`Produto das lavouras (C?digo)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Produ??o - Julho

Dados7 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202007",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO7 <- Dados7 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Produção - Agosto

Dados8 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202008",
                    geo = "State",
                    header = TRUE,
                    format = 1)

Dados_TO8 <- Dados8 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 


#Produção - Setembro


Dados9 <- get_sidra(x = 6588,
                    variable = 35,
                    period = "202009",
                    geo = "State",
                    header = TRUE,
                    format = 1)


Dados_TO9 <- Dados9 %>% 
  filter(`Unidade da Federação (Código)` == 17) %>% 
  filter(`Produto das lavouras (Código)` %in% c(39428, 39432, 39441, 39442, 39443, 39456, 39467)) 





Total <-  data.frame("Meses" = 1:1, "Preços" = c(15, 15, 15, 14, 13, 15, 14, 14, 14, 16, 16, 16, 16, 41, 41), "tempo" = c("5/11", "14/11", "17/11", "29/11", "5/12", "10/12", "18/12", "2/1", "7/1", "31/1", "3/2", "11/2", "27/2", "4/3", "11/3"))

Total %>%
  mutate(tempo = factor(tempo, levels=c("5/11", "14/11", "17/11", "29/11", "5/12", "10/12", "18/12", "2/1", "7/1", "31/1", "3/2", "11/2", "27/2", "4/3", "11/3"))) %>%
  ggplot(aes(x=tempo, y = Preços)) +
  geom_bar(stat='identity', width=0.5, fill = "#4682B4") + 
  theme_classic() + 
  geom_text(aes(label=Preços), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_y_continuous(limits = c(0, 60)) + 
  labs(title = "Avaliação de preço do álcool em gel 500ml", y= "Preços", x= " ", caption = "Fonte: JáCotei \n Elaboração própria") + 
  theme(plot.title = element_text(hjust = 0.5, size = 13))



plot = ggplot(Total, aes(x=Produtos, y=Producao, fill=Produtos))

plot + geom_area(stat = "identity", position = "stack")




#Dados Google Trends


data_gtrends = gtrends(keyword = c("Álcool em gel"),
                       geo = "BR", time='2019-11-5 2020-11-05', onlyInterest=TRUE)

google_trends %>% 
ggplot(aes(x = Semana, y = `Álcool em gel 500ml: (Brasil)`, group = 1))  +
  geom_line(size = 0.9, colour = "#4682B4")  +  
  theme_classic() + 
  labs(title="Procura por álcool em gel 500ml usando o Google Trends", x= " " , y= "Hits",  caption = "Fonte: Google Trends \n Elaboração própria") + 
  theme(plot.title = element_text(hjust = 0.5, size = 13))

seguro_desemprego = data_gtrends$interest_over_time %>%
  filter(keyword == "Álcool em gel") %>%
  mutate(mes = floor_date(date, "month"))



google_trends <- read_csv("multiTimeline.csv")
