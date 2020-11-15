library(readxl)
library(tidyverse)
library(scales)
library(ggplot2)
library(dplyr)



#Dados Nacionais

coronguinha = read_xlsx("coronguinha_de_leve.xlsx")
coronguinha1 <- coronguinha %>%
  filter(regiao == "Brasil")

ggp <- ggplot(coronguinha1, aes(x=data, y=casosAcumulado), scale_y_continuous(labels = casosAcumulado)) +geom_line() +  geom_point()
ggp

ggp + 
  scale_y_continuous(labels = comma)
  
#Dados Estaduais
coronguinha2 <- coronguinha %>%
  filter(estado == "TO")

aaa <- coronguinha2 %>% slice(1:117)

ggplot(aaa, aes(x=data, y=casosAcumulado)) +geom_line() + geom_point() + labs(title = ) + labs(title="Casos acumulados de Covid - TO. At? o dia 20/06/20\n", y= "Casos Acumulados", x= "Datas")

#Curvas de estados do Norte
coronguinha3 <- coronguinha %>%
  filter(regiao == "Norte") %>% 
  slice(1:805) %>% 
  group_by(estado)
grafnorte <- ggplot(coronguinha3, aes(x=data, y=casosAcumulado, color=estado)) +
  geom_line(size = 1.5)
grafnorte  

#Criando a coluna "Casos por 100.000 hab. para região Norte"

coronguinha3["casos3_p_100000"] <- as.numeric(coronguinha3$casosAcumulado)/as.numeric(coronguinha3$populacaoTCU2019)
grafnorte <- ggplot(coronguinha3, aes(x=data, y=casos3_p_100000, color=estado)) +

    geom_line(size = 1.5)
grafnorte
