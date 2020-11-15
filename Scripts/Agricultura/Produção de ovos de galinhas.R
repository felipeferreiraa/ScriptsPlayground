library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(sidrar)

#Diret?rio 
setwd("C:/Users/ferre/OneDrive/?rea de Trabalho/UFT/Boletim - PET/Dados Agricultura")


#Gr?ficos sobre Ovos - Total.

Total <-  data.frame("Per?odo" = 1:3, "Número_Galinhas_poedeiras" = c(1512.339,1559.960,1361.992), "Meses" = c("3T - 2019", "4T - 2019", "1T - 2020"))

Total %>%
  mutate(Meses = factor(Meses, levels=c("3T - 2019", "4T - 2019", "1T - 2020"))) %>%
  ggplot(aes(x=Meses, y = Número_Galinhas_poedeiras, group = 1)) + geom_bar(stat = "identity", width = 0.3, fill = "#4682B4")+
  theme_classic()   + 
  scale_y_continuous(limits = c(0, 2000), breaks = seq(from = 0, to = 2000, by = 200)) + 
  labs(title="Total de galinhas poedeiras", x= " ", y= "N?mero de galinhas poedeiras por mil",  caption = "Fonte: IBGE \n Elaborado por: PET Economia") + theme(plot.title = element_text(hjust = 0.5, size = 13))

#Quantidade

Quantidade <-  data.frame("Per?odo" = 1:3, "Quantidade_de_Ovos" = c(9726,9074,7088), "Meses" = c("3T - 2019", "4T - 2019", "1T - 2020"))

Quantidade %>%
  mutate(Meses = factor(Meses, levels=c("3T - 2019", "4T - 2019", "1T - 2020"))) %>%
  ggplot(aes(x=Meses, y = Quantidade_de_Ovos, group = 1)) + geom_bar(stat = "identity", width = 0.3, fill = "#4682B4")+
  theme_classic()   + 
  scale_y_continuous(limits = c(0, 12000), breaks = seq(from = 0, to = 12000, by = 1200)) + 
  labs(title="Quantidade produzida de ovos de galinhas", x= "Per?odo", y= "Produ??o de ovos de galinha (Mil d?zias)",  caption = "Fonte: IBGE \n Elaborado por: PET Economia") + theme(plot.title = element_text(hjust = 0.5, size = 13)) + 
  geom_text(aes(label=Quantidade_de_Ovos), position=position_dodge(width=0.9), vjust=-0.25)
