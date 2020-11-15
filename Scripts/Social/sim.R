library(survey)
library(PNADcIBGE)
library(tidyverse)
library(convey)

#Importando os dados
ren_2018 <- get_pnadc(year = 2018, interview = 1, vars=c("UF", "V2007", "VD4020", "VD5002",defyear = 2018, deflator = TRUE))
ren_20181 <- convey_prep(ren_2018)

ren_2018DF <- ren_2018$variables

ren_2017 <- get_pnadc(year = 2017, interview = 1, vars=c("UF", "V2007", "VD4020", "VD5002", defyear = 2017, deflator = TRUE))

#Deflacionando a preços médios do próprio ano
ren_def_2018 <- (ren_2018DF) %>%
  mutate(VD5002_real = VD5002 * 1.002650476)

ren_2018[["variables"]] <- ren_def_2018

ren_20181 <- convey_prep(ren_2018)

#Gini do Brasil
giniBR2018 <- svygini(~VD5002_real, ren_20181, na.rm  =  TRUE)

#Pobreza no Tocantins
totalTO <- svytotal(~UF == "Tocantins", ren_2018, na.rm = T)
totalTO2017 <- svytotal(~UF == "Tocantins", ren_2017, na.rm = T)

totalpobTO <- svytotal(~ interaction(UF == "Tocantins", VD5002_real < 415), ren_2018, na.rm = T)

#Pobreza Brasil
totalBR <- svytotal(~V2007, ren_2018, na.rm = T)

totalpobBR <- svytotal(~VD5002_real < 411.31, ren_2018, na.rm = T)


