library(PNADcIBGE)
library(survey)
library(convey)
library(tidyverse)


#Dados da PNAD Continua
renda2018 <- get_pnadc(year = 2018, interview = 1, vars=c("UF", "VD4020", "VD5002"))
head(renda2018)
renda2018 <- convey_prep(renda2018)



#Survey
#Tocantins
total_to <- svytotal(~UF == "Tocantins", renda2018, na.rm = T)
total_to


total_pobTO <- svytotal(~interaction(UF == "Tocantins", VD5002 <= 411), renda2018, na.rm = T)
total_pobTO


#Brasil

total_br <- svytotal(~UF, renda2018, na.rm = T)
total_br
sum(total_br)

total_pobBR <- svytotal(~VD5002 <= 410.40, renda2018, na.rm = T)
total_pobBR
sum(total_pobBR)
#Dataframe de conjuntura Mensal

DFConj <- get_pnadc(year = 2019, quarter = 4, vars=c("UF", "V2001", "V2007", "V2010", "V4001", "V4039", "V4071", "V4073", "VD4003", "VD4004", "VD4005", "VD4020"))

#Dataframe de conjuntura Anual

DfConj_anual <- get_pnadc(year = 2018, interview = 1, vars=c("UF", "VD4020", "VD5002"))
DfConj_anual
#Pobreza para dados trimestrais

#Renda

renda <- (DfConj_anual$variables) %>%
  select()

#pob



#Amostra1
amostra1 <- (dadosPNADc_anual$variables) %>%
  filter(UF == "Tocantins") %>%
  select(UF, VD5002, VD2002, VD4008, VD4020) %>%
  filter(VD5002 <= 410.85) %>%
  na.omit()


#amostra3

amostra3 <- (Amostra) %>%
  filter(UF == "Tocantins")


#Salvar RDS
saveRDS(amostra3, "Amostra3.rds")

#Leitura RDS
Amostra <- readRDS("Amostra.rds")


#amostrinha1

amostra1 <- (a$variables)



#pobBR
pob <- (Amostra$variables) %>%
  select(VD5002) %>%
  filter(VD5002 <= 410.85)
  



#Pobreza
pob <- (Amostra) %>%
  filter(UF == "Tocantins") %>%
  select(VD5002) %>%
  filter(VD5002 <= 410.85) %>%
  na.omit()



#Riqueza do Jean

jian <- (DfConj_anual$variables) %>%
  filter(UF == "Tocantins") %>%
  select(UF, VD5002) %>%
  na.omit() %>%
  filter(VD5002 >= 447)

pobres <- dim(pob) / dim(Amostra)
gang <- dim(amostra)
summary(gang)
