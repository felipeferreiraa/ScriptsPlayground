library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)

#Diretório

setwd("~/Área de Trabalho/UFT/Pesquisa_PET/Dados POF/Dados")


#Microdados Despesa_Individual
DESPESA_INDIVIDUAL <- 
  read.fwf("DESPESA_INDIVIDUAL.txt" 
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V9002", "V8000", "V9010", "V9011", "V9012",
                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL"
           )
           , dec="."
  )   


#SaveRDS

saveRDS(DESPESA_INDIVIDUAL, "Despesa_Individual.rds")

#Lendo a base de dados Despesa

Despesa <- as_tibble(readRDS("Despesa_Individual.rds"))


despesa_to <- (Despesa) %>%
  filter(UF == 17) %>%
  select(UF, ESTRATO_POF, TIPO_SITUACAO_REG, COD_UPA, NUM_DOM, NUM_UC, COD_INFORMANTE,QUADRO, SEQ, V9001,V9002,V8000) %>%
  filter(V9001 %in% c(2100101, 2100401,2100201,2100301,2100402,2100501,2100601,2100701,2100801,2100901,2101001,2101101,2101201,2101301,2101501))




#Microdados Morador

MORADOR <- 
  read.fwf("MORADOR.txt" 
           , widths = c(2,4,1,9,2,1,2,2,1,2,2,4,3,1,1,
                        1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,
                        2,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "V0306", "V0401",
                           "V04021", "V04022", "V04023", "V0403",
                           "V0404", "V0405", "V0406", "V0407",
                           "V0408", "V0409", "V0410", "V0411",
                           "V0412", "V0413", "V0414", "V0415",
                           "V0416", "V041711", "V041712", "V041721",
                           "V041722", "V041731", "V041732","V041741",
                           "V041742", "V0418", "V0419", "V0420",
                           "V0421", "V0422", "V0423", "V0424",
                           "V0425", "V0426", "V0427", "V0428",
                           "V0429", "V0430", "ANOS_ESTUDO","PESO", 
                           "PESO_FINAL", "RENDA_TOTAL")
           , dec="."
  )   

#Save MoradorRDS

saveRDS(MORADOR, "Morador.rds")


#Leitura de Dados - Morador

Morador <- as.tibble(readRDS("Morador.rds"))

Morador_to <- (Morador) %>%
  filter(UF == 17) 


#Estatistica


#Homem/Mulheres - Idade
Morador_toEST <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG ,V0403, V0404) %>% 
  filter(TIPO_SITUACAO_REG == 2) %>% 
  filter(V0404 == 2) %>% 
  filter(V0403 %in% c(65:100))

#Moradia - Homem/Mulheres

Morador_toEST1 <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG) %>% 
  count(TIPO_SITUACAO_REG)


#Etnia dos individuos

Morador_toEST_Etnia <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405) %>% 
  filter(V0405 == 5) %>% 
  filter(V0404 == 2) %>% 
  filter(V0403 %in% c(35:34)) %>% 
  filter(TIPO_SITUACAO_REG == 1)

#Plano de Sa?de

Plano_de_Saude_TO <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405, V0406) %>% 
  filter(V0404 == 2) %>% 
  filter(V0406 == 1) %>% 
  filter(V0405 == 5) %>% 
  filter(V0403 %in% c(65:100)) %>% 
  filter(TIPO_SITUACAO_REG == 1)

#Teve algum rendimento ou trabalho no per?odo de 12 meses.

RenOuTrab <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405, V0407) %>% 
  filter(V0404 == 1) %>% 
  filter(V0407 == 1) %>% 
  filter(V0405 == 5) %>% 
  filter(V0403 %in% c(15:34)) %>% 
  filter(TIPO_SITUACAO_REG == 2)

#Realiza aquisi??es de bens ou despesas

AquiouDesp <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405, V0408) %>% 
  filter(V0404 == 1) %>% 
  filter(V0408 == 1) %>% 
  filter(V0405 == 5) %>% 
  filter(V0403 %in% c(15:34)) %>% 
  filter(TIPO_SITUACAO_REG == 1)
  

# Quantos cart?es de cr?ditos tem:

Cart <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405, V0409) %>% 
  filter(V0404 == 1) %>% 
  filter(V0409 %in% c(0:30)) %>% 
  filter(V0405 == 5) %>% 
  filter(V0403 %in% c(35:64)) %>% 
  filter(TIPO_SITUACAO_REG == 1)


#Alfabetizados

Alfabetizados <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405, V0414) %>% 
  filter(V0404 == 1) %>% 
  filter(V0414 == 1) %>% 
  filter(V0405 == 5) %>% 
  filter(V0403 %in% c(15:34)) %>% 
  filter(TIPO_SITUACAO_REG == 2)


#Não ALfabetizados

NãoAlfabetizados <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405, V0414) %>% 
  filter(V0404 == 1) %>% 
  filter(V0414 == 2) %>% 
  filter(V0405 == 5) %>% 
  filter(V0403 %in% c(15:64)) %>% 
  filter(TIPO_SITUACAO_REG == 2)

#Graduação

Graduandos <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405, V0419) %>% 
  filter(V0404 == 2) %>% 
  filter(V0419 %in% c(8)) %>% 
  filter(V0405 == 5) %>% 
  filter(V0403 %in% c(65:100)) %>% 
  filter(TIPO_SITUACAO_REG == 2)


# Anos de estudos 

AnosDeEstudos <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405, ANOS_ESTUDO) %>% 
  filter(ANOS_ESTUDO %in% c(16:100)) %>% 
  filter(V0405 == 5) %>%
  filter(V0403 %in% c(65:100)) %>% 
  filter(TIPO_SITUACAO_REG == 2)


#Renda total
  
Renda <- (Morador_to) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405, RENDA_TOTAL) %>% 
  filter(RENDA_TOTAL >= 3748) %>% 
  filter(V0404 == 1) %>% 
  filter(V0405 == 5) %>% 
  filter(V0403 %in% c(65:100)) %>% 
  filter(TIPO_SITUACAO_REG == 1)


#Saneamento básico

Saneamento <- (MergeDomicilio) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405, V0207) %>% 
  filter(V0207 == 1) %>% 
  filter(V0404 == 2) %>% 
  filter(V0405 == 5) %>% 
  filter(V0403 %in% c(65:100)) %>% 
  filter(TIPO_SITUACAO_REG == 2)


#Coleta de lixo 


coletaDeLixo <- (MergeDomicilio) %>% 
  select(TIPO_SITUACAO_REG, V0403, V0404, V0405, V0213) %>% 
  filter(V0213 %in% (1:2)) %>% 
  filter(V0404 == 2) %>% 
  filter(V0405 == 1) %>% 
  filter(V0403 %in% c(65:100)) %>% 
  filter(TIPO_SITUACAO_REG == 2)



#Leitura de dados Rendimento do Trabalho.

RENDIMENTO_TRABALHO <- 
  read.fwf("RENDIMENTO_TRABALHO.txt" 
           , widths = c(2,4,1,9,2,1,2,2,1,1,7,1,1,1,1,1,1,7,7,7
                        ,7,2,2,3,1,12,10,10,10,10,1,1,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SUB_QUADRO",
                           "SEQ", "V9001", "V5302", "V53021", "V5303",
                           "V5304", "V5305", "V5307", "V8500", "V531112",
                           "V531122", "V531132", "V9010", "V9011",
                           "V5314", "V5315", "DEFLATOR", "V8500_DEFLA",
                           "V531112_DEFLA", "V531122_DEFLA",
                           "V531132_DEFLA", "COD_IMPUT_VALOR",
                           "FATOR_ANUALIZACAO", "PESO", "PESO_FINAL",
                           "RENDA_TOTAL"
           )
           , dec="."
  )

#SaveRDS Rendimento trabalho

saveRDS(RENDIMENTO_TRABALHO, "Rendimento_trabalho.rds")

#Leitura Rendimento Trabalho

RendimentoTRAB <- as.tibble(readRDS("Rendimento_trabalho.rds"))

RendimentoTRAB_TO <- (RendimentoTRAB) %>%
  filter(UF == 17)

# REGISTRO - OUTROS RENDIMENTOS

OUTROS_RENDIMENTOS <- 
  read.fwf("OUTROS_RENDIMENTOS.txt" 
           , widths = c(2,4,1,9,2,1,2,2,2,7,10,10,2
                        ,2,12,10,10,1,1,14,14,10
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V8500", "V8501", "V9010", "V9011",
                           "DEFLATOR", "V8500_DEFLA", "V8501_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL")
           , dec="."
  )   


#Save RDS - Outros rendimentos

saveRDS(OUTROS_RENDIMENTOS, "Outros_rendimentos.rds")


#Leitura RDS - Outros rendimentos

Outros_Rendimentos <- as.tibble(readRDS("Outros_rendimentos.rds"))

outros_rendimentos_to <- (Outros_Rendimentos) %>%
  filter(UF == 17)


MergeDomicilio<- inner_join(x=Morador_to, y=Domicilio_TO)


# REGISTRO - DOMICILIO

DOMICILIO <- 
  read.fwf("DOMICILIO.txt" 
           , widths = c(2,4,1,9,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,2,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,14,14
           )
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "V0201", "V0202", 
                           "V0203", "V0204", "V0205", "V0206", "V0207",
                           "V0208", "V0209", "V02101", "V02102",
                           "V02103", "V02104", "V02105", "V02111",
                           "V02112", "V02113", "V0212", "V0213",
                           "V02141", "V02142", "V0215", "V02161", 
                           "V02162", "V02163", "V02164", "V0217", 
                           "V0219", "V0220", "V0221", "PESO",
                           "PESO_FINAL"
           )
           , dec="."
  )   

#SaveRDS Domicilio

saveRDS(DOMICILIO, "Domicilio.rds")


#Leitura RDS Domicilio

DOMICILIO <- as.tibble(readRDS("Domicilio.rds"))

Domicilio_TO <- (DOMICILIO) %>% 
  filter(UF == 17)




Banco_de_DadosPesquisa <- inner_join(x= Morador_to, y=Domicilio_TO)

saveRDS(Banco_de_DadosPesquisa, "BancO_de_DadosPesquisa_atualizado.rds")



write.csv(Banco_de_DadosPesquisa,"Banco de Dados.csv")
