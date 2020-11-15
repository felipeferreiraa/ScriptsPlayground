library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(odbc)


DESPESA_COLETIVA <- 
  read.fwf("DESPESA_COLETIVA.txt"
           , widths = c(2,4,1,9,2,1,2,2,7,2,4,10,2,2,1
                        ,10,1,12,10,10,1,1,2,14,14,10)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9002", "V9005", "V8000",
                           "V9010", "V9011", "V9012", "V1904",
                           "V1905", "DEFLATOR", "V8000_DEFLA",
                           "V1904_DEFLA", "COD_IMPUT_VALOR",
                           "COD_IMPUT_QUANTIDADE", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL"
           )
           , dec="."
  )
