library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(odbc)


Morador <- as.tibble(readRDS("Morador.rds"))

Morador_to <- (Morador) %>%
  filter(UF == 17) 

Moradores_urbano <- (Morador_to) %>%
  filter(TIPO_SITUACAO_REG == 1)

Moradores_rural <- (Morador_to) %>%
  filter(TIPO_SITUACAO_REG == 2)