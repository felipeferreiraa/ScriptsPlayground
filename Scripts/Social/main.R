library(tidyverse)
library(ipeadatar)
library(PNADcIBGE)
library(survey)
library(srvyr)

#PNAD 2018 (1 entrevista)
pnad_2018 <- get_pnadc(
  year = 2018,
  interview = 1,
  vars = c("UF", "V1032", "V2005", "V2007", "V2001" ,"VD2002", "VD4008", "VD4020", "VD5002"),
  design = TRUE,
  labels = FALSE,
  deflator = TRUE,
  defyear = 2018
)

# pnad_2018 <- readRDS('pnad/pnad_2018_visita1.rds')
# pnad <- as_tibble(pnad_2018$variables)

# pnad_design <- pnad_2018$variables %>%
#   as_survey_design(
#     ids = UPA,
#     strata = Estrato,
#     weights = V1032,
#     nest = TRUE
#   )

pnad_2018[["variables"]] <- pnad_2018$variables %>%
  mutate(
    VD5002_real = ifelse(VD5002 != 0, VD5002 * CO1e, NA)
  )

# Linha de pobreza R$419.7368
pobreza <- svytotal(~VD5002_real < 419.7368, pnad_2018, na.rm = T)
