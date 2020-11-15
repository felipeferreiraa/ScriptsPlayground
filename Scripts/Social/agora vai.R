library(tidyverse)

UF <- read.fwf("PNADC_2018_visita5.txt", widths = c(-5,2))
VD2002 <- read.fwf("PNADC_2018_visita5.txt", widths = c(-512,2))
VD4008 <- read.fwf("PNADC_2018_visita5.txt", widths = c(-527,1))
VD4020 <- read.fwf("PNADC_2018_visita5.txt", widths = c(-563,8))
VD5002 <- read.fwf("PNADC_2018_visita5.txt", widths = c(-628,8))

VD4008 <- c(1=="Empregado no setor privado", 2=="Trabalhador doméstico",
            3=="Empregado no setor público (inclusive servidor estatutário e militar)",
            4=="Empregador", 5=="Conta-própria", 6=="Trabalhador familiar auxiliar")

vai.carai <- dplyr::bind_cols(UF, VD2002, VD4008, VD4020, VD5002)

colnames(vai.carai) <- c("UF", "VD2002", "VD4008", "VD4020", "VD5002")

vai.carai4<-vai.carai[!(vai.carai$VD4008==6),]

vai.carai3<-vai.carai4[!(vai.carai4$VD2002=="15" | vai.carai4$VD2002=="16" | vai.carai4$VD2002=="17"),]

saveRDS(vai.carai3, "Lucas_IBGE.rds")

vai.carai.4 <- as_tibble(readRDS("Lucas_IBGE.rds"))




amostraBR <- (vai.carai3) %>%
  select(VD5002)%>%
  na.omit()


#Taxa de pobreza BR
vai.carai6 <- (vai.carai3)%>%
  group_by(VD4008)%>%
  filter(VD4008 %in% c(1,2,3,4,5,"NA")| n() == 1) 


pobBR <- (vai.carai3)%>%
  select(VD5002)%>%
  filter(VD5002 <= 411)

taxa_pob_BR_2018 <- dim(vai.carai6) / dim(vai.carai3)

#Taxa de pobreza TO
amostra <- (vai.carai)%>%
  filter(UF == 17)%>%
  select(VD5002)%>%
  na.omit()
pob <- (vai.carai)%>%
  filter(UF == 17)%>%
  select(VD5002)%>%
  na.omit()%>%
  filter(VD5002 <= 411)

taxa_pob_TO_2018 <- dim(pob) / dim(amostra)

