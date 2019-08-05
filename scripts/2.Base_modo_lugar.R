####################################
###           Gina               ###
###        23/07/2019            ###
###    Base de modo y lugar      ###
###        Amnistía              ###
####################################       
library(tidyverse)
rm(list=ls())
setwd("~")

#Directorios
inp <- "/Users/georginajimenezrios/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/amnistia/datos/inp"
out<- "/Users/georginajimenezrios/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/amnistia/datos/out"

##Base de tasas ##
base_full <- read.csv(paste(inp, "sinais_full_variables.csv", sep="/"))

######Base sobre modo y lugar de ocurrencia #Modo, lugar, edad, año
tempo <- base_full %>% 
  mutate(lugar_ne = ifelse(via_publica != 1 & vivienda != 1 & lugar_ne_otro != 1, 1, 0),
         grupo_edad = case_when(edad < 12 ~ "De 0 a 11 años", 
                                edad >= 12 & edad < 20 ~ "De 12 a 19 años",
                                edad >= 20 & edad < 28 ~ "De 20 a 27 años",
                                edad >= 28 & edad < 36 ~ "De 28 a 35 años",
                                edad >= 36 & edad < 44 ~ "De 36 a 43 años",
                                edad >= 44 & edad < 55 ~ "De 44 a 54 años",
                                edad >= 55 & edad < 65 ~ "De 55 a 64 años",
                                edad >= 65  ~ "De 65 en adelante")) %>% 
  select(sexo, year, grupo_edad,arma_fuego, arma_blanca, ahorcamiento, 
         veneno, medio_no_esp, fuerza_corporal, via_publica, vivienda, lugar_ne_otro, lugar_ne)%>%
  mutate(modo = ifelse(arma_fuego==1, "Arma de fuego",
                ifelse(arma_blanca==1, "Arma blanca",
                ifelse(ahorcamiento==1, "Ahorcamiento o ahogamiento",
                ifelse(fuerza_corporal==1, "Fuerza corporal", 
                ifelse(veneno==1, "Veneno",
                ifelse(medio_no_esp==1, "No especificado", "Otro")))))))%>%
  mutate(lugar = ifelse(lugar_ne_otro==1, "Otro",
                 ifelse(lugar_ne==1, "No especificado",
                 ifelse(via_publica==1, "Via Pública",
                 ifelse(vivienda==1, "Vivienda", "upsy")))))%>%
  mutate(total=1)%>%
  group_by(sexo, lugar, year, grupo_edad, modo)%>% 
  summarize(total = sum(total, na.rm = T))%>%
  ungroup() %>% 
  filter(sexo != "No especificado" & is.na(grupo_edad)==F) %>%
  filter(year>1999)%>%
  group_by(sexo, year, grupo_edad) %>% 
  mutate(denomin = sum(total, na.rm = T),
           porcent = round(total / denomin * 100, 1))

write_excel_csv(tempo, paste(out, "base_modo_lugar.csv", sep="/")) 
  
