####################################
###           Gina               ###
###        23/07/2019            ###
###      Base de tasas           ###
###        Amnistía              ###
####################################       

rm(list=ls())
setwd("~")

#Directorios
inp <- "/Users/georginajimenezrios/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/amnistia/datos/inp"
out<- "/Users/georginajimenezrios/Dropbox (Data Cívica A.C.)/Data Cívica/Proyectos/OnGoing/amnistia/datos/out"

##Base de tasas ##
base_full <- read_csv(paste(inp, "sinais_full_variables.csv", sep="/"))
poblacion<-read_csv(paste(inp, "pob_porsexo_pormun_9018.csv", sep = "/"))

pob_2 <- read_xlsx(paste(inp, "pob_mitadanio_ents_7050.xlsx", sep = "/"))
pob_2 <- filter(pob_2, year > 1999 & year <= 2017)

pob_nac_edad <- pob_2 %>% 
  mutate(grupo_edad = case_when(edad < 12 ~ "De 0 a 11 años", 
                                edad >= 12 & edad < 20 ~ "De 12 a 19 años",
                                edad >= 20 & edad < 28 ~ "De 20 a 27 años",
                                edad >= 28 & edad < 36 ~ "De 28 a 35 años",
                                edad >= 36 & edad < 44 ~ "De 36 a 43 años",
                                edad >= 44 & edad < 55 ~ "De 44 a 54 años",
                                edad >= 55 & edad < 65 ~ "De 55 a 64 años",
                                edad >= 65  ~ "De 65 en adelante"),
         sexo = str_replace_all(sexo, "Mujeres", "Mujer"),
         sexo = str_replace_all(sexo, "Hombres", "Hombre")) %>%              
  group_by(year, sexo, grupo_edad, cve_geo, ent) %>% 
  summarize(total_pob = sum(poblacion, na.rm = T))%>%
  mutate(ent_ocurr=formatC(cve_geo, width = 2, flag = "0", format = "d"))

#Estados
estados <- base_full %>% 
  mutate(tot = 1,
         grupo_edad = case_when(edad < 12 ~ "De 0 a 11 años", 
                                edad >= 12 & edad < 20 ~ "De 12 a 19 años",
                                edad >= 20 & edad < 28 ~ "De 20 a 27 años",
                                edad >= 28 & edad < 36 ~ "De 28 a 35 años",
                                edad >= 36 & edad < 44 ~ "De 36 a 43 años",
                                edad >= 44 & edad < 55 ~ "De 44 a 54 años",
                                edad >= 55 & edad < 65 ~ "De 55 a 64 años",
                                edad >= 65  ~ "De 65 en adelante")) %>% 
  group_by(sexo, year, grupo_edad, ent_ocurr) %>% 
  summarize(total_total = sum(tot, na.rm = T)) %>% 
  filter(year>1999 & year<2018)%>%
  left_join(pob_nac_edad, by=c("year", "sexo", "grupo_edad", "ent_ocurr")) %>% 
  ungroup() %>% 
  mutate(tasa_total = round(total_total / total_pob * 100000, 1)) %>% 
  ungroup() %>% 
  filter(is.na(grupo_edad)==F)

#Nacional
tempo<-group_by(estados, sexo, year, grupo_edad)%>%
  summarize(total_total=sum(total_total, na.rm=T))%>%
  filter(sexo!="No especificado")%>%
  mutate(ent_ocurr="00")%>%
  left_join(pob_nac_edad, by=c("sexo", "ent_ocurr", "year", "grupo_edad"))%>%
  filter(ent_ocurr=="00")%>%
  mutate(tasa_total=round(total_total / total_pob * 100000, 1))

estados<-bind_rows(estados, tempo)%>%
  select(ent_ocurr, ent, year, sexo, grupo_edad, total_total, tasa_total)

estados$nom_ent<-ifelse(estados$ent_ocurr=="00", "Nacional", estados$nom_ent)
write_excel_csv(estados, paste(out, "tasa_estatal.csv", sep="/")) 
rm(estados, nombres, pob_2, pob_nac_edad, poblacion, tempo)

