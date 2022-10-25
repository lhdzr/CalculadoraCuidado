# Punteo AMAI y ENUT

library(tidyverse)

tvivienda = read.csv("data/conjunto_de_datos_enut_2019_csv/conjunto_de_datos_enut_2019/conjunto_de_datos_tvivienda_enut_2019/conjunto_de_datos/conjunto_de_datos_tvivienda_enut_2019.csv",
                     encoding = "utf-8")

thogar = read.csv("data/conjunto_de_datos_enut_2019_csv/conjunto_de_datos_enut_2019/conjunto_de_datos_thogar_enut_2019/conjunto_de_datos/conjunto_de_datos_thogar_enut_2019.csv",
                  encoding = "utf-8")

tmodulo = read.csv("data/conjunto_de_datos_enut_2019_csv/conjunto_de_datos_enut_2019/conjunto_de_datos_tmodulo_enut_2019/conjunto_de_datos/conjunto_de_datos_tmodulo_enut_2019.csv",
                   encoding = "utf-8")

tsdem = read.csv("data/conjunto_de_datos_enut_2019_csv/conjunto_de_datos_enut_2019/conjunto_de_datos_tsdem_enut_2019/conjunto_de_datos/conjunto_de_datos_tsdem_enut_2019.csv",
                 encoding = "utf-8")

#### Renombrar columnas con nombre importado incorrectamente

#tvivienda = tvivienda %>% rename(UPM=ï..UPM)
#thogar = thogar %>% rename(UPM=ï..UPM)
#tsdem = tsdem %>% rename(UPM=ï..UPM)

### Lectura de variables seleccionadas

var_sel = read.csv("data/variables_seleccionadas - Bases de datos .csv",encoding = "utf-8")


#### Definición de variables seleccionadas para cada tabla

vars_viv = toupper(var_sel$Variable[var_sel$Tabla=="tvivienda"])
vars_hog = toupper(var_sel$Variable[var_sel$Tabla=="thogar"])
vars_sdm = toupper(var_sel$Variable[var_sel$Tabla=="tsdem"])
vars_mod = toupper(var_sel$Variable[var_sel$Tabla=="tmodulo"])


#### Subset de las tablas usando únicamente variables seleccionadas

tvivienda = subset(tvivienda,select = vars_viv)
thogar = subset(thogar,select = vars_hog)
tsdem = subset(tsdem,select = vars_sdm)
tmodulo = subset(tmodulo,select = vars_mod)

#### Definición de identificadores

tvivienda$vid = paste(tvivienda$UPM,tvivienda$VIV_SEL, sep = "_")

thogar$vid = paste(thogar$UPM,thogar$VIV_SEL,sep="_")
thogar$hid = paste(thogar$UPM,thogar$VIV_SEL,thogar$HOGAR,sep="_")

tsdem$vid = paste(tsdem$UPM,tsdem$VIV_SEL,sep="_")
tsdem$hid = paste(tsdem$UPM,tsdem$VIV_SEL,tsdem$HOGAR,sep="_")
tsdem$uid = paste(tsdem$UPM,tsdem$VIV_SEL,tsdem$HOGAR,tsdem$N_REN,sep="_")

tmodulo$vid = paste(tmodulo$UPM,tmodulo$VIV_SEL,sep="_")
tmodulo$hid = paste(tmodulo$UPM,tmodulo$VIV_SEL,tmodulo$HOGAR,sep="_")
tmodulo$uid = paste(tmodulo$UPM,tmodulo$VIV_SEL,tmodulo$HOGAR,tmodulo$N_REN,sep="_")


#Juntar tablas

#enut = left_join(tmodulo,tsdem,on="uid") %>%
#  left_join(thogar,on="hid") %>%
#  left_join(tvivienda,on="vid") %>%
#  full_join(tab_esc,on="NIV") %>%
#  full_join(tab_sal,on="P5_7A") %>%
#  mutate(ESC=ESC+GRA, SAL_SEM=P5_7/DIVISOR_SALARIO)

#Transformar variables a dummies (pregunta 2-5 de la AMAI, falta la 1 y 6)

## Jefe de hogar (1 es sí es jefe de hogar y 0 no)
tsdem$jefehog<- (tsdem$PAREN == "1")*1

#Transformar nivel y grado a escolaridad
tmodulo$nivelescolar<- ifelse(tmodulo$NIV<=1, "No estudió",
                       ifelse(tmodulo$NIV==2 & tmodulo$GRA<6, "Primaria incompleta",
                       ifelse(tmodulo$NIV==2 & tmodulo$GRA>=6, "Primaria completa",
                       ifelse(tmodulo$NIV==3 & tmodulo$GRA<3, "Secundaria incompleta",
                       ifelse(tmodulo$NIV==3 & tmodulo$GRA>=3, "Secundaria completa",
                       ifelse(tmodulo$NIV==4, "Secundaria completa",
                       ifelse(tmodulo$NIV==5|tmodulo$NIV==7, "Preparatoria completa",
                       ifelse(tmodulo$NIV==6 & tmodulo$GRA<3, "Preparatoria incompleta",
                       ifelse(tmodulo$NIV==6 & tmodulo$GRA>=3, "Preparatoria completa",
                       ifelse(tmodulo$NIV==8 & tmodulo$GRA<5, "Licenciatura incompleta",
                       ifelse(tmodulo$NIV==8 & tmodulo$GRA>=5, "Licenciatura completa",
                       ifelse(tmodulo$NIV==9, "Diplomado o maestría", "NA"))))))))))))

## Baño en vivienda (1 es sí y 0 no)
tvivienda$sanitarioviv<- (tvivienda$P1_7 == "1")*1

##Automovil (1 es sí y 0 no)
thogar$automovilhog<- (thogar$P2_5_09 == "1")*1

## Internet (1 es sí y 0 no)
thogar$internethog<- (thogar$P2_5_14 == "1")*1 

##Trabajas o no (1 es sí y 0 no)
tmodulo$trabajaper<- (tmodulo$P5_1 =="1")*1 

#Agrupar por hogar 
trabaj_hogar <- aggregate.data.frame(tmodulo$P5_1, by=list(tmodulo$hid),FUN=sum) %>%
rename(hid=Group.1,num_trab=x)

thogar= left_join(thogar,trabaj_hogar,on="hid")

#Crear variable de puntaje por pregunta

## Jefe de hogar (1 es sí es jefe de hogar y 0 no)
## Puntaje de nivel escolaridad 

tmodulo %>% 
  mutate(case_when(nivelescolar=="No estudió"~0,
                   nivelescolar=="Primaria incompleta"~6, 
                   nivelescolar=="Primaria completa"~ 11, 
                   nivelescolar=="Secundaria incompleta"~ 12,
                   nivelescolar=="Secundaria completa"~18,
                   nivelescolar=="Preparatoria incompleta"~23,
                   nivelescolar=="Preparatoria completa"~27, 
                   nivelescolar=="Licenciatura incompleta"~36,
                   nivelescolar=="Licenciatura completa"~59,
                   nivelescolar=="Diplomado o maestría"~85))

## Baño en vivienda
tvivienda$puntosbaño<- ifelse(tvivienda$sanitarioviv == "1", 24, 0)

##Automovil
thogar$puntosautomovil<- ifelse(thogar$automovilhog == "1", 22, 0)

## Internet
thogar$puntosinternet<- ifelse(thogar$internethog == "1", 32, 0)

#Cuartos
tvivienda$puntoscuartos<- ifelse(tvivienda$P1_3=="1", 8,
                          ifelse(tvivienda$P1_3=="2", 16,
                          ifelse(tvivienda$P1_3=="3", 24, 
                          ifelse(tvivienda$P1_3>="4", 32, 0))))

#Trabajadores por hogar 
#Cuartos

thogar$puntostrab<- ifelse(thogar$num_trab=="0", 0,
                    ifelse(thogar$num_trab=="1", 15,
                    ifelse(thogar$num_trab=="2", 31,
                    ifelse(thogar$num_trab=="3", 46,
                    ifelse(thogar$num_trab>="4", 61, "NA")))))

#Puntaje final 


#Puntaje a Nivel Socioeconómico 

thogar$puntoscuartos<- ifelse(tvivienda$P1_3=="1", 8,
                                 ifelse(tvivienda$P1_3=="2", 16,
                                        ifelse(tvivienda$P1_3=="3", 24, 
                                               ifelse(tvivienda$P1_3>="4", 32, 0))))