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

var_sel = read.csv("data/variables_seleccionadas.csv",encoding = "utf-8")


#### Definición de variables seleccionadas para cada tabla

vars_viv = toupper(var_sel$variable[var_sel$tabla=="tvivienda"])
vars_hog = toupper(var_sel$variable[var_sel$tabla=="thogar"])
vars_sdm = toupper(var_sel$variable[var_sel$tabla=="tsdem"])
vars_mod = toupper(var_sel$variable[var_sel$tabla=="tmodulo"])


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

#Transformar variables a dummies y categóricas

## Pregunta 1: Transformar nivel y grado a escolaridad

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

## Pregunta 2: Baño en vivienda (1 es sí y 0 no)
tvivienda$sanitarioviv<- (tvivienda$P1_7 == "1")*1

## Pregunta 3: Automovil (1 es sí y 0 no)
thogar$automovilhog<- (thogar$P2_5_09 == "1")*1

## Pregunta 4: Internet (1 es sí y 0 no)
thogar$internethog<- (thogar$P2_5_14 == "1")*1 

##Pregunta 5: Trabajas o no (1 es sí y 0 no)
tmodulo$trabajaper<- (tmodulo$P5_1 =="1")*1 

#Agrupar por hogar 
trabaj_hogar <- aggregate.data.frame(tmodulo$P5_1, by=list(tmodulo$hid),FUN=sum) %>%
rename(hid=Group.1,num_trab=x)

thogar= left_join(thogar,trabaj_hogar,on="hid")

#Crear variable de puntaje por pregunta

## 1. Puntaje de nivel escolaridad 

jefes= filter(tsdem, PAREN==1) %>%
  left_join(tmodulo, on="uid") %>%
  select(c("nivelescolar", "hid"))

thogar=left_join(thogar, jefes, on="hid")

thogar$puntos_nivel<- ifelse(thogar$nivelescolar=="No estudió",0,
                      ifelse(thogar$nivelescolar=="Primaria incompleta",6,
                      ifelse(thogar$nivelescolar=="Primaria completa",11,
                      ifelse(thogar$nivelescolar=="Secundaria incompleta",12,
                      ifelse(thogar$nivelescolar=="Secundaria completa",18,
                      ifelse(thogar$nivelescolar=="Preparatoria incompleta",23,
                      ifelse(thogar$nivelescolar=="Preparatoria completa",27,
                      ifelse(thogar$nivelescolar=="Licenciatura incompleta",36,
                      ifelse(thogar$nivelescolar=="Licenciatura completa",59,
                      ifelse(thogar$nivelescolar=="Diplomado o maestría",85, 0))))))))))

#tmodulo %>% 
#  mutate(case_when(nivelescolar=="No estudió"~0,
#                   nivelescolar=="Primaria incompleta"~6, 
#                   nivelescolar=="Primaria completa"~ 11, 
#                   nivelescolar=="Secundaria incompleta"~ 12,
#                   nivelescolar=="Secundaria completa"~18,
#                   nivelescolar=="Preparatoria incompleta"~23,
#                   nivelescolar=="Preparatoria completa"~27, 
#                   nivelescolar=="Licenciatura incompleta"~36,
#                   nivelescolar=="Licenciatura completa"~59,
#                   nivelescolar=="Diplomado o maestría"~85))


## 2. Baño en vivienda
tvivienda$puntos_baño<- ifelse(tvivienda$sanitarioviv == "1", 24, 0)

## 3. Automovil
thogar$punto_automovil<- ifelse(thogar$automovilhog == "1", 22, 0)

## 4. Internet
thogar$puntos_internet<- ifelse(thogar$internethog == "1", 32, 0)

## 5. Cuartos
tvivienda$puntos_cuartos<- ifelse(tvivienda$P1_3=="1", 8,
                          ifelse(tvivienda$P1_3=="2", 16,
                          ifelse(tvivienda$P1_3=="3", 24, 
                          ifelse(tvivienda$P1_3>="4", 32, 0))))

## 6. Trabajadores por hogar 

thogar$puntostrab<- ifelse(thogar$num_trab=="0", 0,
                    ifelse(thogar$num_trab=="1", 15,
                    ifelse(thogar$num_trab=="2", 31,
                    ifelse(thogar$num_trab=="3", 46,
                    ifelse(thogar$num_trab>="4", 61, 0)))))

#UNIR TODO

tab_esc <- data.frame(NIV=c(0:10,99),ESC=c(0,0,0,6,9,9,9,12,12,16,16,0))
tab_sal <- data.frame(P5_7A=c(1,2,3,4), DIVISOR_SALARIO = c(1,2,4,52))

enut <- left_join(tmodulo,tsdem,on="uid") %>%
  left_join(thogar,on="hid") %>%
  left_join(tvivienda,on="vid") %>%
  full_join(tab_esc,on="NIV") %>%
  full_join(tab_sal,on="P5_7A") %>%
  mutate(ESC=ESC+GRA, SAL_SEM=P5_7/DIVISOR_SALARIO)

#Puntaje final 

attach(enut)
enut$Puntaje_AMAI<- puntos_nivel+ puntos_baño+ punto_automovil+ puntos_internet+ puntos_cuartos+puntostrab


#Puntaje a Nivel Socioeconómico 

attach(enut)
enut$clasif_AMAI<- ifelse(Puntaje_AMAI>=0 & Puntaje_AMAI<=47, "E",
                   ifelse(Puntaje_AMAI>=48 & Puntaje_AMAI<=94, "D",
                   ifelse(Puntaje_AMAI>=95 & Puntaje_AMAI<=115, "D+",
                   ifelse(Puntaje_AMAI>=116 & Puntaje_AMAI<=140, "C-",
                   ifelse(Puntaje_AMAI>=141 & Puntaje_AMAI<=167, "C",
                   ifelse(Puntaje_AMAI>=168 & Puntaje_AMAI<=201, "C+",
                   ifelse(Puntaje_AMAI>=202, "A/B", "NA")))))))

attach(enut)
enut$salarioprom_AMAI<- ifelse(clasif_AMAI=="E", 6200,
                        ifelse(clasif_AMAI=="D", 8900,
                        ifelse(clasif_AMAI=="D+", 11900,
                        ifelse(clasif_AMAI=="C-", 15000,
                        ifelse(clasif_AMAI=="C", 18800,
                        ifelse(clasif_AMAI=="C+", 26300,
                        ifelse(clasif_AMAI=="A/B", 40500, "NA")))))))

#Comparar con Salario promedio por Nivel 
tapply(enut$SAL_SEM, enut$clasif_AMAI, summary)


#PRUEBA 

jefes= filter(tsdem, PAREN==1) %>%
  left_join(tmodulo, on="uid") %>%
  select(c("nivelescolar", "hid"))

thogar=left_join(thogar, jefes, on="hid")



#Shiny
thogar$puntos_nivel<- ifelse(thogar$nivelescolar=="No estudió",0,
                             ifelse(thogar$nivelescolar=="Primaria incompleta",6,
                                    ifelse(thogar$nivelescolar=="Primaria completa",11,
                                           ifelse(thogar$nivelescolar=="Secundaria incompleta",12,
                                                  ifelse(thogar$nivelescolar=="Secundaria completa",18,
                                                         ifelse(thogar$nivelescolar=="Preparatoria incompleta",23,
                                                                ifelse(thogar$nivelescolar=="Preparatoria completa",27,
                                                                       ifelse(thogar$nivelescolar=="Licenciatura incompleta",36,
                                                                              ifelse(thogar$nivelescolar=="Licenciatura completa",59,
                                                                                     ifelse(thogar$nivelescolar=="Diplomado o maestría",85, 0))))))))))

## 2. Baño en vivienda
tvivienda$puntos_baño<- ifelse(tvivienda$sanitarioviv == "1", 24, 0)

## 3. Automovil
thogar$punto_automovil<- ifelse(thogar$automovilhog == "1", 22, 0)

## 4. Internet
thogar$puntos_internet<- ifelse(thogar$internethog == "1", 32, 0)

## 5. Cuartos
tvivienda$puntos_cuartos<- ifelse(tvivienda$P1_3=="1", 8,
                           ifelse(tvivienda$P1_3=="2", 16,
                           ifelse(tvivienda$P1_3=="3", 24, 
                           ifelse(tvivienda$P1_3>="4", 32, 0))))

## 6. Trabajadores por hogar 

thogar$puntostrab<- ifelse(thogar$num_trab=="0", 0,
                           ifelse(thogar$num_trab=="1", 15,
                                  ifelse(thogar$num_trab=="2", 31,
                                         ifelse(thogar$num_trab=="3", 46,
                                                ifelse(thogar$num_trab>="4", 61, 0)))))

