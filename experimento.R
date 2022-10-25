library(tidyverse)

# LECTURA DE DATOS
tvivienda = read.csv("data/conjunto_de_datos_enut_2019_csv/conjunto_de_datos_enut_2019/conjunto_de_datos_tvivienda_enut_2019/conjunto_de_datos/conjunto_de_datos_tvivienda_enut_2019.csv",
                     encoding = "utf-8")
thogar = read.csv("data/conjunto_de_datos_enut_2019_csv/conjunto_de_datos_enut_2019/conjunto_de_datos_thogar_enut_2019/conjunto_de_datos/conjunto_de_datos_thogar_enut_2019.csv",
                  encoding = "utf-8")
tmodulo = read.csv("data/conjunto_de_datos_enut_2019_csv/conjunto_de_datos_enut_2019/conjunto_de_datos_tmodulo_enut_2019/conjunto_de_datos/conjunto_de_datos_tmodulo_enut_2019.csv",
                 encoding = "utf-8")
tsdem = read.csv("data/conjunto_de_datos_enut_2019_csv/conjunto_de_datos_enut_2019/conjunto_de_datos_tsdem_enut_2019/conjunto_de_datos/conjunto_de_datos_tsdem_enut_2019.csv",
                   encoding = "utf-8")

# RENOMBRAR COLUMNAS CON NOMBRE IMPORTADO INCORRECTAMENTE
tvivienda = tvivienda %>% rename(UPM=ï..UPM)
thogar = thogar %>% rename(UPM=ï..UPM)
tsdem = tsdem %>% rename(UPM=ï..UPM)

# LECTURA DE VARIABLES SELECCIONADAS
var_sel = read.csv("data/variables_seleccionadas.csv",encoding = "utf-8")

# VARIABLES SELECCIONADAS PARA CADA TABLA
vars_viv = toupper(var_sel$variable[var_sel$tabla=="tvivienda"])
vars_hog = toupper(var_sel$variable[var_sel$tabla=="thogar"])
vars_sdm = toupper(var_sel$variable[var_sel$tabla=="tsdem"])
vars_mod = toupper(var_sel$variable[var_sel$tabla=="tmodulo"])

# SUBSET DE LAS TABLAS USANDO ÚNICAMENTE VARIABLES SELECCIONADAS
tvivienda = subset(tvivienda,select = vars_viv)
thogar = subset(thogar,select = vars_hog)
tsdem = subset(tsdem,select = vars_sdm)
tmodulo = subset(tmodulo,select = vars_mod)

# DEFINICIÓN DE IDENTIFICADORES
tvivienda$vid = paste(tvivienda$upm,tvivienda$VIV_SEL,sep = "_")

thogar$vid = paste(thogar$upm,thogar$VIV_SEL,sep="_")
thogar$hid = paste(thogar$upm,thogar$VIV_SEL,thogar$HOGAR,sep="_")

tsdem$vid = paste(tsdem$UPM,tsdem$VIV_SEL,sep="_")
tsdem$hid = paste(tsdem$UPM,tsdem$VIV_SEL,tsdem$HOGAR,sep="_")
tsdem$uid = paste(tsdem$UPM,tsdem$VIV_SEL,tsdem$HOGAR,tsdem$N_REN,sep="_")

tmodulo$vid = paste(tmodulo$upm,tmodulo$VIV_SEL,sep="_")
tmodulo$hid = paste(tmodulo$upm,tmodulo$VIV_SEL,tmodulo$HOGAR,sep="_")
tmodulo$uid = paste(tmodulo$upm,tmodulo$VIV_SEL,tmodulo$HOGAR,tmodulo$N_REN,sep="_")





