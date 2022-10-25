library(tidyverse)

# LECTURA DE LA ENOE
enoe_c1 = read_csv("data/conjunto_de_datos_enoen_2022_2t_csv/conjunto_de_datos_coe1_enoen_2022_2t/conjunto_de_datos/conjunto_de_datos_coe1_enoen_2022_2t.csv")
enoe_c2 = read_csv("data/conjunto_de_datos_enoen_2022_2t_csv/conjunto_de_datos_coe2_enoen_2022_2t/conjunto_de_datos/conjunto_de_datos_coe2_enoen_2022_2t.csv")

# LECTURA DE VARIABLES SELECCIONADAS
var_sel = read.csv("data/variables_seleccionadas.csv",encoding = "utf-8")

# SELECCIÓN DE VARIABLES EN LAS TABLAS
vars_c1 = var_sel$variable[var_sel$encuesta=="ENOE" & var_sel$tabla=="coe1"]
enoe_c1 = subset(enoe_c1,select = vars_c1)

vars_c2 = var_sel$variable[var_sel$encuesta=="ENOE" & var_sel$tabla=="coe2"]
enoe_c2 = subset(enoe_c2,select = vars_c2)

# DEFINICIÓN DE IDENTIFICADORES
attach(enoe_c1)
enoe_c1$uid = paste(upm,v_sel,n_hog,n_ren,sep = "_")
enoe_c1$hid = paste(upm,v_sel,n_hog,sep = "_")
enoe_c1$vid = paste(upm,v_sel,sep = "_")
detach()

attach(enoe_c2)
enoe_c2$uid = paste(upm,v_sel,n_hog,n_ren,sep = "_")
enoe_c2$hid = paste(upm,v_sel,n_hog,sep = "_")
enoe_c2$vid = paste(upm,v_sel,sep = "_")
detach()

# CRUCE DE TABLAS
enoe = left_join(enoe_c1,enoe_c2,on="uid")



###### codigos = 2821,8343,2812,5221,5222
# SUBSET DE ENFERMEROS
enf = filter(enoe,p3==2821)
# SUBSET DE CUIDADORES EN ESTABLECIMIENTOS
cuid_inst = filter(enoe,p3==5221)
# SUBSET DE CUIDADORES EN HOGARES
cuid_hog = filter(enoe,p3==5222)







attach(enf)

# Horas trabajadas por semana

head(p5d_thrs)
hist(p5d_thrs, 
     main = "Horas trabajadas por semana",
     xlab = "Horas totales",
     ylab = "Frecuencia")


#Cada cuánto recibe
# Descripción de los datos.
summary(p6b1)
mutate(p6b1=as.factor(p6b1))
table(p6b1)
head(p6b1)
hist(p6b1, 
     main = "Periodo de recepción de salario",
     xlab = "Tipo de periodo",
     ylab = "Frecuencia")


# Cuánto recibe
head(p6b2)
hist(p6b2)

# sal_sem
detach()
enf %>%
  mutate(sal_sem = case_when(p6b1 == 1 ~ p6b2/4,
                             p6b1 == 2 ~ p6b2/2,
                             p6b1 == 3 ~ p6b2,
                             p6b1 == 4 ~ p6b2*p5d_tdia,
                             p6b1 == 5 ~ NA,
                             p6b1 == 7 ~ NA,
                             p6b1 == 8 ~ NA))

# sal_sem / hrs_sem = sal_hr



