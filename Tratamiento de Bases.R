library(tidyverse)

# TABULADO DE NIVEL DE ESCOLARIDAD Y AÑOS DE ESCOLARIDAD COMPLETADOS
tab_esc <- data.frame(NIV=c(0:10,99),ESC=c(0,0,0,6,9,9,9,12,12,16,16,0))
# TABULADO DE PERIODICIDAD DE RECEPCIÓN DE SALARIO Y NUMERO ESTANDARIZADOR A SALARIO SEMANAL
tab_sal = data.frame(P5_7A=c(1,2,3,4), DIVISOR_SALARIO = c(1,2,4,52))


tsdem$SEXO[tsdem$SEXO==1]=0
tsdem$SEXO[tsdem$SEXO==2]=1








# PUNTOS AMAI
## Baño en vivienda (1 es sí y 0 no)
tvivienda$sanitarioviv<- (tvivienda$P1_7 == "1")*1

##Automovil (1 es sí y 0 no)
thogar$automovilhog<- (thogar$P2_5_09 == "1")*1

## Internet (1 es sí y 0 no)
thogar$internethog<- (thogar$P2_5_14 == "1")*1 

## Trabajadores en el hogar
trabaj_hogar <- aggregate.data.frame(tmodulo$P5_1, by=list(tmodulo$hid),FUN=sum) %>%
  rename(hid=Group.1,num_trab=x)
thogar = left_join(thogar,trabaj_hogar,on="hid")


##Trabajas o no (1 es sí y 0 no)
tmodulo$trabajaper<- (tmodulo$P5_1 =="1")*1






# UNIÓN DE TABLAS Y DEFINICIÓN DE AÑOS DE ESCOLARIDAD
enut = left_join(tmodulo,tsdem,on="uid") %>%
  left_join(thogar,on="hid") %>%
  left_join(tvivienda,on="vid") %>%
  full_join(tab_esc,on="NIV") %>%
  full_join(tab_sal,on="P5_7A") %>%
  mutate(ESC=ESC+GRA, SAL_SEM=P5_7/DIVISOR_SALARIO, EXP=EDAD-ESC-6)


# SUBSET POR JEFES DE HOGAR
jefes = subset(enut,PAREN==1)


attach(enut)
lm.fit = lm(SAL_SEM~EDAD+SEXO+ESC+EXP+EXP**2)
detach()

vars_tiempo = c(P6_15A_1_1,P6_15A_1_3,P6_15A_2_1,P6_15A_2_3,P6_15A_3_1,P6_15A_3_3,P6_15A_4_1,P6_15A_4_3,P6_15A_1_2,P6_15A_1_4,P6_15A_2_2,P6_15A_2_4,P6_15A_3_2,P6_15A_3_4,P6_15A_4_2,P6_15A_4_4)
if(all(is.na(vars_tiempo))){
  enut$TIEMPO_TOTAL = NA
  }else{
  enut$TIEMPO_TOTAL = sum(P6_15A_1_1,P6_15A_1_3,P6_15A_2_1,P6_15A_2_3,P6_15A_3_1,P6_15A_3_3,P6_15A_4_1,P6_15A_4_3)+sum(P6_15A_1_2,P6_15A_1_4,P6_15A_2_2,P6_15A_2_4,P6_15A_3_2,P6_15A_3_4,P6_15A_4_2,P6_15A_4_4)/60
  }

# ENOE

# salario = p6_b2
# edad = eda
# educacion = anios_esc
# experiencia = 
# experiencia^2 = 
lm( ~ edad, educacion, exoe, exp2)
 
typeof(enut$NIV)
enut$NIV = as.character(enut$NIV)
enut$NIV = as.factor(enut$NIV)



enut$EXP = enut$EDAD - 
enut %>% mutate(EXP = ifelse(NIV == "WI" & Name == "John_Smith", "John_Smith1", Name))
  
  
#genero,edad,escolaridad,experiencia, experiencia al cuadradoo
#SEXO,EDAD,,,
#DEPENDIENTE salario
enut$p3_4
lm()


xfill

tab_sal_enoe = data.frame(p6b1=c(1,2,3,4,5,6,7,8),DIVISOR_SALARIO=c(4,2,1,NA,NA,NA,NA,NA))
enoe = left_join(enoe_c1,enoe_c2)

cuid_hog = filter(enoe,p3==5222)
table(p6b1)

cuid_hog %>%
  mutate(sal_sem = case_when(p6b1 == 1 ~ p6b2/4,
                             p6b1 == 2 ~ p6b2/2,
                             p6b1 == 3 ~ p6b2,
                             p6b1 == 4 ~ p6b2*p5d_tdia,
                             p6b1 == 5 ~ NA_real_,
                             p6b1 == 7 ~ NA_real_,
                             p6b1 == 8 ~ NA_real_))



enut$TIEMPO_CUID <- rowSums(enut[,c("P6_15A_1_1","P6_15A_1_3","P6_15A_4_1","P6_15A_4_3","P6_11A_02_1","P6_11A_02_3","P6_11A_03_1","P6_11A_03_3","P6_11A_09_1","P6_11A_09_3","P6_11A_11_1","P6_11A_11_3")],na.rm = TRUE) + rowSums(enut[,c("P6_15A_1_2","P6_15A_1_4","P6_15A_4_2","P6_15A_4_4","P6_11A_02_2","P6_11A_02_4","P6_11A_03_2","P6_11A_03_4","P6_11A_09_2","P6_11A_09_4","P6_11A_11_2","P6_11A_11_4")],na.rm = TRUE)/60
enut$TIEMPO_ENF_TEC <- rowSums(enut[,c("P6_11A_01_1","P6_11A_01_3","P6_11A_05_1","P6_11A_05_3")],na.rm = TRUE) + rowSums(enut[,c("P6_11A_01_2","P6_11A_01_4","P6_11A_05_2","P6_11A_05_4")],na.rm = TRUE)/60
enut$TIEMPO_FISIO <- rowSums(enut[,c("P6_11A_07_1","P6_11A_07_3")],na.rm = TRUE) + rowSums(enut[,c("P6_11A_07_2","P6_11A_07_4")],na.rm = TRUE)/60
enut$TIEMPO_CHOF <- rowSums(enut[,c("P6_15A_2_1","P6_15A_2_3","P6_15A_3_1","P6_15A_3_3","P6_11A_06_1","P6_11A_06_3","P6_11A_08_1","P6_11A_08_3")],na.rm = TRUE) + rowSums(enut[,c("P6_15A_2_2","P6_15A_2_4","P6_15A_3_2","P6_15A_3_4","P6_11A_06_2","P6_11A_06_4","P6_11A_08_2","P6_11A_08_4")],na.rm = TRUE)/60
enut$TIEMPO_COCIN <- rowSums(enut[,c("P6_11A_04_1","P6_11A_04_3")],na.rm = TRUE) + rowSums(enut[,c("P6_11A_04_2","P6_11A_04_4")],na.rm = TRUE)/60
enut$TIEMPO_TOTAL <- rowSums(enut[,c("P6_15A_1_1","P6_15A_1_3","P6_15A_2_1","P6_15A_2_3","P6_15A_3_1","P6_15A_3_3","P6_15A_4_1","P6_15A_4_3","P6_11A_01_1","P6_11A_01_3","P6_11A_02_1","P6_11A_02_3","P6_11A_03_1","P6_11A_03_3","P6_11A_04_1","P6_11A_04_3","P6_11A_05_1","P6_11A_05_3","P6_11A_06_1","P6_11A_06_3","P6_11A_07_1","P6_11A_07_3","P6_11A_08_1","P6_11A_08_3","P6_11A_09_1","P6_11A_09_3","P6_11A_11_1","P6_11A_11_3")], na.rm=TRUE) + rowSums(enut[,c("P6_15A_1_2","P6_15A_1_4","P6_15A_2_2","P6_15A_2_4","P6_15A_3_2","P6_15A_3_4","P6_15A_4_2","P6_15A_4_4","P6_11A_01_2","P6_11A_01_4","P6_11A_02_2","P6_11A_02_4","P6_11A_03_2","P6_11A_03_4","P6_11A_04_2","P6_11A_04_4","P6_11A_05_2","P6_11A_05_4","P6_11A_06_2","P6_11A_06_4","P6_11A_07_2","P6_11A_07_4","P6_11A_08_2","P6_11A_08_4","P6_11A_09_2","P6_11A_09_4","P6_11A_11_2","P6_11A_11_4")], na.rm = TRUE)/60
attach(enut)

lm()
