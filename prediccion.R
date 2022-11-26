################################################################################
### PARAMETROS CAPTURADOS DEL USUARIO
################################################################################

## 1) cargar librerias
# ==============================================================================
library(faraway)
# Gráficos y tratamiENTo de datos
# ==============================================================================
library(tidyverse)
library(skimr)
#devtools::install_github("boxuancui/DataExplorer")
library(DataExplorer)
library(scales)
library(corrr)
# Modelado
# ==============================================================================
library(glmnet)
library(pls)
# Residuales
# ==============================================================================
library(olsrr)
# Residuales
# ==============================================================================
library(cluster) # gower distance
# # ML
# # ==============================================================================
# # install.packages("h2o")
library(h2o)
# # Start the H2O cluster (locally)
h2o.init()

### 2) Descargar los resultados del modelado

load("equipo_3_modelado.R")
# el objeto que contiene todos los resultados es:
# fn_modelado_res

### 3) Capturar datos

## Dataframe de los parametros a recabar
pars_usuario <- data.frame(pars=colnames(datos)[-c(1)], value=NA)
# 1. Ciudad
pars_usuario[1,"value"] <- 1 ## valor capturado de RShiny
# 2. ENT
pars_usuario[2,"value"] <- par_2
# 3. Personas_en_vivienda
pars_usuario[3,"value"] <- par_3
# 4. Rraul
pars_usuario[4,"value"] <- par_4
# 5. Tipo.de.entrevista
pars_usuario[4,"value"] <- par_5
# sucesivamente... hasta 23

### aqui acaba la captura de datos

################################################################################
### -- > A partir de aqui se corre en el sistema
################################################################################

pars_usuario_t <- t(pars_usuario)
names<-pars_usuario_t[1,]
colnames(pars_usuario_t) <- names
pars_usuario_t <- pars_usuario_t[2,,drop=F]
pars_usuario_t <- as.data.frame(pars_usuario_t)

### SE LE DA FORMATO A CADA UNA DE LAS VARIABLES (PARAMETROS) CONSISTETE CON LA BD DE ENTRENAMIENTO
## se le da formato a las variables categorias

pars_usuario_t$SEXO<- as.factor(pars_usuario_t$SEXO)
pars_usuario_t$urbano <- as.numeric(pars_usuario_t$urbano)
pars_usuario_t$INDIGENA <- as.factor(pars_usuario_t$INDIGENA)
pars_usuario_t$PROGRAMA_SOCIAL <- as.factor(pars_usuario_t$PROGRAMA_SOCIAL)
pars_usuario_t$SEGURO_MEDICO <- as.factor(pars_usuario_t$SEGURO_MEDICO)
pars_usuario_t$PAREN <- as.factor(pars_usuario_t$PAREN)
pars_usuario_t$jefatura_femenina <- as.factor(pars_usuario_t$jefatura_femenina)

### PARA FINES DE EJEMPLIFICACION/SIMULACION
## a) formato "normal" para modelos OLS y STEP
id_sample <- sample(1:300, 1, F)
pars_usuario_t <- datos[id_sample,,drop=F] ## debe llenarse con los datos nuevos (parametros del usuario). Para ejemplificar se considera una de las observaciones de la base de datos
# b) Formato necesario al dataframe previo porque asi lo requieren los modelos Ridge y Lasso
pars_usuario_t_lasso_ridge <- model.matrix(formula_i_global, data = datos[id_sample,])[,-1, drop=F]

### PREDICCION
prediccion <- fn_prediction(x_new=pars_usuario_t, 
                            x_new_lasso_ridge=pars_usuario_t_lasso_ridge,
                            datos,
                            par_y_log)
prediccion



