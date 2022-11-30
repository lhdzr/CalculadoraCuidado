setwd("C:/Users/hecto/Documents/Finanzas clase/ChileTextMining/products/csv")
getwd()
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

###########################
### Parametros globales
###########################

par_seed <- 11
par_y_log =T
 
# ###########################
# ### Datos
# ###########################
# 
bd <- read.csv("bd.csv")
# 
# bd <- bd[sample(1:nrow(bd), 0.7*nrow(bd), F),]
# 
# REORDENAMIENTO DE LAS COLUMNAS
id_y <- grep("SAL_SEM", colnames(bd))
y_name <- colnames(bd)[id_y]
bd <- data.frame(bd[,id_y], bd[,-id_y])
colnames(bd)[1] <- y_name

# # head(bd)


# ## se le da formato a las variables categorias
bd$SEXO<- as.factor(bd$SEXO)
bd$urbano <- as.numeric(bd$urbano)
bd$INDIGENA <- as.factor(bd$INDIGENA)
bd$PROGRAMA_SOCIAL <- as.factor(bd$PROGRAMA_SOCIAL)
bd$SEGURO_MEDICO <- as.factor(bd$SEGURO_MEDICO)
bd$PAREN <- as.factor(bd$PAREN)
bd$jefatura_femenina <- as.factor(bd$jefatura_femenina)
# 
# # ### se corrige el nombre de la columna de Entidad
colnames(bd)[which(names(bd)=="ENT")] <- "ent"

# 
# ### restriccion cuando no varia la informacion de una columna/variable
# ### se consideran los valores mayores a 0
# # length(which(bd$ing_x_hrs>0))
# bd <- bd[which(bd$ing_x_hrs>0),]
bd <- bd[which(bd$SAL_SEM>0),]

# # colnames(bd)
# 
# # ### TRANSFORMACION DE LAS VARIABLES NUMERICAS se estandarizan las variables numericas (OPCIONAL)
# # classes <- unlist(lapply(bd, class))
# # id_numeric <- which(classes=="numeric")
# # bd[,id_numeric] <- lapply(bd[,id_numeric], FUN=function(x){as.numeric((x - min(x)) / (max(x) - min(x)))})
# 
# ### Metodo de imputacion
bd_mice <- mice::mice(bd)
bd_mice <- mice::complete(bd_mice)
# 
# # ## *OPCIONAL* selecciono unicamENTe los cuantiles adecuados para evitar sesgos
# # quantiles_v <- quantile(bd_mice$Ingreso_mensual, probs = seq(0, 1, 0.2))
# # id_quant_val <- which(bd_mice$Ingreso_mensual>=quantiles_v[2] & bd_mice$Ingreso_mensual<=quantiles_v[5])
# # id_quant_val <- which(bd_mice$Ingreso_mensual<=quantiles_v[5])
# # bd_mice_val <- bd_mice[id_quant_val,]
# 
# # bd_mice_val <- bd_mice
# # datos <- bd_mice_val

datos <- bd_mice
datos <- datos[,-grep("PROGRAMA_SOCIAL", colnames(datos))]
colnames(datos)

save(datos, file="datos_eq3.R")

## SE DESCARGA LA BASE DE DATOS CON EL PRE-PROCESAMIENTO
load("datos_eq3.R")

## estandarizacion de la variable de respuesta para la evaluacion del MSE 
# bd_mice$Ingreso_mensual <- scale(bd_mice$Ingreso_mensual)
# summary(bd_mice$Ingreso_mensual)

# ## se comprueba la distribucion del salario
# plot(sort(datos$Ingreso_mensual))
# hist(datos$Ingreso_mensual, breaks = 100)

###########################
### Relacion ENTre variables
###########################

# El primer paso a la hora de establecer un modelo lineal múltiple es estudiar la 
# relación que existe ENTre variables. Esta información es crítica a la hora de idENTificar 
# cuáles pueden ser los mejores predictores para el modelo, y para detectar colinealidad ENTre 
# predictores. A modo complemENTario, es recomendable represENTar la distribución de 
# cada variable mediante histogramas.

###########################
### MODELOS
###########################

###
### Definicion de formula
###

# Creación y ENTrenamiENTo del modelo
# ==============================================================================

par_var_x <- colnames(datos)[-c(1, grep("ent", colnames(datos)))]

# ### GRID DE COMBINACIONES DE VARIABLES REGRESORAS
ncols_vars_x <- length(par_var_x)
l <- rep(list(0:1), ncols_vars_x)
grid_vars_x <- expand.grid(l)
id_elim <- which(apply(grid_vars_x, 1, FUN=sum)==0)
grid_vars_x <- grid_vars_x[-id_elim,]
id_pares <- which(apply(grid_vars_x, 1, FUN=sum)<=2)
grid_vars_x <- grid_vars_x[id_pares,]

###
### MODELOS PREDICTIVOS
###

## LOOP de ENTidades
fn_modelado <- function(datos, 
                        par_interacciones=F, 
                        par_sample_h20=0.5,
                        par_y_log=T,
                        par_vars_x_seleccion_step=T,
                        par_h2o_max_runtime_secs=5){
# 
  # ## @ PARAMETROS
  # par_interacciones=F
  # par_sample_h20 <- 0.3
  # par_y_log=T
  # par_vars_x_seleccion_step=T
  # par_h2o_max_runtime_secs=30
  
  ## listas donde se guardaran los resultados: MSE,R2 y modelos
  results_ls <- list()
  results_ls$r2 <- list()
  results_ls$mse <- list()
  results_ls$modelos <- list()
  results_ls$bd <- list()
  
  ###
  ### TRANSFORMACION DE LA VARIABLE Y A LOG(Y)
  ###
  
  if(par_y_log){
    y_formula <- paste("log(",colnames(datos)[1], ")", sep="")
  }else{
    y_formula <- paste(colnames(datos)[1])
  }
  x_formula <- paste(colnames(datos)[-c(1, grep("ent", colnames(datos)))], collapse="+")
  
  
  j=1
  for(j in 1:32){
    
    print(c(j, 32))
    
    # Se filtra segun ENTidad
    # ==============================================================================
    id_ENT_j <- which(datos$ent==j)
    datos_ENT_j <- datos[id_ENT_j,]
    
    # lapply(datos_ENT_j, FUN=summary)
    
    # SE ELIMINANA LOS NIVELES QUE TIENEN MUY POCAS MUESTRAS (<5)
    # ==============================================================================
    
    
    id_cols_factor <- which(lapply(datos_ENT_j, class)=="factor")
    
    rows_elim_ls <- list()
    k=1
    for(k in 1:ncol(datos_ENT_j[,id_cols_factor])){
      
      print(c(k, ncol(datos_ENT_j[,id_cols_factor])))
      table_Entidad_nacimiento <- table(datos_ENT_j[,id_cols_factor][,k])
      id_elim <- which(table_Entidad_nacimiento<15)
      
      if(length(id_elim)>0){
        oficio_elim <- names(table_Entidad_nacimiento)[id_elim]
        id_elim <- list()
        y=1
        for(y in 1:length(oficio_elim)){
          
          id_elim[[y]] <- which(oficio_elim[y]==as.character(datos_ENT_j[,id_cols_factor][,k]))
        }
        rows_elim_ls[[k]] <- unlist(id_elim)
      }
      
    }
    
    id_elim_factores_pequenios <- unlist(rows_elim_ls)
    
    if(length(id_elim_factores_pequenios)>0){
      datos_ENT_j <- datos_ENT_j[-id_elim_factores_pequenios,]
    }
    
    # División de los datos en train y test
    # ==============================================================================
    
    set.seed(par_seed)
    id_train <- sample(1:nrow(datos_ENT_j), size = 0.7*nrow(datos_ENT_j), replace = FALSE)
    
    datos_train_0 <- datos_ENT_j[id_train, ]
    dim(datos_train_0)
    datos_train <- datos_train_0[,-grep("ent", colnames(datos_train_0))]
    
    datos_test_0 <- datos_ENT_j[-id_train, ]
    dim(datos_test_0)
    datos_test <- datos_test_0[,-grep("ent", colnames(datos_test_0))]
    
    ###
    ### SE CONSTRUYE LA FORMULA DE ORDEN 1
    ###
    par_var_x <- colnames(datos)[-c(id_y, grep("ent", colnames(datos)))]
    
    ### formula sin interacciones
    x_formula <- paste(x_formula, collapse=" + ")
    formula_i <-as.formula(paste(y_formula,x_formula, sep="~"))
    formula_i_global <<- formula_i
      
    ###
    ### 1) METODO: Mínimos cuadrados (OLS) - SOLO COMO UN MODELO BASICO DE REFERENCIA
    ###
    
    modelo_ols <- function(formula_i, 
                           datos_train, 
                           datos_test){
      
      ### lista donde se guardaran klos resultdos
      results_ls <- list()
      
      # Creación y ENTrenamiENTo del modelo
      # ==============================================================================
      modelo <- try(lm(formula_i, data = datos_train), silent=T)
      # table(datos_train$Oficio)
      # table(datos_test$Oficio)
      
      if(class(modelo)!="try-error"){
        # modelo <- lm(log(Ingreso_mensual) ~ ., data = datos_train)
        # summary(modelo)
        summary_i <- summary(modelo)
      }else{
        next
      }
      
      summary_i <- summary(modelo)
      print(paste("ENT",j, "ols ","R2adj=", summary_i$adj.r.squared, sep=" "))
      
      # CoeficiENTes del modelo
      # ==============================================================================
      df_coeficiENTes <- modelo$coefficients %>%
        enframe(name = "predictor", value = "coeficiENTe")
      
      df_coeficiENTes %>%
        filter(predictor != "(Intercept)") %>%
        ggplot(aes(x = predictor, y = coeficiENTe)) +
        geom_col() +
        labs(title = "CoeficiENTes del modelo OLS") +
        theme_bw() +
        theme(axis.text.x = element_text(size = 5, angle = 45))
      
      ## -> INTERPRETACIÓN: El valor R2ajustado obtenido es muy alto (0.9967) lo que 
      # indica que el modelo es capaz de predecir con gran exactitud el contenido en grasa 
      # de las observaciones con las que se ha ENTrenado. El hecho de que el modelo en 
      # conjunto sea significativo (p-value: < 2.2e-16), pero que muy pocos de los 
      # predictores lo sean a nivel individual, es indicativo de una posible redundancia 
      # ENTre los predictores (colinealidad).
      
      ## MSE
      # ¿Qué tan bueno es el modelo prediciendo nuevas observaciones que no han participado 
      # en el ajuste? Al tratarse de un modelo de regresión, se emplea como métrica el 
      # Mean Square Error (MSE).
      
      
      # Se transforma Y en caso de haberlo elegido
      # ==============================================================================
      
      if(par_y_log){
        datos_train_ <- datos_train
        datos_test_ <- datos_test
        datos_train_[,1] <- log(datos_train_[,1])
        datos_test_[,1] <- log(datos_test_[,1])
      }
      
      
      # Predicciones de ENTrenamiENTo
      # ==============================================================================
      
      predicciones_train <- predict(modelo, newdata = datos_train_)
      # datos_train_$Oficio
      
      # MSE de ENTrenamiENTo
      # ==============================================================================
      training_mse <- mean((predicciones_train - datos_train_[,1])^2)
      paste("Error (mse) de ENTrenamiENTo:", training_mse)
      
      # Predicciones de test
      # ==============================================================================
      # dim(datos_test_)
      # dim(datos_train_)
      predicciones_test <- predict(modelo, newdata = datos_test_)
      # datos_train_$Entidad_nacimiento
      
      # MSE de test
      # ==============================================================================
      # length(predicciones_test)
      # length(datos_test_[,1])
      test_mse_ols <- mean((predicciones_test - datos_test_[,1])^2)
      paste("Error (mse) de test:", test_mse_ols)
      
      ## -> INTERPRETACIÓN: El modelo tiene un MSE muy alto (0.4740585) cuando predice 
      # las mismas observaciones con las que se ha ENTrenado, pero mucho más bajo (19.4475566) 
      # al predecir nuevas observaciones. Esto significa que el modelo no es útil, ya que 
      # el objetivo es aplicarlo para predecir el contenido en grasa de futuras muestras 
      # de carne. Una de las causas por las 
      # que un modelo puede sufrir overfitting es la incorporación de predictores innecesarios, 
      # que no aportan información o que la información que aportan es redundante.
      
      
      results_ls$modelo <- modelo
      results_ls$training_mse <- training_mse
      results_ls$test_mse_ols <- test_mse_ols
      results_ls$summary_i <- summary_i
      results_ls$adj.r.squared <- summary_i$adj.r.squared
      
      
      return(results_ls)
      
    }
    modelo_ols_results <- modelo_ols(formula_i, datos_train, datos_test)
    modelo_ols_mse <- modelo_ols_results$test_mse_ols
    modelo_ols_r2 <- modelo_ols_results$adj.r.squared
    modelo_ols_coef <- modelo_ols_results$summary_i$coefficients
    
    ###
    ### 2) METODO: Stepwise Selection. Para seleccionar las variables mas significativas en un modelo de primer orden
    ###
    modelo_step <- function(formula_i, 
                            datos_train, 
                            datos_test){
      
      ### lista donde se guardaran klos resultdos
      results_ls <- list()
      
      ###
      ### 2) METODO: Stepwise Selection
      ###
      
      # La función step() de paquete stats permite aplicar el proceso de stepwise selection 
      # (both, backward, forward) y seleccionar el mejor modelo en base al AIC.
      
      # Creación y ENTrenamiENTo del modelo
      # ==============================================================================
      modelo <- step(
        object    = lm(formula = formula_i, data = datos_train),
        direction = "backward",
        scope     = list(upper = ~., lower = ~1),
        trace     = FALSE
      )
      # length(attr(modelo$terms, "term.labels"))
      # dim(datos_train)

      # dim(datos_train)
      summary_i <- summary(modelo)
      print(paste("ENT",j, "stepwise ","R2adj=", summary_i$adj.r.squared, sep=" "))
      
      # CoeficiENTes del modelo
      # ==============================================================================
      df_coeficiENTes <- modelo$coefficients %>%
        enframe(name = "predictor", value = "coeficiENTe")
      
      df_coeficiENTes %>%
        filter(predictor != "(Intercept)") %>%
        ggplot(aes(x = predictor, y = coeficiENTe)) +
        geom_col() +
        labs(title = "CoeficiENTes del modelo Stepwise") +
        theme_bw() +
        theme(axis.text.x = element_text(size = 6, angle = 45))
      
      paste("Número de predictores incluidos en el modelo:", length(modelo$coefficients))
      
      # Se transforma Y en caso de haberlo elegido
      # ==============================================================================
      
      if(par_y_log){
        datos_train_ <- datos_train
        datos_test_ <- datos_test
        datos_train_[,1] <- log(datos_train_[,1])
        datos_test_[,1] <- log(datos_test_[,1])
      }
      
      # Predicciones de ENTrenamiENTo
      # ==============================================================================
      predicciones_train <- predict(modelo, newdata = datos_train_)
      
      # MSE de ENTrenamiENTo
      # ==============================================================================
      training_mse <- mean((predicciones_train - datos_train_[,1])^2)
      paste("Error (mse) de ENTrenamiENTo:", training_mse)
      
      # Predicciones de test
      # ==============================================================================
      # View(datos_test_)
      # dim(datos_test_)
      # length(predicciones_train)
      predicciones_test <- predict(modelo, newdata = datos_test_)
      
      # MSE de test
      # ==============================================================================
      test_mse_step <- mean((predicciones_test - datos_test_[,1])^2)
      paste("Error (mse) de test:", test_mse_step)
      
      ## -> INTERPRETACIÓN: El proceso de stepwise selection devuelve como mejor modelo 
      # el formado por 69 de los 100 predictores disponibles. Al haber eliminado predictores 
      # del modelo, el error de ENTrenamiENTo siempre aumENTa.
      
      
      results_ls$modelo <- modelo
      results_ls$test_mse_ols <- test_mse_step
      results_ls$training_mse <- training_mse
      results_ls$summary_i <- summary_i
      
      
      return(results_ls)
      
    }
    modelo_step_results <- modelo_step(formula_i, datos_train, datos_test)
    modelo_step_mse <- modelo_step_results$test_mse_ols
    modelo_step_r2 <- modelo_step_results$summary_i$adj.r.squared
    modelo_step_coef <- modelo_step_results$summary_i$coefficients
    
    ###
    ### 3) Si se desea, se pueden generar un listado de terminos cuadraticos (interacciones de variables) basado unicamente en las variables seleccionadas en Step.
    ###
    if(par_vars_x_seleccion_step){
      
      par_vars_select <- c(colnames(datos_train)[1], c((attr(modelo_step_results$modelo$terms, "term.labels"))))
      par_var_x_select <- attr(modelo_step_results$modelo$terms, "term.labels")
      
      if(par_interacciones){
        
        x_formula <- paste(par_var_x_select, collapse="+")
        
        ### GRID DE COMBINACIONES DE VARIABLES REGRESORAS
        ncols_vars_x <- length(par_var_x_select)
        l <- rep(list(0:1), ncols_vars_x)
        grid_vars_x <- expand.grid(l)
        id_elim <- which(apply(grid_vars_x, 1, FUN=sum)==0)
        grid_vars_x <- grid_vars_x[-id_elim,]
        id_pares <- which(apply(grid_vars_x, 1, FUN=sum)<=2)
        grid_vars_x <- grid_vars_x[id_pares,]
        
        x_formula_combinacion_i_ls <- list()
        i=1
        for(i in 1:nrow(grid_vars_x)){
          
          # print(c(i, nrow(grid_vars_x)))
          id_cols_i <- which(grid_vars_x[i,]==1)
          if(length(id_cols_i)==1){
            x_formula_combinacion_i <- paste(par_var_x_select[id_cols_i], par_var_x_select[id_cols_i],sep="*")
          }else{
            x_formula_combinacion_i <- paste(par_var_x_select[id_cols_i], collapse="*")
          }
          
          x_formula_combinacion_i_ls[[i]] <- x_formula_combinacion_i
          
        }
        x_formula_combinaciones <- paste(x_formula, paste(unlist(x_formula_combinacion_i_ls), collapse="+"), sep="+")
        formula_i_2 <-as.formula(paste(y_formula,x_formula_combinaciones, sep="~"))
        
      }else{
        x_formula <- paste(par_var_x_select, collapse="+")
        formula_i_2 <-as.formula(paste(y_formula,paste(x_formula, collapse="+"), sep="~"))
        
      }
    }else{
      par_vars_select <- colnames(datos_train)
      formula_i_2 <- formula_i
    }
  
    ###
    ### 3) METODO: Ridge
    ###
    modelo_ridge <- function(formula_i, 
                             datos_train, 
                             datos_test){
      
      ### lista donde se guardaran klos resultdos
      results_ls <- list()
      
      # El paquete glmnet incorpora toda una serie de funcionalidades para ENTrenar modelos 
      # lineales (regresión y clasificación) con regularización Ridge, Lasso y Elastic Net. 
      # La función glmnet() empleada para ENTrenar los modelos no permite utilizar formulas ~, 
      # necesita una matriz x con el valor de los predictores y un vector y con la variable respuesta. 
      # Estas matrices pueden crearse de forma rápida con la función model.matrix(), idENTificando 
      # los predictores y generando las variables dummy necesarias en caso de que los predictores 
      # sean cualitativos.
      
      # El objeto devuelto por la función glmnet() contiene toda la información relevante 
      # del modelo ENTrenado. Con las funciones plot(), print(), coef() y predict() se puede 
      # extraer su información de forma eficiENTe. Para conocer en más detalle este potENTe 
      # paquete visitar glmnet.
      
      # Variables y
      # ==============================================================================
      if(par_y_log){
        y_train <- log(datos_train[,1])
        y_test <- log(datos_test[,1])
      }else{
        y_train <- (datos_train[,1])
        y_test <- (datos_test[,1])
      }
      
      # Matrices de ENTrenamiENTo y test
      # ==============================================================================
      colnames(datos_train)
      x_train <- model.matrix(formula_i, data = datos_train)[, -1]
      x_test <- model.matrix(formula_i, data = datos_test)[, -1]
      
      # Creación y ENTrenamiENTo del modelo
      # ==============================================================================
      # Para obtener un ajuste con regularización Ridge se indica argumENTo alpha=0.
      # Si no se especifica valor de lambda, se selecciona un rango automático.
      modelo <- glmnet(
        x           = x_train,
        y           = y_train,
        alpha       = 0,
        nlambda     = 100,
        standardize = TRUE
      )
      
      # # # sum(modelo$dev.ratio)
      # # fit.lasso$glmnet.fit$dev.ratio
      # cvfit = cv.glmnet(x=x_train,
      #                   y=y_train,
      #                   family = "gaussian",
      #                   type.measure = "deviance")
      # rsq = 1 - cvfit$cvm/var(y_train)
      # a <- plot(cvfit$lambda,rsq)
      # max(rsq)
      
      

      # glmnet() almacena en una matriz el valor de los coeficiENTes de regresión para 
      # cada valor de lambda. Esto permite acceder, mediante la función coef(), a los 
      # coeficiENTes obtenidos para un determinado valor de lambda (que haya sido incluido 
      # en el rango cuando se han generado los modelos). También permite represENTar la 
      # evolución de los coeficiENTes a medida que se incremENTa lambda.
      
      # Evolución de los coeficiENTes en función de lambda
      # ==============================================================================
      regularizacion <- modelo$beta %>% 
        as.matrix() %>%
        t() %>% 
        as_tibble() %>%
        mutate(lambda = modelo$lambda)
      # View(regularizacion)
      
      regularizacion <- regularizacion %>%
        pivot_longer(
          cols = !lambda, 
          names_to = "predictor",
          values_to = "coeficiENTes"
        )
      
      regularizacion %>%
        ggplot(aes(x = lambda, y = coeficiENTes, color = predictor)) +
        geom_line() +
        scale_x_log10(
          breaks = trans_breaks("log10", function(x) 10^x),
          labels = trans_format("log10", math_format(10^.x))
        ) +
        labs(title = "CoeficiENTes del modelo en función de la regularización") +
        theme_bw() +
        theme(legend.position = "none")
      
      # Puede verse como, a medida que aumENTa el valor de lambda, la regularización es 
      # mayor y el valor de los coeficiENTes se reduce.Para idENTificar el valor de lambda 
      # que da lugar al mejor modelo, se puede recurrir a validación cruzada con la 
      # función cv.glmnet().
      
      # Evolución del error en función de lambda
      # ==============================================================================
      set.seed(par_seed)
      cv_error <- cv.glmnet(
        x      = x_train,
        y      = y_train,
        alpha  = 0,
        nfolds = 10,
        type.measure = "mse",
        standardize  = TRUE
      )
      plot(cv_error)
      
      # cvfit <- cv.glmnet(
      #   x      = x_train,
      #   y      = y_train,
      #   alpha  = 0,
      #   nfolds = 10,
      #   family = "gaussian",
      #   type.measure = "deviance"
      # )
      # rsq = 1 - cvfit$cvm/var(y_train)
      # # round(rsq, 4)
      # a <- plot(cvfit$lambda,rsq)
      # max(rsq)
      
      # El gráfico muestra el Mean Square Error obtenido por validación cruzada para 
      # cada valor de lambda junto con la barra de error correspondiENTe. ENTre la 
      # información almacenada en el objeto cv.glmnet se encuENTra el valor de lambda 
      # con el que se consigue el menor error (lambda.min) y el mayor valor de lambda 
      # con el que se consigue el modelo más sencillo que se aleja menos de 1 desviación 
      # estándar del error mínimo encontrado.
      
      # Mejor valor lambda encontrado
      # ==============================================================================
      paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
      
      # Mejor valor lambda encontrado + 1sd
      # ==============================================================================
      # Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
      paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)
      
      # Se ENTrena de nuevo el modelo, esta vez empleando el mayor valor de lambda cuyo 
      # error está a menos de una desviación típica del mínimo encontrado en la validación cruzada.
      
      # Mejor modelo lambda óptimo + 1sd
      # ==============================================================================
      modelo <- glmnet(
        x           = x_train,
        y           = y_train,
        alpha       = 0,
        lambda      = cv_error$lambda.1se,
        standardize = TRUE
      )
      
      summary_i <- summary(modelo)
      
      # CoeficiENTes del modelo
      # ==============================================================================
      df_coeficiENTes <- coef(modelo) %>%
        as.matrix() %>%
        as_tibble(rownames = "predictor") %>%
        rename(coeficiENTe = s0)
      
      df_coeficiENTes %>%
        filter(predictor != "(Intercept)") %>%
        ggplot(aes(x = predictor, y = coeficiENTe)) +
        geom_col() +
        labs(title = "CoeficiENTes del modelo Ridge") +
        theme_bw() +
        theme(axis.text.x = element_text(size = 6, angle = 45))
      
      
      # ESTADISTICAS DE AJUSTE: R2 & RMSE
      # ==============================================================================
      predicciones_train <- predict(modelo, newx = x_train)
      
      eval_results <- function(true, predicted, df) {
        
        # true <- y_train
        # predicted <- predicciones_train
        
        SSE <- sum((predicted - true)^2)
        SST <- sum((true - mean(true))^2)
        # var(true)*(length(true)-1) # equivalente
        R_square <- 1 - (SSE/SST)
        RMSE = sqrt(SSE/nrow(df))
        
        # Model performance metrics
        data.frame(
          RMSE = RMSE,
          Rsquare = R_square
        )
        
      }
      performance_statistics <- eval_results(y_train, predicciones_train, x_train)
      
      # MSE de ENTrenamiENTo
      # ==============================================================================
      training_mse <- mean((predicciones_train - y_train)^2)
      paste("Error (mse) de ENTrenamiENTo:", training_mse)
      
      ## [1] "Error (mse) de ENTrenamiENTo: 27.6682804462286"
      
      # Predicciones de test
      # ==============================================================================
      predicciones_test <- predict(modelo, newx = x_test)
      
      # MSE de test
      # ==============================================================================
      test_mse_ridge <- mean((predicciones_test - y_test)^2)
      paste("Error (mse) de test:", test_mse_ridge)
      
      
      results_ls$modelo <- modelo
      results_ls$test_mse_ridge <- test_mse_ridge
      results_ls$training_mse <- training_mse
      results_ls$coef <- as.data.frame(df_coeficiENTes)
      results_ls$performance_statistics <- performance_statistics
      
      return(results_ls)
      
    }
    modelo_ridge_results <- modelo_ridge(formula_i_2, 
                                         datos_train[,par_vars_select], 
                                         datos_test[,par_vars_select])
    modelo_ridge_mse <- modelo_ridge_results$test_mse_ridge
    modelo_ridge_r2 <- modelo_ridge_results$performance_statistics$Rsquare
    modelo_ridge_coef <- modelo_ridge_results$coef
    
    
    ###
    ### 4) METODO: Lasso
    ###
    
    modelo_lasso <- function(formula_i, 
                             datos_train, 
                             datos_test){
      
      ### lista donde se guardaran klos resultdos
      results_ls <- list()
      
      # El proceso para realizar un ajuste mediante Lasso y la idENTificación del mejor 
      # valor de lambda es equivalENTe al seguido en el caso de Ridge pero indicando en la 
      # función glmnet() que alpha=1.
      
      # Variables y
      # ==============================================================================
      if(par_y_log){
        y_train <- log(datos_train[,1])
        y_test <- log(datos_test[,1])
      }else{
        y_train <- (datos_train[,1])
        y_test <- (datos_test[,1])
      }
      
      # Matrices de ENTrenamiENTo y test
      # ==============================================================================
      x_train <- model.matrix(formula_i, data = datos_train)[, -1]
      x_test <- model.matrix(formula_i, data = datos_test)[, -1]
      
      # Creación y ENTrenamiENTo del modelo
      # ==============================================================================
      # Para obtener un ajuste con regularización Lasso se indica argumENTo alpha=1.
      # Si no se especifica valor de lambda, se selecciona un rango automático.
      modelo <- glmnet(
        x           = x_train,
        y           = y_train,
        alpha       = 1,
        nlambda     = 100,
        standardize = TRUE
      )
      
      # Evolución de los coeficiENTes en función de lambda
      # ==============================================================================
      regularizacion <- modelo$beta %>% 
        as.matrix() %>%
        t() %>% 
        as_tibble() %>%
        mutate(lambda = modelo$lambda)
      
      regularizacion <- regularizacion %>%
        pivot_longer(
          cols = !lambda, 
          names_to = "predictor",
          values_to = "coeficiENTes"
        )
      
      regularizacion %>%
        ggplot(aes(x = lambda, y = coeficiENTes, color = predictor)) +
        geom_line() +
        scale_x_log10(
          breaks = trans_breaks("log10", function(x) 10^x),
          labels = trans_format("log10", math_format(10^.x))
        ) +
        labs(title = "CoeficiENTes del modelo en función de la regularización") +
        theme_bw() +
        theme(legend.position = "none")
      
      # Puede verse como, a medida que aumENTa el valor de lambda, la regularización es 
      # mayor y más predictores quedan excluidos (su coeficiENTe es 0).Para idENTificar 
      # el valor de lambda que da lugar al mejor modelo, se puede recurrir a validación 
      # cruzada con la función cv.glmnet().
      
      # Evolución del error en función de lambda
      # ==============================================================================
      set.seed(par_seed)
      cv_error <- cv.glmnet(
        x      = x_train,
        y      = y_train,
        alpha  = 1,
        nfolds = 10,
        type.measure = "mse",
        standardize  = TRUE
      )
      
      # # sum(modelo$dev.ratio)
      # fit.lasso$glmnet.fit$dev.ratio
      # ?cv.glmnet
      
      cvfit <- cv.glmnet(
        x      = x_train,
        y      = y_train,
        alpha  = 1,
        nfolds = 10,
        family = "gaussian",
        type.measure = "deviance",
        standardize  = TRUE
      )
      rsq = 1 - cvfit$cvm/var(y_train)
      a <- plot(cvfit$lambda,rsq)
      # max(rsq)
      
      plot(cv_error)
      
      # Mejor valor lambda encontrado
      # ==============================================================================
      paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
      
      
      # Mejor valor lambda encontrado + 1sd
      # ==============================================================================
      # Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
      paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)
      
      # Se ENTrena de nuevo el modelo, esta vez empleando el mayor valor de lambda cuyo 
      # error está a menos de una desviación típica del mínimo encontrado en la validación 
      # cruzada.
      
      # Mejor modelo lambda óptimo + 1sd
      # ==============================================================================
      modelo <- glmnet(
        x           = x_train,
        y           = y_train,
        alpha       = 1,
        lambda      = cv_error$lambda.1se,
        standardize = TRUE
      )
      
      summary_i <- summary(modelo)
      
      # CoeficiENTes del modelo
      # ==============================================================================
      df_coeficiENTes <- coef(modelo) %>%
        as.matrix() %>%
        as_tibble(rownames = "predictor") %>%
        rename(coeficiENTe = s0)
      
      df_coeficiENTes %>%
        filter(predictor != "(Intercept)") %>%
        ggplot(aes(x = predictor, y = coeficiENTe)) +
        geom_col() +
        labs(title = "CoeficiENTes del modelo Lasso") +
        theme_bw() +
        theme(axis.text.x = element_text(size = 6, angle = 45))
      
      df_coeficiENTes %>%
        filter(
          predictor != "(Intercept)",
          coeficiENTe != 0
        ) 
      
      # ESTADISTICAS DE AJUSTE: R2 & RMSE
      # ==============================================================================
      predicciones_train <- predict(modelo, newx = x_train)
      
      eval_results <- function(true, predicted, df) {
        
        # y_train <- true
        # predicted <- predicciones_train
        # df <- x_train
        
        
        SSE <- sum((predicted - true)^2)
        SST <- sum((true - mean(true))^2)
        R_square <- 1 - SSE / SST
        RMSE = sqrt(SSE/nrow(df))
        
        
        # Model performance metrics
        data.frame(
          RMSE = RMSE,
          Rsquare = R_square
        )
        
      }
      performance_statistics <- eval_results(y_train, predicciones_train, x_train)
      
      # Predicciones de ENTrenamiENTo
      # ==============================================================================
      predicciones_train <- predict(modelo, newx = x_train)
      
      # MSE de ENTrenamiENTo
      # ==============================================================================
      training_mse <- mean((predicciones_train - y_train)^2)
      paste("Error (mse) de ENTrenamiENTo:", training_mse)
      
      
      # Predicciones de test
      # ==============================================================================
      predicciones_test <- predict(modelo, newx = x_test)
      
      # MSE de test
      # ==============================================================================
      test_mse_lasso <- mean((predicciones_test - y_test)^2)
      paste("Error (mse) de test:", test_mse_lasso)
      
      
      results_ls$modelo <- modelo
      results_ls$test_mse_ols <- test_mse_lasso
      results_ls$training_mse <- training_mse
      results_ls$coef <- as.data.frame(df_coeficiENTes)
      results_ls$performance_statistics <- performance_statistics
      
      return(results_ls)
      
    }
    modelo_lasso_results <- modelo_lasso(formula_i_2, 
                                         datos_train[,par_vars_select], 
                                         datos_test[,par_vars_select])
    modelo_lasso_mse <- modelo_lasso_results$test_mse_ols
    modelo_lasso_r2 <- modelo_lasso_results$performance_statistics$Rsquare
    modelo_lasso_coef <- modelo_lasso_results$coef
    
    # ###
    # ### 5) METODOS DE ML (H2O)
    # ###
    
    ## para pruebas, solo se usa el 5% de los datos de la ENTidad j
    datos_train_h20 <- datos_train[sample(1:nrow(datos_train), (par_sample_h20*nrow(datos_train)), F),par_vars_select]
    datos_test_h20 <- datos_test[sample(1:nrow(datos_test), (par_sample_h20*nrow(datos_test)), F),par_vars_select]
    # dim(datos_train_h20)
    # dim(datos_test_h20)
    fn_h2o <- function(datos__,
                       datos_test__,
                       par_y_type="numeric",
                       par_y_log=F,
                       par_h2o_max_runtime_secs,
                       par_modelo_h2o){
      
      # ###@parametros
      # par_y_type="numeric"
      # datos__ <- datos_train_h20
      # datos_test__ <- datos_test_h20
      # par_h2o_max_runtime_secs=60
      # par_modelo_h2o="xgboost"
      
      ## lista de resultados
      results_ls <- list()
      
      datos_ <- as.h2o(datos__)
      dim(datos_)
      datos_test_ <- as.h2o(datos_test__)
      dim(datos_test_)
      
      # test <- as.h2o(datos_test)
      
      # datos_ <- datos_datos__
      # test <- datos_test_
      
      # IdENTify predictors and response
      y <- colnames(datos_)[1]
      x <- setdiff(names(datos_), y)
      
      # Condicion de logaritmos de variable de respuesta
      if(par_y_log){
        # y <- paste("log(", y, ")",sep="")
        colnames(datos_)[1] <- y
        datos_[,1] <- log(datos_[,1])
        datos_test_[,1] <- log(datos_test_[,1])
      }
      
      ###
      ### MODELOS ESPECIFICOS 
      ###
      
      if(par_modelo_h2o=="randomforest"){
        # ?h2o.randomForest
        m <- h2o.randomForest(x = x,
                                y = y,
                                training_frame = datos_,
                                model_id = "our.rf",
                                seed = 1234)
      }

      if(par_modelo_h2o=="xgboost"){
        # Build and train the model: ## NO FUNCIONA BIEN (REVISAR)
        m <- h2o.xgboost(x = x,
                           y = y,
                          training_frame = datos_,
                          booster = "dart",
                          normalize_type = "tree",
                          seed = 1234)
      }
      
      if(par_modelo_h2o=="neural_network"){
        # Build and train the model:
        m <- h2o.deeplearning(x = x,
                                y = y,
                                distribution = "tweedie",
                                hidden = c(1),
                                epochs = 1000,
                                train_samples_per_iteration = -1,
                                reproducible = TRUE,
                                activation = "Tanh",
                                single_node_mode = FALSE,
                                balance_classes = FALSE,
                                force_load_balance = FALSE,
                                seed = 23123,
                                tweedie_power = 1.5,
                                score_training_samples = 0,
                                score_validation_samples = 0,
                                training_frame = datos_,
                                stopping_rounds = 0)
      }
      
      # 
      # rf_perf2 <- h2o.performance(model = aml, newdata = datos_test_)
      # print(rf_perf2)
      # 
      # predictions <- h2o.predict(aml, datos_test_)
      # test_mse_ols <- mean((predictions - datos_test_[,1])^2)
      # paste("Error (mse) de test:", test_mse_ols)
      
      ###
      ### METODO AUTOMATICO PARA LA SELECCION DEL MEJOR MODELO
      ###
      # Run AutoML for 20 base models
      # ?h2o.automl
      # set.seed(par_seed)
      # aml <- h2o.automl(x = x,
      #                   y = y,
      #                   training_frame = datos_,
      #                   max_models = 20,
      #                   max_runtime_secs=par_h2o_max_runtime_secs,
      #                   seed = 1)
      # # View the AutoML Leaderboard
      # lb <- aml@leaderboard
      # lb
      # lb2 <- aml2@leaderboard
      # lb2
      # print(lb, n = nrow(lb))  # Print all rows instead of default (6 rows)
      
      # # Get leaderboard with all possible columns
      # lb <- h2o.get_leaderboard(object = aml, extra_columns = "ALL")
      # lb
      # lb2 <- h2o.get_leaderboard(object = aml2, extra_columns = "ALL")
      # lb2
      
      ###Examine Models
      
      # Get the best model using the metric
      # m <- aml@leader
      # m2 <- aml2@leader
      
      ### Parametros de la funcions "automl"
      {
        # explain(m)
        # # this is equivalENT to
        # # m <- h2o.get_best_model(aml)
        #
        # # Get the best model using a non-default metric
        # m <- h2o.get_best_model(aml, criterion = "logloss")
        #
        # # Get the best XGBoost model using default sort metric
        # xgb <- h2o.get_best_model(aml, algorithm = "xgboost")
        #
        # # Get the best XGBoost model, ranked by logloss
        # xgb <- h2o.get_best_model(aml, algorithm = "xgboost", criterion = "logloss")
        #
        # # Get a specific model by model ID
        # m <- h2o.getModel("StackedEnsemble_BestOfFamily_AutoML_20191213_174603")
        # # View the non-default parameter values for the XGBoost model above
        # xgb@parameters
        #
        # # Get AutoML evENT log
        # log <- aml@evENT_log
        #
        # # Get datos_ing timing info
        # info <- aml@datos_ing_info
      }
      
      
      rf_perf2 <- h2o.performance(model = m, newdata = datos_test_)
      print(rf_perf2)
      
      predictions <- h2o.predict(m, datos_test_)
      test_mse_ols <- mean((predictions - datos_test_[,1])^2)
      paste("Error (mse) de test:", test_mse_ols)
      
      ## Se calcula el R2
      eval_results <- function(true, predicted, df) {
        
        # @ parametros
        # true=datos_test__[,1]
        # predicted=predictions
        
        SSE <- sum((predicted - true)^2)
        SST <- sum((true - mean(true))^2)
        R_square <- 1 - SSE / SST
        RMSE = sqrt(SSE/nrow(df))
        
        
        # Model performance metrics
        data.frame(
          RMSE = RMSE,
          Rsquare = R_square
        )
      }
      performance_statistics <- eval_results(true=datos_test_[,1], predicted=predictions, df= datos_test_)
      # performance_statistics
 
      
      ### Importancia de variables
      varimp <- h2o.varimp(m)
      
      
      results_ls$modelo <- m
      # results_ls$leaderboard <- lb
      results_ls$rf_perf2 <- rf_perf2
      results_ls$performance_statistics <- performance_statistics
      results_ls$varimp <- varimp
      
      
      return(results_ls)
      
    }
    
    ###
    ### 6) RANDOMFOREST
    ###
    
    fn_h2o_res_randomforest <- fn_h2o(datos__=datos_train_h20,
                         datos_test__=datos_test_h20,
                         par_y_type="numeric",
                         par_y_log=T,
                         par_h2o_max_runtime_secs,
                         par_modelo_h2o="randomforest")
    h2o_randomforest_mse <- fn_h2o_res_randomforest$rf_perf2@metrics$MSE
    h2o_randomforest_r2 <- fn_h2o_res_randomforest$rf_perf2@metrics$r2
    modelo_randomforest_coef <- fn_h2o_res_randomforest$varimp[,c(1,3)]
    
    ###
    ### 7) XGBOOST
    ###
    # 
    # fn_h2o_res_xgboost <- fn_h2o(datos__=datos_train_h20,
    #                      datos_test__=datos_test_h20,
    #                      par_y_type="numeric",
    #                      par_y_log=T,
    #                      par_h2o_max_runtime_secs,
    #                      par_modelo_h2o="xgboost")
    # h2o_xgboost_mse <- fn_h2o_res_xgboost$rf_perf2@metrics$MSE
    # h2o_xgboost_r2 <- fn_h2o_res_xgboost$rf_perf2@metrics$r2
    # modelo_xgboost_coef <- fn_h2o_res_xgboost$varimp
    
    ###
    ### 8) NEURAL NETWORK
    ###
    
    fn_h2o_res_neural_network <-  try(fn_h2o(datos__=datos_train_h20,
                         datos_test__=datos_test_h20,
                         par_y_type="numeric",
                         par_y_log=T,
                         par_h2o_max_runtime_secs,
                         par_modelo_h2o="neural_network"), silent=T)
    if(class(fn_h2o_res_neural_network)!="try-error"){
      h2o_neural_network_mse <-fn_h2o_res_neural_network$rf_perf2@metrics$MSE
      h2o_neural_network_r2 <- fn_h2o_res_neural_network$rf_perf2@metrics$r2
      modelo_neural_network_coef <- fn_h2o_res_neural_network$varimp[,c(1,3)]
      
    }else{
      h2o_neural_network_mse <-NA
      h2o_neural_network_r2 <- NA
      fn_h2o_res_neural_network <- list()
      fn_h2o_res_neural_network$modelo <- NA
      modelo_neural_network_coef <- data.frame(variable=rep(NA, 2), importancia=rep(NA, 2))
      
    }

    
    {
      # h2o_r2 <- (fn_h2o_res$best_model@model$training_metrics@metrics$r2)
      # h2o_r2
      # h2o_mse <- (fn_h2o_res$best_model@model$training_metrics@metrics$MSE)
      # h2o_mse
      # h2o_mae <- (fn_h2o_res$best_model@model$training_metrics@metrics$mae)
    }
    # h2o_model_name <- fn_h2o_res$modelo@algorithm
    # h2o_r2 <- fn_h2o_res$performance_statistics$Rsquare
    # h2o_mse <- fn_h2o_res$rf_perf2@metrics$MSE
    # h2o_r2 <- fn_h2o_res$rf_perf2@metrics$r2
    
    ###
    ### --> COMPARACION DE RESULTADOS
    ###
    
    # df_comparacion <- data.frame(
    #   ENT=j,
    #   modelo = c("OLS", "Stepwise","Lasso", "Ridge",  "PCR","PLS", paste("H2O",h2o_model_name, sep="_")),
    #   mse    = c(modelo_ols_results$test_mse_ols, modelo_step_results$test_mse_ols, modelo_lasso_results$test_mse_ols,
    #              modelo_ridge_results$test_mse_ridge, modelo_pcr_results$test_mse_ols, modelo_pls_results$test_mse_ols, h2o_mse))
    # # View(df_comparacion)
    
    # df_mse_comparacion <- data.frame(
    #   ENT=j,
    #   modelo = c("OLS", "Stepwise","Lasso", "Ridge", "Randomforest", "Xgboost", "Neural_network"),
    #   mse    = c(modelo_ols_results$test_mse_ols, modelo_step_results$test_mse_ols, modelo_lasso_results$test_mse_ols,
    #              modelo_ridge_results$test_mse_ridge,h2o_randomforest_mse,h2o_xgboost_mse, h2o_neural_network_mse))
    
    df_mse_comparacion <- data.frame(
      ENT=j,
      modelo = c("OLS", 
                 "Stepwise",
                 "Lasso", 
                 "Ridge", 
                 "Randomforest", 
                 "Neural_network"),
      mse    = c(modelo_ols_results$test_mse_ols, 
                 modelo_step_results$test_mse_ols, 
                 modelo_lasso_results$test_mse_ols,
                 modelo_ridge_results$test_mse_ridge,
                 h2o_randomforest_mse,
                 h2o_neural_network_mse))
    
    # df_mse_comparacion <- data.frame(
    #   ENT=j,
    #   modelo = c("OLS", "Stepwise","Lasso", "Ridge"),
    #   mse    = c(modelo_ols_results$test_mse_ols, modelo_step_results$test_mse_ols, modelo_lasso_results$test_mse_ols,
    #              modelo_ridge_results$test_mse_ridge))
    
    
    ## se ordena tabla segun tamanio del estadistico
    # df_mse_comparacion <- df_mse_comparacion[order(df_mse_comparacion$mse, decreasing = T),]
    rownames(df_mse_comparacion) <- 1:nrow(df_mse_comparacion)
    
    ## se grafica
    ggplot(data = df_mse_comparacion[,-1], aes(x = modelo, y = mse)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = round(mse, 2)), vjust = -0.1) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # df_r2_comparacion <- data.frame(
    #   ENT=j,
    #   modelo = c("OLS", "Stepwise","Lasso", "Ridge", paste("H2O",h2o_model_name, sep="_")),
    #   r2    = c(modelo_ols_r2, modelo_step_r2, modelo_lasso_r2,
    #              modelo_ridge_r2, h2o_r2))
    
    df_r2_comparacion <- data.frame(
      ENT=j,
      modelo=c("OLS", 
               "Stepwise",
               "Lasso", 
               "Ridge", 
               "Randomforest", 
               "Neural_network"),
      r2    = c(modelo_ols_r2, 
                modelo_step_r2, 
                modelo_lasso_r2,
                modelo_ridge_r2, 
                h2o_randomforest_r2, 
                h2o_neural_network_r2))
    
    # df_r2_comparacion <- data.frame(
    #   ENT=j,
    #   modelo = c("OLS", "Stepwise","Lasso", "Ridge"),
    #   r2    = c(modelo_ols_r2, modelo_step_r2, modelo_lasso_r2,
    #             modelo_ridge_r2))
    
    ## se ordena tabla segun tamanio del estadistico
    # df_r2_comparacion <- df_r2_comparacion[order(df_r2_comparacion$r2, decreasing = T),]
    rownames(df_r2_comparacion) <- 1:nrow(df_r2_comparacion)
    
    ## se grafica
    ggplot(data = df_r2_comparacion[,-1], aes(x = modelo, y = r2)) +
      geom_col(width = 0.5) +
      geom_text(aes(label = round(r2, 2)), vjust = -0.1) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    results_ls$r2[[j]] <- df_r2_comparacion
    results_ls$mse[[j]] <- df_mse_comparacion
    results_ls$modelos[[j]] <- list(modelo_ols_results$modelo, 
                                    modelo_step_results$modelo, 
                                    modelo_lasso_results$modelo,
                                    modelo_ridge_results$modelo, 
                                    fn_h2o_res_randomforest$modelo,
                                    fn_h2o_res_neural_network$modelo)
    results_ls$coef[[j]] <- list(modelo_ols_coef[,1:2], 
                                 modelo_step_coef[,1:2], 
                                 modelo_lasso_coef[,1:2], 
                                 modelo_ridge_coef[,1:2], 
                                 modelo_randomforest_coef[,1:2], 
                                 modelo_neural_network_coef[,1:2])
    # results_ls_modelos$Stepwise[[j]] <- modelo_step_results$modelo
    # results_ls_modelos$Lasso[[j]] <- modelo_lasso_results$modelo
    # results_ls_modelos$Ridge[[j]] <- modelo_ridge_results$modelo
    results_ls$bd[[j]] <- datos_ENT_j
    
  }
  
  return(results_ls)
  
}
fn_modelado_res <- fn_modelado(datos, 
                               par_interacciones=F, 
                               par_sample_h20=0.7,
                               par_y_log=T,
                               par_vars_x_seleccion_step=T,
                               par_h2o_max_runtime_secs=30)

r2_ENTidades <- do.call("rbind", fn_modelado_res$r2)
mse_ENTidades <- do.call("rbind", fn_modelado_res$mse)

## se guarda el objeto con los resultados
save.image("equipo_3_environment.R")
# getwd()

### -- > HASTA AQUI SE CORRE UNA SOLA VEZ
################################################################################




