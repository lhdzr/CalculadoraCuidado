library(faraway)
library(tidyverse)
library(skimr)
library(DataExplorer)
library(scales)
library(corrr)
library(glmnet)
library(pls)
library(olsrr)
library(cluster)




###########################


bd <- read.csv("products/csv/bd.csv")


bd = bd %>% 
  mutate(SEXO=as.factor(SEXO),
         ENT=as.factor(ENT),
         urbano=as.factor(urbano),
         SIT_CONYUGAL=as.factor(SIT_CONYUGAL),
         INDIGENA=as.factor(INDIGENA),
         TIPO_TRABAJO=as.factor(TIPO_TRABAJO),
         PROGRAMA_SOCIAL=as.factor(PROGRAMA_SOCIAL),
         SEGURO_MEDICO=as.factor(SEGURO_MEDICO),
         PAREN=as.factor(PAREN),
         jefatura_femenina=as.factor(jefatura_femenina))


bd <- bd[which(bd$SAL_SEM>0),]



### Metodo de imputacion
set.seed(10)
bd_mice <- mice::mice(bd)
bd_mice <- mice::complete(bd_mice)


bd_mice_val <- bd_mice
datos <- bd_mice_val



plot(sort(datos$SAL_SEM))
hist(datos$SAL_SEM, breaks = 100)

###########################
### Relacion entre variables
###########################

# El primer paso a la hora de establecer un modelo lineal múltiple es estudiar la 
# relación que existe entre variables. Esta información es crítica a la hora de identificar 
# cuáles pueden ser los mejores predictores para el modelo, y para detectar colinealidad entre 
# predictores. A modo complementario, es recomendable representar la distribución de 
# cada variable mediante histogramas.

###########################
### MODELOS
###########################

# Se ajustan varios modelos lineales con y sin regularización, con el objetivo de 
# identificar cuál de ellos es capaz de predecir mejor el contenido en grasa de la 
# carne en función de las señales registradas por el espectrofotómetro. Para poder 
# evaluar la capacidad predictiva de cada modelo, se dividen las observaciones 
# disponibles en dos grupos: uno de entrenamiento (70%) y otro de test (30%).


###
### Definicion de formula
###

# Creación y entrenamiento del modelo
# ==============================================================================

par_var_x <- colnames(datos)[-c(22, grep("ENT", colnames(datos)))]
### GRID DE COMBINACIONES DE VARIABLES REGRESORAS
ncols_vars_x <- length(par_var_x)
l <- rep(list(0:1), ncols_vars_x)
grid_vars_x <- expand.grid(l)
id_elim <- which(apply(grid_vars_x, 1, FUN=sum)==0)
grid_vars_x <- grid_vars_x[-id_elim,]
id_pares <- which(apply(grid_vars_x, 1, FUN=sum)<=2)
grid_vars_x <- grid_vars_x[id_pares,]


###
### TRANSFORMACION DE LA VARIABLE Y A LOG(Y)
###

par_y_log=T
if(par_y_log){
  y_formula <- paste("log(",colnames(datos)[22], ")", sep="")
}else{
  y_formula <- paste(colnames(datos)[22])
}

x_formula <- paste(colnames(datos)[-c(22, grep("ENT", colnames(datos)))], collapse="+")

###
### LOOP ENTIDADES
###

mse_res <- list()
x_formula_combinacion_i_ls <- list()
# unique_entidades <- sort(as.numeric(unique(datos$ent)))
j=1
for(j in 1:32){
  
  print(c(j, 32))
  
  # División de los datos en train y test

  id_train <- sample(1:nrow(datos), size = 0.7*nrow(datos), replace = FALSE)
  
  datos_train_0 <- datos[id_train, ]
  id_ent_j_train <- which(datos_train_0$ENT==j)
  dim(datos_train_0)
  datos_test_0  <- datos[-id_train, ]
  id_ent_j_test <- which(datos_test_0$ENT==j)
  dim(datos_test_0)
  
  datos_train <- datos_train_0[id_ent_j_train,-grep("ENT", colnames(datos))]
  dim(datos_train)
  datos_test <- datos_test_0[id_ent_j_test,-grep("ENT", colnames(datos))]
  dim(datos_test)
  # dim(datos_test)
  
  ###
  ### LOOP INTERACCIONES ORDEN 2
  ###
  x_formula_combinacion_i_ls <- list()
  i=1
  for(i in 1:nrow(grid_vars_x)){
    
    ###
    ### SE CONSTRUYE LA FORMULA
    ###
    
    # print(c(i, nrow(grid_vars_x)))
    id_cols_i <- which(grid_vars_x[i,]==1)
    if(length(id_cols_i)==1){
      x_formula_combinacion_i <- paste(par_var_x[id_cols_i], par_var_x[id_cols_i],sep="*")
    }else{
      x_formula_combinacion_i <- paste(par_var_x[id_cols_i], collapse="*")
    }
    
    x_formula_combinacion_i_ls[[i]] <- x_formula_combinacion_i
    
  }
  
  x_formula_i <- paste(x_formula, x_formula_combinacion_i, sep="+")
  # formula_i <-as.formula(paste(y_formula,x_formula_i, sep="~"))
  
  x_formula_combinaciones <- paste(x_formula, paste(unlist(x_formula_combinacion_i_ls), collapse="+"), sep="+")
  formula_i <-as.formula(paste(y_formula,x_formula_combinaciones, sep="~"))
  
  
  ###
  ### METODO: RIDGE
  ###
  
  modelo_ridge <- function(formula_i, datos_train, datos_test){
    
    ### lista donde se guardaran klos resultdos
    results_ls <- list()
    
    # El paquete glmnet incorpora toda una serie de funcionalidades para entrenar modelos 
    # lineales (regresión y clasificación) con regularización Ridge, Lasso y Elastic Net. 
    # La función glmnet() empleada para entrenar los modelos no permite utilizar formulas ~, 
    # necesita una matriz x con el valor de los predictores y un vector y con la variable respuesta. 
    # Estas matrices pueden crearse de forma rápida con la función model.matrix(), identificando 
    # los predictores y generando las variables dummy necesarias en caso de que los predictores 
    # sean cualitativos.
    
    # El objeto devuelto por la función glmnet() contiene toda la información relevante 
    # del modelo entrenado. Con las funciones plot(), print(), coef() y predict() se puede 
    # extraer su información de forma eficiente. Para conocer en más detalle este potente 
    # paquete visitar glmnet.
    
    # Variables y
    # ==============================================================================
    if(par_y_log){
      y_train <- (datos_train$SAL_SEM)
      y_test <- (datos_test$SAL_SEM)
      
    }
    
    # Matrices de entrenamiento y test
    # ==============================================================================
    x_train <- model.matrix((SAL_SEM)~., data = datos_train)[, -1]
    
    x_test <- model.matrix((SAL_SEM)~., data = datos_test)[, -1]
    
    # Creación y entrenamiento del modelo
    # ==============================================================================
    # Para obtener un ajuste con regularización Ridge se indica argumento alpha=0.
    # Si no se especifica valor de lambda, se selecciona un rango automático.
    modelo <- glmnet(
      x           = x_train,
      y           = y_train,
      alpha       = 0,
      nlambda     = 100,
      standardize = TRUE
    )
    
    # glmnet() almacena en una matriz el valor de los coeficientes de regresión para 
    # cada valor de lambda. Esto permite acceder, mediante la función coef(), a los 
    # coeficientes obtenidos para un determinado valor de lambda (que haya sido incluido 
    # en el rango cuando se han generado los modelos). También permite representar la 
    # evolución de los coeficientes a medida que se incrementa lambda.
    
    # Evolución de los coeficientes en función de lambda
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
        values_to = "coeficientes"
      )
    
    regularizacion %>%
      ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
      geom_line() +
      scale_x_log10(
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
      ) +
      labs(title = "Coeficientes del modelo en función de la regularización") +
      theme_bw() +
      theme(legend.position = "none")
    
    # Puede verse como, a medida que aumenta el valor de lambda, la regularización es 
    # mayor y el valor de los coeficientes se reduce.Para identificar el valor de lambda 
    # que da lugar al mejor modelo, se puede recurrir a validación cruzada con la 
    # función cv.glmnet().
    
    # Evolución del error en función de lambda
    # ==============================================================================
    set.seed(123)
    cv_error <- cv.glmnet(
      x      = x_train,
      y      = y_train,
      alpha  = 0,
      nfolds = 10,
      type.measure = "mse",
      standardize  = TRUE
    )
    
    plot(cv_error)
    
    # El gráfico muestra el Mean Square Error obtenido por validación cruzada para 
    # cada valor de lambda junto con la barra de error correspondiente. Entre la 
    # información almacenada en el objeto cv.glmnet se encuentra el valor de lambda 
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
    
    # Se entrena de nuevo el modelo, esta vez empleando el mayor valor de lambda cuyo 
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
    
    # Coeficientes del modelo
    # ==============================================================================
    df_coeficientes <- coef(modelo) %>%
      as.matrix() %>%
      as_tibble(rownames = "predictor") %>%
      rename(coeficiente = s0)
    
    df_coeficientes %>%
      filter(predictor != "(Intercept)") %>%
      ggplot(aes(x = predictor, y = coeficiente)) +
      geom_col() +
      labs(title = "Coeficientes del modelo Ridge") +
      theme_bw() +
      theme(axis.text.x = element_text(size = 6, angle = 45))
    
    # Predicciones de entrenamiento
    # ==============================================================================
    predicciones_train <- predict(modelo, newx = x_train)
    
    # MSE de entrenamiento
    # ==============================================================================
    training_mse <- mean((predicciones_train - y_train)^2)
    paste("Error (mse) de entrenamiento:", training_mse)
    
    ## [1] "Error (mse) de entrenamiento: 27.6682804462286"
    
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
    results_ls$summary_i <- summary_i
    
    
    return(results_ls)
    
  }
  
  ###
  ### 6) COMPARACION DE RESULTADOS
  ###
  
  # df_comparacion <- data.frame(
  #   ent=j,
  #   modelo = "Ridge",
  #   mse    = modelo_ridge_results$test_mse_ols
  # )
  
  #mse_res[[j]] <- df_comparacion
  
  # ggplot(data = df_comparacion[,-1], aes(x = modelo, y = mse)) +
  #   geom_col(width = 0.5) +
  #   geom_text(aes(label = round(mse, 2)), vjust = -0.1) +
  #   theme_bw() +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
}
