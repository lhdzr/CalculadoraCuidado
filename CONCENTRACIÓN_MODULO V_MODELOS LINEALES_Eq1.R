# Instalacion de librerias
# ==============================================================================
# install.packages("faraway")
# install.packages("tidyverse")
# install.packages("skimr")
# install.packages("DataExplorer")
# install.packages("scales")
# install.packages("corrr")
# install.packages("glmnet")
# install.packages("pls")

# ==============================================================================
library(faraway)

# Gráficos y tratamiento de datos
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

# ###########################
# ### Conversion a variables numericas (coninuizacion del espacio)
# ###########################
# 
# # create nominal variable
# nom <- factor(rep(letters[1:3], each=10))
# # create numeric variables
# vars <- as.matrix(replicate(17, rnorm(30)))
# df <- data.frame(nom, vars)
# 
# # install.packages("cluster")
# library(cluster)
# daisy.mat <- as.matrix(daisy(df, metric="gower"))
# dim(daisy.mat)
# 
# # install.packages("StatMatch")
# library(StatMatch)
# gower.mat <- gower.dist(df)
# 
# #Escalamiento multidimensional
# fit.2 <-  bios2mds::mmds(matrix.gower, pc = 5)
# scree.plot(fit.2$eigen.perc, lab = TRUE, title = "Scree plot of metric MDS")


###########################
### Datos
###########################

# El set de datos contiene 101 columnas. Las 100 primeras, nombradas como V1 , …, V100 
# recogen el valor de absorbencia para cada una de las 100 longitudes de onda analizadas (predictores), 
# y la columna ing_x_hrs el contenido en grasa medido por técnicas químicas (variable respuesta).
# ==============================================================================
# data("meatspec")
# datos <- meatspec
# head(datos,3)

bd <- read.csv("products/csv/bd.csv")

## se elimina la priemra columa con el index


## se quita la columna de ent, pues se correra un modelo para cada una
# ent_v <- bd$ent
# plot(table(ent_v))
# bd <- bd[,-c(grep("ent", colnames(bd)))]

## se le da formato a las variables categorias
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

### se consideran los valores mayores a 0
bd <- bd[which(bd$SAL_SEM>0),]

# head(bd)
# summary(bd)
# bd %>% mutate(dummy=1) %>%
#   spread(key=bd,value=dummy, fill=0) %>% slice(1:5)

### Metodo de imputacion
set.seed(10)
bd_mice <- mice::mice(bd)
bd_mice <- mice::complete(bd_mice)
# table(bd_mice$urbano)
# table(bd_mice$programa_social)

# ## selecciono unicamente los cuantiles adecuados para evitar sesgos
# quantiles_v <- quantile(bd_mice$ing_x_hrs, probs = seq(0, 1, 0.2))
# id_quant_val <- which(bd_mice$ing_x_hrs>=quantiles_v[2] & bd_mice$ing_x_hrs<=quantiles_v[5])
# id_quant_val <- which(bd_mice$ing_x_hrs<=quantiles_v[5])
# bd_mice_val <- bd_mice[id_quant_val,]

bd_mice_val <- bd_mice
datos <- bd_mice_val
## estandarizacion de la variable de respuesta para la evaluacion del MSE 
# bd_mice$ing_x_hrs <- scale(bd_mice$ing_x_hrs)
# summary(bd_mice$ing_x_hrs)

## se comprueba la distribucion del salario
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
  # ==============================================================================
  
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
  ### 1) METODO: Mínimos cuadrados (OLS)
  ###
  
  modelo_ols <- function(formula_i, datos_train, datos_test){
    
    ### lista donde se guardaran klos resultdos
    results_ls <- list()
    
    # Creación y entrenamiento del modelo
    # ==============================================================================
    modelo <- try(lm(formula_i, data = datos_train), silent=T)
    
    if(class(modelo)!="try-error"){
      # modelo <- lm(log(ing_x_hrs) ~ ., data = datos_train)
      # summary(modelo)
      summary_i <- summary(modelo)
      print(c(j, i, summary_i$adj.r.squared))
    }else{
      next
    }
    
    summary_i <- summary(modelo)
    print(paste("ENT",j, "ols ","R2adj=", summary_i$adj.r.squared, sep=" "))
    
    # Coeficientes del modelo
    # ==============================================================================
    df_coeficientes <- modelo$coefficients %>%
      enframe(name = "predictor", value = "coeficiente")
    
    df_coeficientes %>%
      filter(predictor != "(Intercept)") %>%
      ggplot(aes(x = predictor, y = coeficiente)) +
      geom_col() +
      labs(title = "Coeficientes del modelo OLS") +
      theme_bw() +
      theme(axis.text.x = element_text(size = 5, angle = 45))
    
    ## -> INTERPRETACIÓN: El valor R2ajustado obtenido es muy alto (0.9967) lo que 
    # indica que el modelo es capaz de predecir con gran exactitud el contenido en grasa 
    # de las observaciones con las que se ha entrenado. El hecho de que el modelo en 
    # conjunto sea significativo (p-value: < 2.2e-16), pero que muy pocos de los 
    # predictores lo sean a nivel individual, es indicativo de una posible redundancia 
    # entre los predictores (colinealidad).
    
    ## MSE
    # ¿Qué tan bueno es el modelo prediciendo nuevas observaciones que no han participado 
    # en el ajuste? Al tratarse de un modelo de regresión, se emplea como métrica el 
    # Mean Square Error (MSE).
    
    # Predicciones de entrenamiento
    # ==============================================================================
    predicciones_train <- predict(modelo, newdata = datos_train)
    
    # MSE de entrenamiento
    # ==============================================================================
    training_mse <- mean((predicciones_train - datos_train$SAL_SEM)^2)
    paste("Error (mse) de entrenamiento:", training_mse)
    
    # Predicciones de test
    # ==============================================================================
    # dim(datos_test)
    # dim(datos_train)
    predicciones_test <- predict(modelo, newdata = datos_test)
    
    # MSE de test
    # ==============================================================================
    test_mse_ols <- mean((predicciones_test - datos_test$SAL_SEM)^2)
    paste("Error (mse) de test:", test_mse_ols)
    
    
    #####CALCULAR EL MAE (APARTE DEL MCE)
    
    ## -> INTERPRETACIÓN: El modelo tiene un MSE muy alto (0.4740585) cuando predice 
    # las mismas observaciones con las que se ha entrenado, pero mucho más bajo (19.4475566) 
    # al predecir nuevas observaciones. Esto significa que el modelo no es útil, ya que 
    # el objetivo es aplicarlo para predecir el contenido en grasa de futuras muestras 
    # de carne. Una de las causas por las 
    # que un modelo puede sufrir overfitting es la incorporación de predictores innecesarios, 
    # que no aportan información o que la información que aportan es redundante.
    
    
    results_ls$modelo <- modelo
    results_ls$test_mse_ols <- test_mse_ols
    results_ls$training_mse <- training_mse
    results_ls$summary_i <- summary_i
    
    
    return(results_ls)
    
  }
  modelo_ols_results <- modelo_ols(formula_i, datos_train, datos_test)
  
  fn_resids <- function(modelo){
    
    res_ls <- list()
    
    # Diagnostico de residuos
    # ==============================================================================
    
    ## Graficas generales
    # Change the panel layout to 2 x 2 (to look at all 4 plots at once)
    par(mfrow = c(2, 2))
    # Use plot() function to create diagnostic plots
    plot(modelo)
    # The diagnostic plots show residuals in four different ways:
    # Residuals vs Fitted: is used to check the assumptions of linearity. If the residuals 
    # are spread equally around a horizontal line without distinct patterns (red line 
    # is approximately horizontal at zero), that is a good indication of having a linear 
    # relationship.
    # 
    # Normal Q-Q: is used to check the normality of residuals assumption. If the majority 
    # of the residuals follow the straight dashed line, then the assumption is fulfilled.
    # 
    # Scale-Location: is used to check the homoscedasticity of residuals (equal variance 
    # of residuals). If the residuals are spread randomly and the see a horizontal line 
    # with equally (randomly) spread points, then the assumption is fulfilled.
    # 
    # Residuals vs Leverage: is used to identify any influential value in our dataset. 
    # Influential values are extreme values that might influence the regression results 
    # when included or excluded from the analysis. Look for cases outside of a dashed line.
    
    ##Residual QQ Plot
    print(ols_plot_resid_qq(modelo))
    
    ##Residual Normality Test
    
    # ols_test_normality(modelo$residuals[sample(1:length(modelo$residuals), 4999, FALSE)])
    if(length(modelo$residuals)>4999){
      res_ls$shapiro_test <- shapiro.test(modelo$residuals[sample(1:length(modelo$residuals), 4999, FALSE)])
      
    }else{
      res_ls$shapiro_test <- shapiro.test(modelo$residuals)
      
    }
    
    
    # Como es una prueba de bondad de ajuste aquí interesa no rechazar la hipótesis nula, es
    # decir, interesa que el valor de p sea mayor de 0,05 para no rechazar la hipótesis nula
    # (queremos que p > 0,05). 
    
    ##Correlation between observed residuals and expected residuals under normality.
    res_ls$cor_resid <- ols_test_correlation(modelo)
    
    ##Testing the Homoscedasticity Assumption
    # Breusch-Pagan test
    homoscedasticidad_p_value <- lmtest::bptest(modelo)$p.value
    res_ls$homoscedasticidad_p_value <- homoscedasticidad_p_value
    # If p-value<0.05 then the test suggests heteroscedasticity (i.e. residuals having 
    # a non-constant variance). If we were quite concerned about # heteroscedasticity, 
    # we could try using logarithmic or square root transformation on the response 
    # variable to reduce heteroscedasticity.
    
    ##Test for autocorrelation (Violations of independence)
    par(mfrow = c(1, 1))
    acf_v <- acf(modelo$residuals, lag.max = 15, plot = TRUE)
    print(acf_v)
    quantile_critico <- qnorm((1 + .95)/2)/sqrt(acf_v$n.used)
    criterio <- abs(quantile_critico)
    significativos <- which(abs(as.data.frame(acf_v$acf)[,1])>criterio)
    lags_significativos <- paste(significativos, sep=", ")
    res_ls$lags_significativos <- lags_significativos
    
    ##Collinearity
    # model_corr_matrix <- cor(datos_train)
    # model_corr_matrix
    # cor_matrix <- corrplot::corrplot(df_correlaciones)
    # res_ls$cor_matrix <- cor_matrix
    
    ##Otro metodo de correlacion
    # # Correlación entre columnas numéricas
    df_correlaciones <- bd_mice %>%
      correlate(method = "pearson") %>%
      stretch(remove.dups = TRUE)
    df_correlaciones %>% mutate(r_abs = abs(r)) %>% arrange(desc(r_abs)) %>% head(5)
    # View(df_correlaciones)
    res_ls$df_correlaciones <- df_correlaciones
    
    ## -> INTERPRETACIÓN: 
    # Muchas de las variables están altamente correlacionadas 
    # (correlación absoluta > 0.8), lo que supone un problema a la hora de emplear modelos 
    # de regresión lineal.
    
    ##Multicollinearity
    # Use the variance inflation factor from the car package
    vif_values <- try(car::vif(modelo, type = 'predictor'), silent=T)
    if(class(vif_values)=="try-error"){
      vif_values <- NA
    }
    
    # #create horizontal bar chart to display each VIF value
    # barplot(vif_values[,3], main = "VIF Values", horiz = TRUE, col = "steelblue")
    # #add vertical line at 5
    # abline(v = 5, lwd = 3, lty = 2)
    
    
    # Whilst collinearity can be detected with a correlation matrix, multicollinearity 
    # is not as easy to detect. The Variance Inflation Factor (VIF) can be used to find 
    # how much the variance of a regression coefficient is inflated due to multicollinearity 
    # in the model. The smallest possible value is one, indicating no multicollinearity. 
    # A value which exceeds 5 or 10 indicates a problematic amount of multicollinearity 
    # in the data. In R we use the vif() function from the car package to detect 
    # multicollinearity in a multiple regression model (where the response variable is 
    # ozone and all explanatory variables are added):
    
    return(res_ls)
  }
  modelo_ols_residuals <- fn_resids(modelo_ols_results$modelo)
  
  
  ###
  ### 2) METODO: Stepwise Selection
  ###
  
  modelo_step <- function(formula_i, datos_train, datos_test){
    
    ### lista donde se guardaran klos resultdos
    results_ls <- list()
    
    ###
    ### 2) METODO: Stepwise Selection
    ###
    
    # La función step() de paquete stats permite aplicar el proceso de stepwise selection 
    # (both, backward, forward) y seleccionar el mejor modelo en base al AIC.
    
    # Creación y entrenamiento del modelo
    # ==============================================================================
    modelo <- step(
      object    = lm(formula = formula_i, data = datos_train),
      direction = "backward",
      scope     = list(upper = ~., lower = ~1),
      trace     = FALSE
    )
    # ?step
    
    summary_i <- summary(modelo)
    print(paste("ENT",j, "stepwise ","R2adj=", summary_i$adj.r.squared, sep=" "))
    
    # Coeficientes del modelo
    # ==============================================================================
    df_coeficientes <- modelo$coefficients %>%
      enframe(name = "predictor", value = "coeficiente")
    
    df_coeficientes %>%
      filter(predictor != "(Intercept)") %>%
      ggplot(aes(x = predictor, y = coeficiente)) +
      geom_col() +
      labs(title = "Coeficientes del modelo Stepwise") +
      theme_bw() +
      theme(axis.text.x = element_text(size = 6, angle = 45))
    
    paste("Número de predictores incluidos en el modelo:", length(modelo$coefficients))
    
    # Predicciones de entrenamiento
    # ==============================================================================
    predicciones_train <- predict(modelo, newdata = datos_train)
    
    # MSE de entrenamiento
    # ==============================================================================
    training_mse <- mean((predicciones_train - datos_train$SAL_SEM)^2)
    paste("Error (mse) de entrenamiento:", training_mse)
    
    # Predicciones de test
    # ==============================================================================
    # View(datos_test)
    dim(datos_test)
    length(predicciones_train)
    predicciones_test <- predict(modelo, newdata = datos_test)
    
    # MSE de test
    # ==============================================================================
    test_mse_step <- mean((predicciones_test - datos_test$SAL_SEM)^2)
    paste("Error (mse) de test:", test_mse_step)
    
    ## -> INTERPRETACIÓN: El proceso de stepwise selection devuelve como mejor modelo 
    # el formado por 69 de los 100 predictores disponibles. Al haber eliminado predictores 
    # del modelo, el error de entrenamiento siempre aumenta.
    
    
    results_ls$modelo <- modelo
    results_ls$test_mse_ols <- test_mse_step
    results_ls$training_mse <- training_mse
    results_ls$summary_i <- summary_i
    
    
    return(results_ls)
    
  }
  modelo_step_results <- modelo_step(formula_i, datos_train, datos_test)
  
  fn_resids <- function(modelo){
    
    # modelo <- modelo_step_results$modelo
    
    res_ls <- list()
    
    # Diagnostico de residuos
    # ==============================================================================
    
    ## Graficas generales
    # Change the panel layout to 2 x 2 (to look at all 4 plots at once)
    par(mfrow = c(2, 2))
    # Use plot() function to create diagnostic plots
    plot(modelo)
    # The diagnostic plots show residuals in four different ways:
    # Residuals vs Fitted: is used to check the assumptions of linearity. If the residuals 
    # are spread equally around a horizontal line without distinct patterns (red line 
    # is approximately horizontal at zero), that is a good indication of having a linear 
    # relationship.
    # 
    # Normal Q-Q: is used to check the normality of residuals assumption. If the majority 
    # of the residuals follow the straight dashed line, then the assumption is fulfilled.
    # 
    # Scale-Location: is used to check the homoscedasticity of residuals (equal variance 
    # of residuals). If the residuals are spread randomly and the see a horizontal line 
    # with equally (randomly) spread points, then the assumption is fulfilled.
    # 
    # Residuals vs Leverage: is used to identify any influential value in our dataset. 
    # Influential values are extreme values that might influence the regression results 
    # when included or excluded from the analysis. Look for cases outside of a dashed line.
    
    ##Residual QQ Plot
    print(ols_plot_resid_qq(modelo))
    
    ##Residual Normality Test
    
    # ols_test_normality(modelo$residuals[sample(1:length(modelo$residuals), 4999, FALSE)])
    if(length(modelo$residuals)>4999){
      res_ls$shapiro_test <- shapiro.test(modelo$residuals[sample(1:length(modelo$residuals), 4999, FALSE)])
      
    }else{
      res_ls$shapiro_test <- shapiro.test(modelo$residuals)
      
    }
    
    
    # Como es una prueba de bondad de ajuste aquí interesa no rechazar la hipótesis nula, es
    # decir, interesa que el valor de p sea mayor de 0,05 para no rechazar la hipótesis nula
    # (queremos que p > 0,05). 
    
    ##Correlation between observed residuals and expected residuals under normality.
    res_ls$cor_resid <- ols_test_correlation(modelo)
    
    ##Testing the Homoscedasticity Assumption
    # Breusch-Pagan test
    homoscedasticidad_p_value <- lmtest::bptest(modelo)$p.value
    res_ls$homoscedasticidad_p_value <- homoscedasticidad_p_value
    # If p-value<0.05 then the test suggests heteroscedasticity (i.e. residuals having 
    # a non-constant variance). If we were quite concerned about # heteroscedasticity, 
    # we could try using logarithmic or square root transformation on the response 
    # variable to reduce heteroscedasticity.
    
    ##Test for autocorrelation (Violations of independence)
    par(mfrow = c(1, 1))
    acf_v <- acf(modelo$residuals, lag.max = 15, plot = TRUE)
    print(acf_v)
    quantile_critico <- qnorm((1 + .95)/2)/sqrt(acf_v$n.used)
    criterio <- abs(quantile_critico)
    significativos <- which(abs(as.data.frame(acf_v$acf)[,1])>criterio)
    lags_significativos <- paste(significativos, sep=", ")
    res_ls$lags_significativos <- lags_significativos
    
    ##Collinearity
    # model_corr_matrix <- cor(datos_train)
    # model_corr_matrix
    # cor_matrix <- corrplot::corrplot(df_correlaciones)
    # res_ls$cor_matrix <- cor_matrix
    
    ##Otro metodo de correlacion
    # # Correlación entre columnas numéricas
    df_correlaciones <- bd_mice %>%
      correlate(method = "pearson") %>%
      stretch(remove.dups = TRUE)
    df_correlaciones %>% mutate(r_abs = abs(r)) %>% arrange(desc(r_abs)) %>% head(5)
    # View(df_correlaciones)
    res_ls$df_correlaciones <- df_correlaciones
    
    ## -> INTERPRETACIÓN: 
    # Muchas de las variables están altamente correlacionadas 
    # (correlación absoluta > 0.8), lo que supone un problema a la hora de emplear modelos 
    # de regresión lineal.
    
    ##Multicollinearity
    # Use the variance inflation factor from the car package
    vif_values <- try(car::vif(modelo, type = 'predictor'), silent=T)
    if(class(vif_values)=="try-error"){
      vif_values <- NA
    }
    
    #create horizontal bar chart to display each VIF value
    # try(barplot(vif_values[,"GVIF"], main = "VIF Values", horiz = TRUE, col = "steelblue"), silent=T)
    #add vertical line at 5
    # abline(v = 5, lwd = 3, lty = 2)
    
    
    # Whilst collinearity can be detected with a correlation matrix, multicollinearity 
    # is not as easy to detect. The Variance Inflation Factor (VIF) can be used to find 
    # how much the variance of a regression coefficient is inflated due to multicollinearity 
    # in the model. The smallest possible value is one, indicating no multicollinearity. 
    # A value which exceeds 5 or 10 indicates a problematic amount of multicollinearity 
    # in the data. In R we use the vif() function from the car package to detect 
    # multicollinearity in a multiple regression model (where the response variable is 
    # ozone and all explanatory variables are added):
    
    return(res_ls)
  }
  modelo_step_residuals <- fn_resids(modelo_step_results$modelo)
  
  ###
  ### 3) METODO: Ridge
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
  modelo_ridge_results <- modelo_ridge(formula_i, datos_train, datos_test)

  ###
  ### 3) METODO: Lasso
  ###
  
  modelo_lasso <- function(formula_i, datos_train, datos_test){
    
    ### lista donde se guardaran klos resultdos
    results_ls <- list()
    
    # El proceso para realizar un ajuste mediante Lasso y la identificación del mejor 
    # valor de lambda es equivalente al seguido en el caso de Ridge pero indicando en la 
    # función glmnet() que alpha=1.
    
    # Matrices de entrenamiento y test
    # ==============================================================================
    x_train <- model.matrix(formula_i, data = datos_train)[, -1]
    y_train <- datos_train$SAL_SEM
    
    x_test <- model.matrix(formula_i, data = datos_test)[, -1]
    y_test <- datos_test$SAL_SEM
    
    # Creación y entrenamiento del modelo
    # ==============================================================================
    # Para obtener un ajuste con regularización Lasso se indica argumento alpha=1.
    # Si no se especifica valor de lambda, se selecciona un rango automático.
    modelo <- glmnet(
      x           = x_train,
      y           = y_train,
      alpha       = 1,
      nlambda     = 100,
      standardize = TRUE
    )
    
    # Evolución de los coeficientes en función de lambda
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
    # mayor y más predictores quedan excluidos (su coeficiente es 0).Para identificar 
    # el valor de lambda que da lugar al mejor modelo, se puede recurrir a validación 
    # cruzada con la función cv.glmnet().
    
    # Evolución del error en función de lambda
    # ==============================================================================
    set.seed(123)
    cv_error <- cv.glmnet(
      x      = x_train,
      y      = y_train,
      alpha  = 1,
      nfolds = 10,
      type.measure = "mse",
      standardize  = TRUE
    )
    
    plot(cv_error)
    
    # Mejor valor lambda encontrado
    # ==============================================================================
    paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
    
    
    # Mejor valor lambda encontrado + 1sd
    # ==============================================================================
    # Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
    paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)
    
    # Se entrena de nuevo el modelo, esta vez empleando el mayor valor de lambda cuyo 
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
      labs(title = "Coeficientes del modelo Lasso") +
      theme_bw() +
      theme(axis.text.x = element_text(size = 6, angle = 45))
    
    df_coeficientes %>%
      filter(
        predictor != "(Intercept)",
        coeficiente != 0
      ) 
    
    # De los 100 predictores disponibles, el modelo final solo incluye 8 .
    
    # Predicciones de entrenamiento
    # ==============================================================================
    predicciones_train <- predict(modelo, newx = x_train)
    
    # MSE de entrenamiento
    # ==============================================================================
    training_mse <- mean((predicciones_train - y_train)^2)
    paste("Error (mse) de entrenamiento:", training_mse)
    
    
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
    results_ls$summary_i <- summary_i
    
    
    return(results_ls)
    
  }
  modelo_lasso_results <- modelo_lasso(formula_i, datos_train, datos_test)

  ###
  ### 4) METODO: PCR
  ###
  
  modelo_pcr <- function(formula_i, datos_train, datos_test){
    
    ### lista donde se guardaran klos resultdos
    results_ls <- list()
    
    # Los modelos de regresión basados en componentes principales pueden ajustarse 
    # mediante la función pcr() del paquete pls.
    
    # Creación y entrenamiento del modelo
    # ==============================================================================
    set.seed(123)
    # Importante estandarizar las variables indicándolo con el argumento scale 
    # Indicando validation = CV, se emplea 10-fold-cross-validation para
    # identificar el número óptimo de componentes.
    
    datos_train_gower_x <- as.matrix(daisy(datos_train[,-1], metric="gower"))
    datos_train_gower <- data.frame(cbind(datos_train[,1], datos_train_gower_x))
    colnames(datos_train_gower)[1] <- colnames(datos_train)[1]
    # colnames(datos_train_gower)[1]
    modelo_pcr <- pcr( SAL_SEM ~., data = datos_train_gower, scale = TRUE, validation = "CV")
    
    # El summary del modelo pcr devuelve la estimación del RMSEP (raíz cuadrada del MSE) 
    # para cada posible número de componentes introducidas en el modelo. También se muestra 
    # el % de varianza explicada acumulada por cada número de componentes. Esta formación 
    # también puede extraerse con las funciones MSEP() y RMSEP().
    
    # Evolución del error en función del número de componentes
    # ==============================================================================
    error_cv <- MSEP(modelo_pcr, estimate = "CV")
    # ?MSEP
    n_componentes_optimo <- which.min(error_cv$val)
    error_cv <- data.frame(
      componentes = seq_along(as.vector(error_cv$val)) - 1,
      mse         = as.vector(error_cv$val)
    )
    error_cv %>% head()
    
    ggplot(data = error_cv, aes(x = componentes, y = mse)) +
      geom_line() +
      geom_vline(xintercept = n_componentes_optimo, color = 'red', linetype = 'dashed') +
      labs(title = "Error en función de las componentes incluidas") +
      theme_bw()
    
    # Mejor número de componentes encontrados
    # ==============================================================================
    paste("Número de componentes óptimo:", n_componentes_optimo)
    
    # Una vez identificado el número óptimo de componentes, se reentrena el modelo 
    # indicando este valor.
    
    modelo <- pcr(SAL_SEM ~ ., data = datos_train, scale = TRUE, ncomp = 10) ## se corrigio para poder elegir menos componentes
    
    summary_i <- summary(modelo)
    
    # Predicciones de entrenamiento
    # ==============================================================================
    predicciones_train <- predict(modelo, newdata = datos_train)
    
    # MSE de entrenamiento
    # ==============================================================================
    training_mse <- mean((predicciones_train - as.numeric(datos_train$SAL_SEM))^2)
    paste("Error (mse) de entrenamiento:", training_mse)
    
    # Predicciones de test
    # ==============================================================================
    predicciones_test <- predict(modelo, newdata  = datos_test)
    
    # MSE de test
    # ==============================================================================
    test_mse_pcr <- mean((predicciones_test - as.numeric(datos_test$SAL_SEM))^2)
    paste("Error (mse) de test:", test_mse_pcr)
    
    
    results_ls$modelo <- modelo
    results_ls$test_mse_ols <- test_mse_pcr
    results_ls$training_mse <- training_mse
    results_ls$summary_i <- summary_i
    
    
    return(results_ls)
    
  }
  modelo_pcr_results <- modelo_pcr(formula_i, datos_train, datos_test)
  
  ###
  ### 5) METODO: PLS
  ###
  
  modelo_pls <- function(formula_i, datos_train, datos_test){
    
    ### lista donde se guardaran klos resultdos
    results_ls <- list()
    
    # Los modelos de regresión basados en partial least squares pueden ajustarse mediante 
    # la función plsr() del paquete pls.
    
    # Creación y entrenamiento del modelo
    # ==============================================================================
    set.seed(123)
    # Importante estandarizar las variables indicándolo con el argumento scale 
    # Indicando validation = CV, se emplea 10-fold-cross-validation para
    # identificar el número óptimo de componentes.
    modelo_plsr <- plsr(formula_i, data = datos_train, scale = TRUE, validation = "CV")
    
    # El summary del modelo plsr devuelve la estimación del RMSEP (raíz cuadrada del MSE) 
    # para cada posible número de componentes introducidas en el modelo. También se 
    # muestra el % de varianza explicada acumulada por cada número de componentes. 
    # Esta formación también puede extraerse con las funciones MSEP() y RMSEP().
    
    # Evolución del error en función del número de componentes
    # ==============================================================================
    error_cv <- MSEP(modelo_plsr, estimate = "CV")
    n_componentes_optimo <- which.min(error_cv$val)
    error_cv <- data.frame(
      componentes = seq_along(as.vector(error_cv$val)) - 1,
      mse         = as.vector(error_cv$val)
    )
    
    error_cv %>% head()
    
    ggplot(data = error_cv, aes(x = componentes, y = mse)) +
      geom_line() +
      geom_vline(xintercept = n_componentes_optimo, color = 'red', linetype = 'dashed') +
      labs(title = "Error en función de las componentes incluidas") +
      theme_bw()
    
    # Mejor número de componentes encontrados
    # ==============================================================================
    paste("Número de componentes óptimo:", n_componentes_optimo)
    
    # Una vez identificado el número óptimo de componentes, se entrena de nuevo el 
    # modelo con el valor encontrado.
    
    modelo <- plsr(SAL_SEM ~ ., data = datos_train, scale = TRUE, ncomp = 10)
    # R2(modelo)
    summary_i <- summary(modelo)
    
    
    # Predicciones de entrenamiento
    # ==============================================================================
    predicciones_train <- predict(modelo, newdata = datos_train)
    
    # MSE de entrenamiento
    # ==============================================================================
    training_mse <- mean((predicciones_train - as.numeric(datos_train$SAL_SEM))^2)
    paste("Error (mse) de entrenamiento:", training_mse)
    
    
    # Predicciones de test
    # ==============================================================================
    predicciones_test <- predict(modelo, newdata  = datos_test)
    
    # MSE de test
    # ==============================================================================
    test_mse_plsr <- mean((predicciones_test - as.numeric(datos_test$SAL_SEM))^2)
    paste("Error (mse) de test:", test_mse_plsr)
    
    
    results_ls$modelo <- modelo
    results_ls$test_mse_ols <- test_mse_plsr
    results_ls$training_mse <- training_mse
    results_ls$summary_i <- summary_i
    
    
    return(results_ls)
    
  }
  modelo_pls_results <- modelo_pls(formula_i, datos_train, datos_test)
  
  ###
  ### 6) COMPARACION DE RESULTADOS
  ###
  
  df_comparacion <- data.frame(
    ent=j,
    modelo = c("ols", "Stepwise","Lasso", "Ridge",  "PCR","PLS"),
    mse    = c(modelo_ols_results$test_mse_ols, modelo_step_results$test_mse_ols, modelo_lasso_results$test_mse_ols,
               modelo_ridge_results$test_mse_ridge, modelo_pcr_results$test_mse_ols, modelo_pls_results$test_mse_ols)
  )
  
  mse_res[[j]] <- df_comparacion
  
  ggplot(data = df_comparacion[,-1], aes(x = modelo, y = mse)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = round(mse, 2)), vjust = -0.1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ### -> RESULTADO: En este caso el mejor modelo se obtiene aplicando regularización 
  # Lasso. De esta forma, se consigue un modelo con mayor poder predictivo y que requiere 
  # de menos predictores.
  
}

do.call("rbind", mse_res)


#################################################################################
#################################################################################

###
### PCR & PLS
###

# Para argumentar el uso de esta técnica  partimos del modelo lineal general 
# Y = X • Beta + Error donde Beta = inv(X’X) * X’Y y ya analizamos los trastornos 
# que nos provoca la inv(X’X) cuando hay columnas de X que son linealmente dependientes, 
# cuando hay multicolinealidad. En ese caso empleábamos la regresión ridge. 
# 
# Si tenemos más variables que observaciones. Entonces si que no somos capaces de 
# tener una solución para la inv(X’X). Para este problema 
# contamos con los mínimos cuadrados parciales.
# 
# Como siempre se trata de estimar Y a partir de X con la salvedad de que X tiene 
# más columnas que filas y el modelo de mínimos cuadrados ordinarios no tiene solución. 
# En este caso lo primero que se nos puede ocurrir es realizar un análisis de componentes 
# principales de X para reducir la dimensionalidad. Estaríamos ante la regresión por 
# componentes principales, principal components regression (PCR). Esta técnica está 
# íntimamente ligada a la PLS. Lo que haremos será estimar Y a partir de las componentes 
# principales de X. Por definición las componentes principales sirven para reducir la 
# dimensionalidad capturando la mayor varianza de los datos, se seleccionan matricialmente 
# las componentes de mayor a menor contribución a la variabilidad de los datos. 
# Si transformamos la matriz X = UdV donde U’U = V’V = I son los vectores singulares 
# y d es la matriz con los valores singulares ya podremos obtener una solución por 
# mínimos cuadrados.
# 
# Principalmente se basan en la descomposición en valores singulares lo que garantiza 
# que la ortonormalidad. Y en función de la matriz utilizada estaremos ante PCR o PLS. 
# 
# Si el modelo emplea la matriz X para la obtención 
# de las componentes estamos ante PCR, si el modelo emplea la matriz YX estamos ante PLS 
# y en PLS si tenemos sólo una variable dependiente tenemos PLS1 y si tenemos más de una 
# variable dependiente tendremos PLS2. Para conocer mejor los algoritmos que emplean 
# estas técnicas recomiendo esta lectura.
# 
# Este ejercicio se va a centrar en PCR y PLS1. La librería que vamos a emplear 
# para la realización de esta tarea en R será pls.

url <- "http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data"
cancer <- read.table(url, header=TRUE)

entreno = subset(cancer,train=="TRUE")
entreno = entreno[,-10]

# Nuestra variable dependiente de nuevo será el logaritmo de psa, del nivel del 
# antígeno prostático. El primer modelo que vamos a realizar será la regresión por 
# componentes principales, con el paquete pls esta tarea se lleva a cabo con la función pcr:

modelo_pcr <- pcr(lpsa~. , ncomp=8, data=entreno, validation="LOO")
summary(modelo_pcr)

# Creamos un modelo con el número máximo de componentes 8 y realizamos una validación 
# cruzada con el método LOO leave-one-out, dejando uno fuera.Por defecto pls tiene 
# CV como método de validación, 
# que realiza una validación cruzada más simple, también podemos emplear la opción none 
# si no deseamos que se realice. Lo que es evidente es que necesitamos determinar el 
# número más apropiado.

# Vamos a graficar la raíz del cuadrado de los errores para determinar que número 
# de componentes seleccionamos:
  
plot(RMSEP(modelo_pcr), legendpos = "topright",
     main="Raiz del error para el modelo con PCR")

# Parece que 5 componentes pueden ser adecuados, aunque se sigue produciendo un 
# descenso en la raíz del error pero en menor medida. Con estas matizaciones nuestro 
# modelo final quedaría:

modelo_pcr <- pcr(lpsa~. , ncomp=5, data=entreno, validation="none")

test = subset(cancer,train=="FALSE")
test = test[,-10]

pred_pcr = predict(modelo_pcr,ncomp=5,newdata=test)
sum((test$lpsa-pred_pcr)^2)

# La suma del cuadrado del error es 16.8 no es un modelo que mejore a un modelo de 
# regresión por mínimos cuadrados ordinarios. En cualquier caso se recomienda tipificar 
# las variables a la hora de realizar el modelo. Por defecto los algoritmos que incluye 
# el paquete pls corrigen los datos por la media, pero si añadimos la opción sclale=TRUE 
# además de corregir por la media dividiremos por la desviación típica:

modelo_pcr2 <- pcr(lpsa~. , ncomp=5, data=entreno, validation="none",scale=T)
pred_pcr2 <- predict(modelo_pcr2,ncomp=5,newdata=test)
sum((test$lpsa-pred_pcr2)^2)

# La suma del cuadrado de los errores queda en 16.2 mejorando ligeramente el 
# resultado anterior pero sin mejorar el resultado de mínimos cuadrados ordinarios. 
# Evaluemos ahora el modelo PLS con los mismos datos:

modelo_pls = plsr(lpsa~. , ncomp=8, data=entreno, validation="LOO", scale=T)
summary(modelo_pls)
plot(RMSEP(modelo_pls), legendpos = "topright",
     main="Raiz del error para el modelo con PLS")

# En este caso 4 componentes si parecen suficientes para realizar nuestro modelo 
# ya que vemos que un mayor número no implica una sustancial mejora. Ajustemos el 
# modelo con esas 4 componentes y comparemos con el modelo por componentes principales:

modelo_pls = plsr(lpsa~. , ncomp=4, data=entreno, validation="LOO",scale=T)
pred_pls = predict(modelo_pls,ncomp=4,newdata=test)
sum((test$lpsa-pred_pls)^2)

# El modelo mejora al anterior pero no 
# mejora a los resultados del modelo por regresión ridge. Es posible que nos interese 
# analizar el comportamiento de los parámetros que obtenemos con el modelo. Para ello 
# el paquete pls nos permite graficar los parámetros de la regresión en función del 
# número de componentes:  
  
plot(modelo_pls,legendpos="bottomleft", plottype='coef', 
     labels=names(entreno), ncomp=1:4)

# Vemos como la primera componente tiene unos parámetros del modelo muy similares 
# para las variables, la segunda ya comienza a diferenciar el comportamiento de 
# algunas variables y la tercera y la cuarta si producen mayores diferencias entre 
# parámetros. Realicemos un gráfico de correlaciones de las cargas entre estas dos 
# primeras componentes:

plot(modelo_pls,plottype="correlation", labels=names(entreno))

# Las cargas y las puntuaciones son los dos elementos más útiles a la hora de 
# realizar interpretaciones sobre los parámetros de nuestro modelo.

###########################
### Principal Component Analysis (PCA)
###########################
# devtools::install_github("richardjtelford/ggbiplot", ref = "experimental")
# install.packages("mice")

library("mice")
library("FactoMineR") #MCA
library("factoextra") #fviz_screeplot
library("fpc") #fpc
library("cluster") #clusplot
library("spatstat") #crossdist
library("ggbiplot")
library("devtools")

# setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/1T/TEC 2022 - Concentración/RETOS/EQUIPO 2")
# load("basejaccard.R")

# We will use the mtcars dataset, which is built into R. This dataset consists of 
# data on 32 models of car, taken from an American motoring magazine (1974 Motor Trend magazine). 
# For each car, you have 11 features, expressed in varying units (US units), They are as follows:

# *mpg: Fuel consumption (Miles per (US) gallon): more powerful and heavier cars tend to consume more fuel.
# *cyl: Number of cylinders: more powerful cars often have more cylinders
# *disp: Displacement (cu.in.): the combined volume of the engine's cylinders
# *hp: Gross horsepower: this is a measure of the power generated by the car
# *drat: Rear axle ratio: this describes how a turn of the drive shaft corresponds to a turn of the wheels. Higher values will decrease fuel efficiency.
# *wt: Weight (1000 lbs): pretty self-explanatory!
# *qsec: 1/4 mile time: the cars speed and acceleration
# *vs: Engine block: this denotes whether the vehicle's engine is shaped like a "V", or is a more common straight shape.
# *am: Transmission: this denotes whether the car's transmission is automatic (0) or manual (1).
# *gear: Number of forward gears: sports cars tend to have more gears.
# *carb: Number of carburetors: associated with more powerful engines
# Note that the units used vary and occupy different scales.

# Because PCA works best with numerical data, you'll exclude the two categorical 
# variables (vs and am). You are left with a matrix of 9 columns and 32 rows, which 
# you pass to the prcomp() function, assigning your output to mtcars.pca. You will 
# also set two arguments, center and scale, to be TRUE. Then you can have a peek at 
# your PCA object with summary().

# colnames(bd)
# mtcars <- bd[,c(24, 4:14)]
# head(mtcars)
# colnames(mtcars)

mtcars.pca <- prcomp(mtcars, center = TRUE,scale. = TRUE)
# plot(mtcars.pca)

# You obtain 9 principal components, which you call PC1-9. Each of these explains a 
# percentage of the total variation in the dataset. That is to say: PC1 explains 63% 
# of the total variance, which means that nearly two-thirds of the information in the 
# dataset (9 variables) can be encapsulated by just that one Principal Component. PC2 
# explains 23% of the variance. So, by knowing the position of a sample in relation to just 
# PC1 and PC2, you can get a very accurate view on where it stands in relation to other samples, 
# as just PC1 and PC2 can explain 86% of the variance.

# str(mtcars.pca)

# PCA object contains the following information:
# The center point ($center), scaling ($scale), standard deviation(sdev) of each principal component
# The relationship (correlation or anticorrelation, etc) between the initial variables and the principal components ($rotation)
# The values of each sample in terms of the principal components ($x)

### Plotting PCA

# Now it's time to plot your PCA. You will make a biplot, which includes both the 
# position of each sample in terms of PC1 and PC2 and also will show you how the 
# initial variables map onto this. You will use the ggbiplot package, which offers 
# a user-friendly and pretty function to plot biplots. A biplot is a type of plot 
# that will allow you to visualize how the samples relate to one another in our PCA 
# (which samples are similar and which are different) and will simultaneously reveal 
# how each variable contributes to each principal component.

ggbiplot(mtcars.pca, labels=rownames(mtcars))
# Now you can see which cars are similar to one another. For example, the Maserati 
# Bora, Ferrari Dino and Ford Pantera L all cluster together at the top. This makes 
# sense, as all of these are sports cars

# Maybe if you look at the origin of each of the cars. You'll put them into one of 
# three categories, one each for the US, Japanese and European cars. 
# You make a list for this info, then pass it to the groups argument of ggbiplot. 
# You'll also set the ellipse argument to be TRUE, which will draw an ellipse around 
# each group.

mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))

ggbiplot(mtcars.pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)

# Now you see something interesting: the American cars form a distinct cluster to 
# the right. Looking at the axes, you see that the American cars are characterized 
# by high values for cyl, disp, and wt. Japanese cars, on the other hand, are 
# characterized by high mpg. European cars are somewhat in the middle and less 
# tightly clustered than either group.


