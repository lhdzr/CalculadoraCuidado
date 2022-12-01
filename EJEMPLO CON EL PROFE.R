fn_modelado_res$
load("equipo_3_environment.R")

# se toma como base la BD para conservar todos los niveles posibles
id_sample <-sample(1:nrow(datos), 1, F)
pars_usuario_t <- datos[id_sample,,drop=F][,-1]

fn_prediction <- function(pars_usuario_t,
                          formula_i_global,
                          datos,
                          par_y_log,
                          fn_modelado_res){
  
  
  # ## @pars
 #  par_y_log=T
 # formula_i_global <- fn_modelado_res$formula_i_global
  
  
  
  ###############################################################################
  ### -- > A partir de aqui se corre en el sistema
  ################################################################################
  
  
  ### SE LE DA FORMATO A CADA UNA DE LAS VARIABLES (PARAMETROS) CONSISTETE CON LA BD DE ENTRENAMIENTO
  
  pars_usuario_t$SEXO<- as.factor(pars_usuario_t$SEXO)
  pars_usuario_t$Edad <- as.numeric(pars_usuario_t$EDAD)
  #pars_usuario_t$ent <- as.factor(pars_usuario_t$ent)
  pars_usuario_t$ESC<- as.numeric(pars_usuario_t$ESC)
  pars_usuario_t$urbano <- as.factor(pars_usuario_t$urbano)
  pars_usuario_t$TIEMPO_TOTAL<- as.numeric(pars_usuario_t$TIEMPO_TOTAL)
  pars_usuario_t$SIT_CONYUGAL <- as.factor(pars_usuario_t$SIT_CONYUGAL)
  pars_usuario_t$INDIGENA <- as.factor(pars_usuario_t$INDIGENA)
  pars_usuario_t$TIPO_TRABAJO <- as.factor(pars_usuario_t$TIPO_TRABAJO)
  pars_usuario_t$SEGURO_MEDICO <- as.factor(pars_usuario_t$SEGURO_MEDICO)
  pars_usuario_t$PAREN <- as.factor(pars_usuario_t$PAREN)
  pars_usuario_t$TIEMPO_CHOF<- as.numeric(pars_usuario_t$TIEMPO_CHOF)
  pars_usuario_t$TIEMPO_COCIN<- as.numeric(pars_usuario_t$TIEMPO_COCIN)
  pars_usuario_t$TIEMPO_ENF_TEC<- as.numeric(pars_usuario_t$TIEMPO_ENF_TEC)
  pars_usuario_t$TIEMPO_FISIO<- as.numeric(pars_usuario_t$TIEMPO_FISIO)
  pars_usuario_t$TIEMPO_CUID<- as.numeric(pars_usuario_t$TIEMPO_CUID)
  pars_usuario_t$NUM_PER<- as.numeric(pars_usuario_t$NUM_PER)
  pars_usuario_t$esc_jefe<- as.numeric(pars_usuario_t$esc_jefe)
  pars_usuario_t$jefatura_femenina <- as.factor(pars_usuario_t$jefatura_femenina)
  pars_usuario_t$edad_jefe<- as.numeric(pars_usuario_t$edad_jefe)
  
  
  ### se agrega un valor artificial a la primera columan (variable Y) solo para obtener la siguiente matriz que necesitan los modelos Lasso y Ridge
  pars_usuario_t <- cbind("SAL_SEM"=1, pars_usuario_t)
  
  ### se crea la formula para obtener la matriz especifica para modelos Lasso y Ridge
  pars_usuario_t_lasso_ridge <- model.matrix(formula_i_global, data = datos[1,])[,-1, drop=F]
  
  ## se renombran objetos
  x_new <- pars_usuario_t
  x_new_lasso_ridge <- pars_usuario_t_lasso_ridge
  
  par_ENT <- x_new[,"ent"]
  par_ENT <- as.numeric(par_ENT)
  print(c(class(par_ENT), par_ENT))
  
  ### 3. Seleccion del modelo con menor MSE
  
  min_mse_value <- min(fn_modelado_res$mse[[par_ENT]]$mse, na.rm=T)
  mse_df <- fn_modelado_res$mse[[par_ENT]]$mse
  # print(mse_df)
  id_modelo_optimo <- which(mse_df == min_mse_value)[1]
  # print(id_modelo_optimo)
  modelo_optimo_name <- fn_modelado_res$mse[[par_ENT]]$modelo[id_modelo_optimo]
  modelo_optimo <- fn_modelado_res$modelos[[par_ENT]][[id_modelo_optimo]] ## string
  # length(fn_modelado_res$modelos)
  
  ### Valor de R2
  max_r2_value <- fn_modelado_res$r2[[par_ENT]]$r2[id_modelo_optimo]
  
  ### COEFICIENTES (VALOR DE BETAS O NIVEL DE IMPORTANCIA)
  coeficientes <- fn_modelado_res$coef[[par_ENT]][[id_modelo_optimo]]
  colnames(coeficientes) <- c("variable", "coef_importancia")
  
  ### 4. Prediccion con el mejor modelo seleccionado
  
  if(modelo_optimo_name=="OLS" | modelo_optimo_name=="Stepwise"){
    
    prediction <- predict(fn_modelado_res$modelos[[par_ENT]][[id_modelo_optimo]], newdata = x_new)
  }
  if(modelo_optimo_name=="Lasso" | modelo_optimo_name=="Ridge"){
    prediction <- predict(fn_modelado_res$modelos[[par_ENT]][[id_modelo_optimo]], newx = x_new_lasso_ridge)
  }
  if(modelo_optimo_name=="Xgboost" | modelo_optimo_name=="Randomforest" | modelo_optimo_name=="Neural_network"){
    ### En caso de que se elija el modelo H2O se requiere otro formato
    x_new_h2o <- as.h2o(x_new)
    prediction <- predict(fn_modelado_res$modelos[[par_ENT]][[id_modelo_optimo]], newdata = x_new_h2o)
    prediction <- as.data.frame(prediction)
  }
  
  if(par_y_log){
    prediccion_final <- exp(prediction)
  }else{
    prediccion_final <- prediction
  }
  
  ### Se reporta el MSE y el R2
  mse_modelo_optimo <- fn_modelado_res$mse[[par_ENT]]$mse[id_modelo_optimo]
  id_r2_optimo <- grep(modelo_optimo_name, fn_modelado_res$r2[[par_ENT]]$modelo)
  r2_modelo_optimo <- fn_modelado_res$r2[[par_ENT]]$r2[id_r2_optimo]
  
  ### Se genera un solo Data frame con los resultados principales
  
  resultado <- data.frame(data.frame(par_ENT=par_ENT, modelo_optimo_name, prediccion=round(as.matrix(prediccion_final),4), mse_modelo_optimo, r2_modelo_optimo))
  print(resultado)
  
  ###
  ### GRAFICAS
  ###
  
  # par(mfrow = c(2, 2)) # divide la pantalla en 1 renglon y 2 columnas
  
  ### BOXPLOT NACIONAL
  
  ### Se eliminan valores extremos basado en cuantiles
  quants_values <- quantile(datos$SAL_SEM, probs=seq(0,1, 0.25))
  id_quant <- which(datos$SAL_SEM>=quants_values[length(quants_values)-1])
  
  datos_j_val <- data.frame(dimension="nacional", datos[-id_quant,])
  
  # a <- boxplot(datos_j_val$SAL_SEM[-id_quant], main="Nacional", outline=T, horizontal=F)
  # b <- points(as.data.frame(prediccion_final), col=2, lwd=5, pch=19)
  
  ### BOXPLOT ESTATAL
  # Se filtra segun ENTidad
  # ==============================================================================
  id_ENT_j <- which(datos$ent==as.numeric(par_ENT))
  datos_ENT_j <- datos[id_ENT_j,]
  
  ### Se eliminan valores extremos basado en cuantiles
  quants_values <- quantile(datos_ENT_j$SAL_SEM, probs=seq(0,1, 0.25))
  id_quant <- which(datos_ENT_j$SAL_SEM>=quants_values[length(quants_values)-1])
  
  datos_ENT_j_val <- data.frame(dimension="entidad", datos_ENT_j[-id_quant,])
  
  
  # a <- boxplot(datos_ENT_j$SAL_SEM[-id_quant], main="Estatal", outline=T, horizontal=F)
  # b <- points(as.data.frame(prediccion_final), col=2, lwd=5, pch=19)
  
  ### BOXPLOT OFICIO
  # Se filtra segun ENTidad
  # ==============================================================================
  id_oficio_j <- which(datos$TIPO_TRABAJO==as.numeric(pars_usuario_t$TIPO_TRABAJO))
  datos_oficio_j <- datos[id_oficio_j,]
  
  ### Se eliminan valores extremos basado en cuantiles
  quants_values <- quantile(datos_oficio_j$SAL_SEM, probs=seq(0,1, 0.25))
  id_quant <- which(datos_oficio_j$SAL_SEM>=quants_values[length(quants_values)-1])
  
  datos_oficio_j_val <- data.frame(dimension="oficio", datos_oficio_j[-id_quant,])
  
  
  # a <- boxplot(datos_oficio_j$SAL_SEM[-id_quant], main="TIPO_TRABAJO", outline=T, horizontal=F)
  # b <- points(as.data.frame(prediccion_final), col=2, lwd=5, pch=19)
  
  ### BOXPLOT OFICIO ESTATAL
  # Se filtra segun ENTidad
  # ==============================================================================
  id_ENT_oficio_j <- which(datos[id_ENT_j,]$TIPO_TRABAJO==as.numeric(pars_usuario_t$TIPO_TRABAJO))
  datos_ENT_oficio_j <- datos[id_ENT_j,][id_ENT_oficio_j,]
  
  ### Se eliminan valores extremos basado en cuantiles
  quants_values <- quantile(datos_ENT_oficio_j$SAL_SEM, probs=seq(0,1, 0.25))
  id_quant <- which(datos_ENT_oficio_j$SAL_SEM>=quants_values[length(quants_values)-1])
  
  datos_ENT_oficio_j_val <- data.frame(dimension="entidad_oficio", datos_ENT_oficio_j[-id_quant,])
  
  a <- boxplot(datos_ENT_oficio_j$SAL_SEM[-id_quant], main="Oficio", outline=T, horizontal=F)
  b <- points(as.data.frame(prediccion_final), col=2, lwd=5, pch=19)
  
  ### se genera un solo dataframe que servira para el boxplot conjunto
  
  datos_boxplot <- rbind(datos_j_val,
                         datos_ENT_j_val,
                         datos_oficio_j_val,
                         datos_ENT_oficio_j_val)
  
  datos_boxplot$dimension <- factor(datos_boxplot$dimension , levels=c("nacional", "entidad", "oficio", "entidad_oficio"))
  
  ## BOXPLOT
  # View(datos_boxplot)
  y_value <- as.numeric(as.data.frame(prediccion_final))
  boxplot <- datos_boxplot %>%
    ggplot( aes(x=dimension, y=SAL_SEM, fill=dimension)) +
    theme(legend.position="none")+
    geom_boxplot() +
    geom_point(aes(x=1, y=y_value), col="red", size=3)+
    geom_point(aes(x=2, y=y_value), col="red", size=3)+
    geom_point(aes(x=3, y=y_value), col="red", size=3)+
    geom_point(aes(x=4, y=y_value), col="red", size=3)
  
  
  ### Titulos y subtitulos
  # title(main = paste("Predicción del Salario:", round(as.data.frame(prediccion_final[,1]), 4), sep=" "))
  # mysubtitle = paste(modelo_optimo_name, " MSE:",round(min_mse_value, 4), " R2:", round(max_r2_value, 4), sep="")
  
  ## Se grafica la importancia de las variables
  if(modelo_optimo_name=="Xgboost" | modelo_optimo_name=="Randomforest" | modelo_optimo_name=="Neural_network"){
    titulo_text <- "Relevancia de variables"
  }else{
    titulo_text <- "Coeficientes del modelo lineal"
  }
  
  
  coef_grafica <- coeficientes %>%
    ggplot(aes(x = reorder(variable, -coef_importancia), y = coef_importancia)) +
    geom_col() +
    labs(title = titulo_text) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10, angle =0))
  # barplot(coeficientes[,2], xlab=coeficientes[,1])
  
  # print(coef_grafica)
  
  
  ### SE GRAFICA
  require(gridExtra)
  
  grid.arrange(boxplot, coef_grafica, nrow=2)
  
  
  return(resultado)
  
}

# undebug(fn_prediction)
prediccion <- fn_prediction(pars_usuario_t,
                            fn_modelado_res$formula_i_global,
                            datos,
                            par_y_log=T,
                            fn_modelado_res)