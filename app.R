#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(ggpubr)
library(shiny)
library(ggplot2)
library(patchwork)
####cargar librerias
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
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("yeti"),

    # Application title
    titlePanel(h2("Calculadora de salario", align = "center")),
    h4("Calculá cuánto vale el trabajo no remunerado", align = "center"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(selectInput("Estado",
                                 "Estado de residencia",
                                 c("Aguascalientes","Baja California", 
                                   "Baja California Sur", "Campeche", 
                                   "Coahuila", "Colima", "Chiapas", 
                                   "Chihuahua", "Durango", "Distrito Federal", 
                                   "Guanajuato", "Guerrero", "Hidalgo", 
                                   "Jalisco", "México", "Michoacán", "Morelos", 
                                   "Nayarit", "Nuevo León", "Oaxaca", "Puebla", 
                                   "Querétaro", "Quintana Roo", "San Luis Potosí", 
                                   "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                                   "Tlaxcala", "Veracruz", "Yucatán","Zacatecas")),
                     radioButtons("Sexo",
                                  "Sexo",
                                  c("Hombre","Mujer")),
                     sliderInput("Escolaridad",
                                 "Años de escolaridad",
                                 0,
                                 30,
                                 9),
                     numericInput(inputId="Edad",
                                  label="Edad",
                                  20,
                                  12,
                                  98),
                     selectInput("EstadoCivil",
                                 "Estado Civil",
                                 c("Pareja en unión libre",
                                   "Separado",
                                   "Divorciado",
                                   "Viudo",
                                   "Casado",
                                   "Soltero")),
                     radioButtons("Urbano",
                                  "¿Vives en un entorno rural o urbano?",
                                  c("Rural","Urbano")),
                     radioButtons("Indigena",
                                  "¿Se considera indígena?",
                                  c("Sí","No")),
                     radioButtons("Afiliacion",
                                  "¿Está afiliado a algún servicio médico?",
                                  c("Sí","No")),
                     radioButtons("ProgramaSocial",
                                  "¿Recientemente tramitó o cobró un programa social?",
                                  c("Sí","No")),
                     selectInput("Tipotrabajo",
                                 "En su trabajo de la semana pasada, ¿usted fue?",
                                 c("Empleado(a) u obrero(a)",
                                   "Trabajador(a) sin pago en un negocio familiar o no familia",
                                   "Jornalero(a) o peón",
                                   "Trabajador(a) por mi cuenta",
                                   "Patrón(a) o empleado")),
                     numericInput("nPersonas",
                                  "¿Cuántos integrantes conforman tu hogar? (Incluyéndote a ti)",
                                  1,
                                  20),
                     selectInput("Paren",
                                 "¿Qué es usted del jefe de hogar?",
                                 c("Soy jefe de hogar",
                                   "Cónyuge/Pareja",
                                   "Hijo/Hija",
                                   "Nieto",
                                   "Yerno/Nuera",
                                   "Padre/Madre/Suegro",
                                   "Otro pariente",
                                   "Sin parentezco")),
                     radioButtons("SexoJefe",
                                  "El/la jefe de tu hogar es:",
                                  c("Hombre", "Mujer")),
                     numericInput("EdadJefe",
                                  "Edad del jefe del hogar",
                                  12,
                                  98),
                     sliderInput("EscJefe",
                                 "Escolaridad del jefe de hogar",
                                 0,
                                 30,
                                 9),
                     numericInput("chofer1",
                                  "Recoger o esperar a algún adulto mayor para que recibiera atención de salud",
                                  value=0,
                                  step = 0.5),
                     numericInput("cuidador1",
                                  "Apoyar o asesorar a algún adulto mayor en el uso de la computadora, celular, internet o actividades relacionadas a cursos o clases",
                                  value=0,
                                  step = 0.5),
                     numericInput("chofer2",
                                  "Llevar y/o recoger a algún adulto mayor del trabajo, de algún trámite u otro lugar",
                                  value=0,
                                  step = 0.5),    
                     numericInput("cuidador2",
                                  "Cuidar o estar al pendiente de algún adulto mayor, mientras hacía otra cosa",
                                  value=0,
                                  step = 0.5),
                     numericInput("enfermero1",
                                  "Dar de comer o ayudar a algún adulto mayor a hacerlo",
                                  value=0,
                                  step = 0.5), 
                     numericInput("cuidador3",
                                  "Asear, vestir, arreglar o ayudar a algún adulto mayor a hacerlo",
                                  value=0,
                                  step = 0.5), 
                     numericInput("cuidador4",
                                  "Cargar, acostar o ayudar a algún adulto mayor a hacerlo",
                                  value=0,
                                  step = 0.5),
                     numericInput("cocinero1",
                                  "Preparar remedios caseros o algún alimento especial a algún adulto mayor",
                                  value=0,
                                  step = 0.5),
                     numericInput("enfermero2",
                                  "Dar medicamentos o checar los síntomas de algún adulto mayor",
                                  value=0,
                                  step = 0.5),
                     numericInput("fisio1",
                                  "Dar terapia especial o ayudar a realizar ejercicios a algún adulto mayor",
                                  value=0,
                                  step = 0.5),
                     numericInput("cuidador5",
                                  "Ayudar o apoyar en el trabajo de algún adulto mayor",
                                  value=0,
                                  step = 0.5),
                     submitButton("Ejecutar")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({

        tab_ent = data.frame(ENT=c("Aguascalientes","Baja California",
                                   "Baja California Sur", "Campeche",
                                   "Coahuila", "Colima", "Chiapas",
                                   "Chihuahua", "Durango", "Distrito Federal",
                                   "Guanajuato", "Guerrero", "Hidalgo",
                                   "Jalisco", "México", "Michoacán", "Morelos",
                                   "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
                                   "Querétaro", "Quintana Roo", "San Luis Potosí",
                                   "Sinaloa", "Sonora", "Tabasco", "Tamaulipas",
                                   "Tlaxcala", "Veracruz", "Yucatán","Zacatecas"),
                             NUM = c(1:32))
        tab_est_civ = data.frame(EST=c("Pareja en unión libre",
                                       "Separado",
                                       "Divorciado",
                                       "Viudo",
                                       "Casado",
                                       "Soltero"),
                                 NUM=c(1:6))
        tab_paren = data.frame(PAREN=c("Soy jefe de hogar",
                                       "Cónyuge/Pareja",
                                       "Hijo/Hija",
                                       "Nieto",
                                       "Yerno/Nuera",
                                       "Padre/Madre/Suegro",
                                       "Otro pariente",
                                       "Sin parentezco"),
                               NUM=c(1:8))
        tab_tipo = data.frame(TIPOTRAB=c("Empleado(a) u obrero(a)",
                                         "Trabajador(a) sin pago en un negocio familiar o no familia",
                                         "Jornalero(a) o peón",
                                         "Trabajador(a) por mi cuenta",
                                         "Patrón(a) o empleado"),
                              NUM=c(1:5))
        # 
        # ### 2) Descargar los resultados del modelado
        # 
        #setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/1T/TEC 2022 - Concentración/RETOS/Equipo 3")
        setwd("C:/Users/hecto/Documents/Finanzas clase/ChileTextMining")
        
        load("equipo_3_environment.R")

        ### 3) Capturar datos
        # ## a) formato "normal" para modelos OLS y STEP
        id_sample <-sample(1:nrow(datos), 1, F)
        # 37917
        pars_usuario_t <- datos[id_sample,,drop=F][,-1] ## debe llenarse con los datos nuevos (parametros del usuario). Para ejemplificar se considera una de las observaciones de la base de datos

        # # ### iNputs
        pars_usuario_t[1,1] = ifelse(input$Sexo=="Mujer",1,0)# SEXO
        pars_usuario_t[1,2] = input$Edad # EDAD
        pars_usuario_t[1,3] = tab_ent$NUM[tab_ent$ENT==input$Estado]#ENT
        pars_usuario_t[1,4] = input$Escolaridad#ESC
        pars_usuario_t[1,5] = ifelse(input$Urbano=="Urbano",1,0)#urbano
        pars_usuario_t[1,6] = input$chofer1+input$chofer2+input$cocinero1+input$enfermero1+input$enfermero2+input$fisio1+input$cuidador1+input$cuidador2+input$cuidador3+input$cuidador4+input$cuidador5#TIEMPO_TOTAL
        pars_usuario_t[1,7] = tab_est_civ$NUM[tab_est_civ$EST==input$EstadoCivil] #SIT_CONYUGAL
        pars_usuario_t[1,8] = ifelse(input$Indigena=="Sí",1,0)#INDIGENA
        pars_usuario_t[1,9] = tab_tipo$NUM[tab_tipo$TIPOTRAB==input$Tipotrabajo] #TIPO_TRABAJO
        pars_usuario_t[1,10] = ifelse(input$Afiliacion=="Sí",1,0)#SEGURO_MEDICO
        pars_usuario_t[1,11] = tab_paren$NUM[tab_paren$PAREN==input$Paren]#PAREN
        pars_usuario_t[1,12] = input$chofer1+input$chofer2#TIEMPO_CHOF
        pars_usuario_t[1,13] = input$cocinero1 #TIEMPO_COCIN
        pars_usuario_t[1,14] = input$enfermero1+input$enfermero2 #TIEMPO_ENF_TEC
        pars_usuario_t[1,15] = input$fisio1 #TIEMPO_FISIO
        pars_usuario_t[1,16] = input$cuidador1+input$cuidador2+input$cuidador3+input$cuidador4+input$cuidador5#TIEMPO_CUID
        pars_usuario_t[1,17] = input$nPersonas#NUM_PER
        pars_usuario_t[1,18] = input$EscJefe#esc_jefe
        pars_usuario_t[1,19] = ifelse(input$SexoJefe=="Mujer",1,0)#jefatura_femenina
        pars_usuario_t[1,20] = input$EdadJefe#edad_jefe
        
        # plot(1:pars_usuario_t[1,2])
        
        ### PREDICCION
        ### FUNCION
        fn_prediction <- function(pars_usuario_t,
                                  formula_i_global,
                                  datos,
                                  par_y_log,
                                  fn_modelado_res){


            # ## @pars
            # par_y_log=T
            # formula_i_global <- fn_modelado_res$formula_i



            ###############################################################################
            ### -- > A partir de aqui se corre en el sistema
            ################################################################################


            ### SE LE DA FORMATO A CADA UNA DE LAS VARIABLES (PARAMETROS) CONSISTETE CON LA BD DE ENTRENAMIENTO

            pars_usuario_t$SEXO<- as.factor(pars_usuario_t$SEXO)
            pars_usuario_t$urbano <- as.factor(pars_usuario_t$urbano)
            pars_usuario_t$INDIGENA <- as.factor(pars_usuario_t$INDIGENA)
            # pars_usuario_t$PROGRAMA_SOCIAL <- as.factor(pars_usuario_t$PROGRAMA_SOCIAL)
            pars_usuario_t$SEGURO_MEDICO <- as.factor(pars_usuario_t$SEGURO_MEDICO)
            pars_usuario_t$PAREN <- as.factor(pars_usuario_t$PAREN)
            pars_usuario_t$jefatura_femenina <- as.factor(pars_usuario_t$jefatura_femenina)
            pars_usuario_t$TIPO_TRABAJO <- as.factor(pars_usuario_t$TIPO_TRABAJO)
            pars_usuario_t$SIT_CONYUGAL <- as.factor(pars_usuario_t$SIT_CONYUGA)


            ### se agrega un valor artificial a la primera columan (variable Y) solo para obtener la siguiente matriz que necesitan los modelos Lasso y Ridge
            pars_usuario_t <- cbind("SAL_SEM"=1, pars_usuario_t)

            ### se crea la formula para obtener la matriz especifica para modelos Lasso y Ridge
            pars_usuario_t_lasso_ridge <- model.matrix(formula_i_global, data = datos[1,])[,-1, drop=F]

            ## se renombran objetos
            # class(pars_usuario_t$urbano)
            x_new <- pars_usuario_t
            # lapply(x_new, class)
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
            r2_df_plot <- fn_modelado_res$r2[[par_ENT]]

           
            ### filtro del top 10 de coeficientes
            if(nrow(coeficientes)>10){
              coeficientes <- coeficientes[1:10,]
            }
            

            ### 4. Prediccion con el mejor modelo seleccionado

            # if(modelo_optimo_name=="Lasso" | modelo_optimo_name=="Ridge"){
            #
            #   id_modelo_optimo <-  which(fn_modelado_res$mse[[par_ENT]]$modelo=="OLS")
            #   prediction <- predict(fn_modelado_res$modelos[[par_ENT]][[id_modelo_optimo]], newdata = x_new)
            # }

            if(modelo_optimo_name=="OLS" | modelo_optimo_name=="Stepwise" ){

                prediction <- try(predict(fn_modelado_res$modelos[[par_ENT]][[id_modelo_optimo]], newdata = x_new),silent=T)
                ### COEFICIENTES (VALOR DE BETAS O NIVEL DE IMPORTANCIA)
                coeficientes <- fn_modelado_res$coef[[par_ENT]][[id_modelo_optimo]]
                coeficientes<- data.frame(variable=rownames(coeficientes),coef_importancia=coeficientes[,1])
                #colnames(coeficientes)[1] <- c("variable", "coef_importancia")
                
              
            }
            # if(modelo_optimo_name=="Lasso" | modelo_optimo_name=="Ridge"){
            #   prediction <- predict(fn_modelado_res$modelos[[par_ENT]][[id_modelo_optimo]], newx = x_new_lasso_ridge)
            # }

            if(modelo_optimo_name=="Xgboost" | modelo_optimo_name=="Randomforest" | modelo_optimo_name=="Neural_network"){
                ### En caso de que se elija el modelo H2O se requiere otro formato
                # lapply(x_new, class)
                x_new_h2o <- as.h2o(x_new)
                prediction <- try(predict(fn_modelado_res$modelos[[par_ENT]][[id_modelo_optimo]], newdata = x_new_h2o),silent=T)
                
                if(class(prediction)=="try-error"){
                  prediction <- try(predict(fn_modelado_res$modelos[[par_ENT]][[1]], newdata = x_new),silent=T)
                  id_modelo_optimo=1
                  ### COEFICIENTES (VALOR DE BETAS O NIVEL DE IMPORTANCIA)
                  coeficientes <- fn_modelado_res$coef[[par_ENT]][[id_modelo_optimo]]
                  coeficientes<- data.frame(variable=rownames(coeficientes),coef_importancia=coeficientes[,1])
                  #colnames(coeficientes)[1] <- c("variable", "coef_importancia")
                }else{
                  prediction <- as.data.frame(prediction)
                  ### COEFICIENTES (VALOR DE BETAS O NIVEL DE IMPORTANCIA)
                  coeficientes <- fn_modelado_res$coef[[par_ENT]][[id_modelo_optimo]]
                  #coeficientes<- data.frame(variable=rownames(coeficientes),coef_importancia=coeficientes[,1])
                  colnames(coeficientes)[1] <- c("variable", "coef_importancia")
                }
                
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
            # ### stackedbar de MSE
            # library(RColorBrewer)
            # coul <- brewer.pal(3, "Pastel2") 
            # 
            # data <- fn_modelado_res$r2[[par_ENT]]$r2
            # data <- rbind(data, 1-data)
            # colnames(data) <- fn_modelado_res$r2[[par_ENT]]$modelo
            # stacked_barplot_r2 <- barplot(data, col=coul , border="white", xlab="group")
            # 
            # data <- ((fn_modelado_res$mse[[par_ENT]]$mse))
            # names(data) <- fn_modelado_res$mse[[par_ENT]]$modelo
            # # class(data)
            # stacked_barplot_mse <- barplot(data, col=coul, border="white", xlab="group")

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

            # a <- boxplot(datos_ENT_oficio_j$SAL_SEM[-id_quant], main="Oficio", outline=T, horizontal=F)
            # b <- points(as.data.frame(prediccion_final), col=2, lwd=5, pch=19)

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
              ggtitle(paste0("Tu salario semanal según la ENUT es", y_value))+
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
            #stacked_barplot_r2,stacked_barplot_mse,

            return(resultado)

        }
        # undebug(fn_prediction)
        prediccion <- fn_prediction(pars_usuario_t,
                                    fn_modelado_res$formula_i,
                                    datos,
                                    par_y_log=T,
                                    fn_modelado_res)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
