# Define UI for application that draws a histogram

library(shinythemes)
ui <- navbarPage("PREDICCION DEL SALARIO MINIMO",
                 
                 tabPanel("Análsis",fluidPage(theme = shinytheme("flatly")),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(
                            headerPanel('Características (Parámetros)'),
                            sidebarPanel(width = 4,
                                         selectInput('ent', 'Seleccione la entidad:',1:32),
                                         selectInput('mujer', 'Seleccione el género:', 1:2),
                                         selectInput('rural', 'Seleccione el ámbito:',1:2),
                                         selectInput(inputId='Oficio', 
                                                     label='Seleccione el oficio:',
                                                     1:5),
                                         selectInput('emp_ppal', 'Seleccione el tipo de:',1:2),
                                         sliderInput("hrsocup", "Seleccione las horas trabajadas:",
                                                     min = 0, max = 100,
                                                     value = c(50)),
                                         sliderInput("eda", "Seleccione la edad:",
                                                     min = 18, max = 100,
                                                     value = c(30)),
                                         sliderInput("anios_esc", "Seleccione los años de escolaridad:",
                                                     min = 0, max = 30,
                                                     value = c(10)),
                                         sliderInput("Experiencia", "Seleccione los años de experiencia:",
                                                     min = 0, max = 30,
                                                     value = c(10)),
                                         submitButton("Ejecutar")
                            ),
                                mainPanel(
                                  plotOutput("distPlot", width = 1000, height=900)
                                )
                          )),
             tabPanel("Metodología",p("We used a data set consisting of 39 attributes from 11,158 players registered
                          in Pro Evolution Soccer 2019 (PES 2019), an electronic soccer game. The data set
                          was obtained from ", a("PES Data Base", href="http://pesdb.net/", target="_blank"),
                          "website using web scraping. This app is an interactive tool that allows any user to choose a soccer player from the game
                         and find the ten players most similar whith him. The similarity between the players is determined using a data mining technique
                         called", a("k-nearest neighbors", href="https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm", target="_blank"), ".",style = "font-size:25px"),
                      
                      hr(), 
                      p("The available player positions are:",style = "font-size:25px"),
                      p("GK: Goalkeeper",style = "font-size:15px;color: blue"),
                      p("CB: Center Back",style = "font-size:15px;color: blue"),
                      p("RB: Right Back",style = "font-size:15px;color: blue"),
                      p("LB: Left Back",style = "font-size:15px;color: blue"),
                      p("DMF: Defense Midfield",style = "font-size:15px;color: blue"),
                      p("CMF: Center Midfield",style = "font-size:15px;color: blue"),
                      p("AMF: Attacking Midfield",style = "font-size:15px;color: blue"),
                      p("RMF: Right Midfield",style = "font-size:15px;color: blue"),
                      p("LMF: Left Midfield",style = "font-size:15px;color: blue"),
                      p("RWF: Right Wing Forward",style = "font-size:15px;color: blue"),
                      p("LWF: Left Wing Forward",style = "font-size:15px;color: blue"),
                      p("SS: Second Striker",style = "font-size:15px;color: blue"),
                      p("CF: Counter Forward",style = "font-size:15px;color: blue"),
                      hr(), 
                      
                      p("The abbreviations used in the radar chart are:",style = "font-size:25px"),
                      
                      p("BAL: Unwavering Balance",style = "font-size:15px;color: blue"),
                      p("STM: Stamina",style = "font-size:15px;color: blue"),
                      p("SPE: Speed",style = "font-size:15px;color: blue"),
                      p("EXP: Explosive Power",style = "font-size:15px;color: blue"),
                      p("ATT: Attacking Prowess",style = "font-size:15px;color: blue"),
                      p("BCO: Ball Control",style = "font-size:15px;color: blue"),
                      p("DRI: Dribbling",style = "font-size:15px;color: blue"),
                      p("LPAS: Low Pass",style = "font-size:15px;color: blue"),
                      p("APAS: Air Pass (Lofted Pass)",style = "font-size:15px;color: blue"),
                      p("KPOW: Kicking Power",style = "font-size:15px;color: blue"),
                      p("FIN: Finishing",style = "font-size:15px;color: blue"),
                      p("PKIC: Place Kicking",style = "font-size:15px;color: blue"),
                      p("SWE: Swerve",style = "font-size:15px;color: blue"),
                      p("HEA: Header",style = "font-size:15px;color: blue"),
                      p("JUM: Jump",style = "font-size:15px;color: blue"),
                      p("PHY: Physical Contact",style = "font-size:15px;color: blue"),
                      p("BWIN: Ball Winning",style = "font-size:15px;color: blue"),
                      p("DEF: Defensive Prowess",style = "font-size:15px;color: blue"),
                      p("GOA: Goalkeeping",style = "font-size:15px;color: blue"),
                      p("GKC: GK Catch",style = "font-size:15px;color: blue"),
                      p("CLE: Clearing",style = "font-size:15px;color: blue"),
                      p("REF: Reflexes",style = "font-size:15px;color: blue"),
                      p("COV: Coverage",style = "font-size:15px;color: blue")),
             tabPanel("Equipo",
                      p(a("Thiago Valentim Marques", href="http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4201666A2", target="_blank"),style = "font-size:25px"),
                      p("e-mail: thiagomadridd@gmail.com",style = "font-size:20px"),
                      p(a("Julio Cesar Soares", href="http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4417495Y5", target="_blank"),style = "font-size:25px"),
                      p("email: soares.julio@gmail.com",style = "font-size:20px"),
                      p(a("Francisco Caninde Assis de Oliveira", href="http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K8219531A6", target="_blank"),style = "font-size:25px"),
                      p("e-mail: frecs123@gmail.com",style = "font-size:20px")))

# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("Modelo predictivo del Salario"),
#   
#   # Sidebar with a slider input for number of bins 
#   sidebarLayout(
#     sidebarPanel(
#       selectInput('ent', 'Seleccione la entidad:',1:32),
#       selectInput('mujer', 'Seleccione el género:', 1:2),
#       selectInput('rural', 'Seleccione el ámbito:',1:2),
#       selectInput(inputId='Oficio', 
#                   label='Seleccione el oficio:',
#                   1:5),
#       selectInput('emp_ppal', 'Seleccione el tipo de:',1:2),
#       sliderInput("hrsocup", "Seleccione las horas trabajadas:",
#                   min = 0, max = 100,
#                   value = c(50)),
#       sliderInput("eda", "Seleccione la edad:",
#                   min = 18, max = 100,
#                   value = c(30)),
#       sliderInput("anios_esc", "Seleccione los años de escolaridad:",
#                   min = 0, max = 30,
#                   value = c(10)),
#       sliderInput("Experiencia", "Seleccione los años de experiencia:",
#                   min = 0, max = 30,
#                   value = c(10)),
#       
#       submitButton("Update filters")
#     ),
#     
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("distPlot", width = 1000, height=900)
#     )
#   )
# )tabPanel("About",p("We used a data set consisting of 39 attributes from 11,158 players registered
               #            in Pro Evolution Soccer 2019 (PES 2019), an electronic soccer game. The data set
               #            was obtained from ", a("PES Data Base", href="http://pesdb.net/", target="_blank"),
               #            "website using web scraping. This app is an interactive tool that allows any user to choose a soccer player from the game
               #           and find the ten players most similar whith him. The similarity between the players is determined using a data mining technique
               #           called", a("k-nearest neighbors", href="https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm", target="_blank"), ".",style = "font-size:25px"),
               # 
               #      hr(),
               #      p("The available player positions are:",style = "font-size:25px"),
               #      p("GK: Goalkeeper",style = "font-size:15px;color: blue"),
               #      p("CB: Center Back",style = "font-size:15px;color: blue"),
               #      p("RB: Right Back",style = "font-size:15px;color: blue"),
               #      p("LB: Left Back",style = "font-size:15px;color: blue"),
               #      p("DMF: Defense Midfield",style = "font-size:15px;color: blue"),
               #      p("CMF: Center Midfield",style = "font-size:15px;color: blue"),
               #      p("AMF: Attacking Midfield",style = "font-size:15px;color: blue"),
               #      p("RMF: Right Midfield",style = "font-size:15px;color: blue"),
               #      p("LMF: Left Midfield",style = "font-size:15px;color: blue"),
               #      p("RWF: Right Wing Forward",style = "font-size:15px;color: blue"),
               #      p("LWF: Left Wing Forward",style = "font-size:15px;color: blue"),
               #      p("SS: Second Striker",style = "font-size:15px;color: blue"),
               #      p("CF: Counter Forward",style = "font-size:15px;color: blue"),
               #      hr(),
               # 
               #      p("The abbreviations used in the radar chart are:",style = "font-size:25px"),
               # 
               #      p("BAL: Unwavering Balance",style = "font-size:15px;color: blue"),
               #      p("STM: Stamina",style = "font-size:15px;color: blue"),
               #      p("SPE: Speed",style = "font-size:15px;color: blue"),
               #      p("EXP: Explosive Power",style = "font-size:15px;color: blue"),
               #           : Attacking Prowess",style = "font-size:15px;color: blue"),
               #      p("BCO: Ball Control",style = "font-size:15px;color: blue"),
               #      p("DRI: Dribbling",style = "font-size:15px;color: blue"),
               # p("LPAS: Low Pass",style = "font-size:15px;color: blue"),
               # p("APAS: Air Pass (Lofted Pass)",style = "font-size:15px;color: blue"),
               # p("KPOW: Kicking Power",style = "font-size:15px;color: blue"),
               # p("FIN: Finishing",style = "font-size:15px;color: blue"),
               # p("PKIC: Place Kicking",style = "font-size:15px;color: blue"),
               # p("SWE: Swerve",style = "font-size:15px;color: blue"),
               # p("HEA: Header",style = "font-size:15px;color: blue"),
               # p("JUM: Jump",style = "font-size:15px;color: blue"),
               # p("PHY: Physical Contact",style = "font-size:15px;color: blue"),
               # p("BWIN: Ball Winning",style = "font-size:15px;color: blue"),
               # p("DEF: Defensive Prowess",style = "font-size:15px;color: blue"),
               # p("GOA: Goalkeeping",style = "font-size:15px;color: blue"),
               # p("GKC: GK Catch",style = "font-size:15px;color: blue"),
               # p("CLE: Clearing",style = "font-size:15px;color: blue"),
               # p("REF: Reflexes",style = "font-size:15px;color: blue"),
               # p("COV: Coverage",style = "font-size:15px;color: blue")),
               # "Developers",
               # p(a("Thiago Valentim Marques", href="http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4201666A2", target="_blank"),style = "font-size:25px"),
               # p("e-mail: thiagomadridd@gmail.com",style = "font-size:20px"),
               # p(a("Julio Cesar Soares", href="http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4417495Y5", target="_blank"),style = "font-size:25px"),
               # p("email: soares.julio@gmail.com",style = "font-size:20px"),
               # p(a("Francisco Caninde Assis de Oliveira", href="http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K8219531A6", target="_blank"),style = "font-size:25px"),
               # p("e-mail: frecs123@gmail.com",style = "font-size:20px"))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$distPlot <- renderPlot({

    ###
    ### SE CARGA  LA SESION PRE-CARGADA CON LOS MODELOS Y BASES DE DATOS
    ### 
      fn_plots <- function(input){
        
        
        # plot(1:input$eda)
        
        
        # # install.packages("h2o")
        library(h2o)
        # # Start the H2O cluster (locally)
        h2o.init()
        
        load("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/1T/TEC 2022 - Concentración/RETOS/Equipo 1/equipo_1_environment.R")
        
        # se toma como base la BD para conservar todos los niveles posibles
        id_sample <-sample(1:nrow(datos), 1, F)
        pars_usuario_t <- datos[id_sample,,drop=F][,-1]
        # se trabaja con la Base de datos original para conservar todos los niveles de cada variable categorica
        
        # pars_usuario_t <- as.data.frame(t(data.frame(rep(NA, ncol(datos)-1))))
        # rownames(pars_usuario_t) <- NULL
        # colnames(pars_usuario_t) <- colnames(datos)[-1]
        # class(pars_usuario_t)
        
        # 2. Ent
        pars_usuario_t[1,1] <- input$ent ## valor capturado de RShiny
        # # 3. Edad
        pars_usuario_t[1,2] <- input$eda
        # # 4. Años escolaridad
        pars_usuario_t[1,3] <- input$anios_esc
        # # 5. Mujer
        pars_usuario_t[1,4] <- input$mujer
        # # 6. Experiencia
        pars_usuario_t[1,5] <- input$Experiencia
        # # 7. Horas ocupacion
        pars_usuario_t[1,6] <-input$hrsocup
        # # 8. Rural
        pars_usuario_t[1,7] <- input$rural
        # # 9. Oficio
        # pars_usuario_t[1,8] <- input$Oficio
        # # 10. Formalidad
        pars_usuario_t[1,9] <- input$emp_ppal
        
        fn_prediction <- function(pars_usuario_t,
                                  formula_i_global,
                                  datos,
                                  par_y_log,
                                  fn_modelado_res){
          
          # ## @pars
          # par_y_log=T
          
          ###############################################################################
          ### -- > A partir de aqui se corre en el sistema
          ################################################################################
          
          
          ### SE LE DA FORMATO A CADA UNA DE LAS VARIABLES (PARAMETROS) CONSISTETE CON LA BD DE ENTRENAMIENTO
          ## se le da formato a las variables categorias
          
          pars_usuario_t$ent <- as.factor(pars_usuario_t$ent)
          # levels(pars_usuario_t$ent) <- levels(datos$ent)
          
          pars_usuario_t$eda <- as.numeric(pars_usuario_t$eda)
          # levels(pars_usuario_t$eda) <- levels(datos$eda)
          
          pars_usuario_t$anios_esc <- as.numeric(pars_usuario_t$anios_esc)
          # levels(pars_usuario_t$anios_esc) <- levels(datos$anios_esc)
          
          pars_usuario_t$Experiencia <- as.numeric(pars_usuario_t$Experiencia)
          # levels(pars_usuario_t$Experiencia) <- levels(datos$Experiencia)
          
          pars_usuario_t$hrsocup <- as.numeric(pars_usuario_t$hrsocup)
          # levels(pars_usuario_t$hrsocup) <- levels(datos$hrsocup)
          
          pars_usuario_t$rural <- as.factor(pars_usuario_t$rural)
          # levels(pars_usuario_t$rural) <- levels(datos$rural)
          
          pars_usuario_t$mujer <- as.factor(pars_usuario_t$mujer)
          # levels(pars_usuario_t$mujer) <- levels(datos$mujer)
          
          pars_usuario_t$emp_ppal <- as.factor(pars_usuario_t$emp_ppal)
          # levels(pars_usuario_t$emp_ppal) <- levels(datos$emp_ppal)
          
          pars_usuario_t$Oficio <- as.factor(pars_usuario_t$Oficio)
          # levels(pars_usuario_t$Oficio) <- levels(datos$Oficio)
          
          ### se agrega un valor artificial a la primera columan (variable Y) solo para obtener la siguiente matriz que necesitan los modelos Lasso y Ridge
          pars_usuario_t <- cbind("ing_x_hrs"=1, pars_usuario_t)
          
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
          quants_values <- quantile(datos$ing_x_hrs, probs=seq(0,1, 0.25))
          id_quant <- which(datos$ing_x_hrs>=quants_values[length(quants_values)-1])
          
          datos_j_val <- data.frame(dimension="nacional", datos[-id_quant,])
          
          # a <- boxplot(datos_j_val$ing_x_hrs[-id_quant], main="Nacional", outline=T, horizontal=F)
          # b <- points(as.data.frame(prediccion_final), col=2, lwd=5, pch=19)
          
          ### BOXPLOT ESTATAL
          # Se filtra segun ENTidad
          # ==============================================================================
          id_ENT_j <- which(datos$ent==as.numeric(par_ENT))
          datos_ENT_j <- datos[id_ENT_j,]
          
          ### Se eliminan valores extremos basado en cuantiles
          quants_values <- quantile(datos_ENT_j$ing_x_hrs, probs=seq(0,1, 0.25))
          id_quant <- which(datos_ENT_j$ing_x_hrs>=quants_values[length(quants_values)-1])
          
          datos_ENT_j_val <- data.frame(dimension="entidad", datos_ENT_j[-id_quant,])
          
          
          # a <- boxplot(datos_ENT_j$ing_x_hrs[-id_quant], main="Estatal", outline=T, horizontal=F)
          # b <- points(as.data.frame(prediccion_final), col=2, lwd=5, pch=19)
          
          ### BOXPLOT OFICIO
          # Se filtra segun ENTidad
          # ==============================================================================
          id_oficio_j <- which(datos$Oficio==as.numeric(pars_usuario_t$Oficio))
          datos_oficio_j <- datos[id_oficio_j,]
          
          ### Se eliminan valores extremos basado en cuantiles
          quants_values <- quantile(datos_oficio_j$ing_x_hrs, probs=seq(0,1, 0.25))
          id_quant <- which(datos_oficio_j$ing_x_hrs>=quants_values[length(quants_values)-1])
          
          datos_oficio_j_val <- data.frame(dimension="oficio", datos_oficio_j[-id_quant,])
          
          
          # a <- boxplot(datos_oficio_j$ing_x_hrs[-id_quant], main="Oficio", outline=T, horizontal=F)
          # b <- points(as.data.frame(prediccion_final), col=2, lwd=5, pch=19)
          
          ### BOXPLOT OFICIO ESTATAL
          # Se filtra segun ENTidad
          # ==============================================================================
          id_ENT_oficio_j <- which(datos[id_ENT_j,]$Oficio==as.numeric(pars_usuario_t$Oficio))
          datos_ENT_oficio_j <- datos[id_ENT_j,][id_ENT_oficio_j,]
          
          ### Se eliminan valores extremos basado en cuantiles
          quants_values <- quantile(datos_ENT_oficio_j$ing_x_hrs, probs=seq(0,1, 0.25))
          id_quant <- which(datos_ENT_oficio_j$ing_x_hrs>=quants_values[length(quants_values)-1])
          
          datos_ENT_oficio_j_val <- data.frame(dimension="entidad_oficio", datos_ENT_oficio_j[-id_quant,])
          
          a <- boxplot(datos_ENT_oficio_j$ing_x_hrs[-id_quant], main="Oficio", outline=T, horizontal=F)
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
            ggplot( aes(x=dimension, y=ing_x_hrs, fill=dimension)) +
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
        
        prediccion <- fn_prediction(pars_usuario_t,
                                    formula_i_global,
                                    datos,
                                    par_y_log=T,
                                    fn_modelado_res)
        
        
      }
    fn_plots(input)

    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
