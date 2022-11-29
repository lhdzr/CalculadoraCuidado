#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(dplyr)
library(ggpubr)
library(shiny)
library(ggplot2)
library(patchwork)
enoe3 <- read.csv("../data/baseenoe.csv")
load("equipo_3_modelado.R")
tab_ent = data.frame(EST=c("Aguascalientes","Baja California", 
                             "Baja California Sur", "Campeche", 
                             "Coahuila", "Colima", "Chiapas", 
                             "Chihuahua", "Durango", "Distrito Federal", 
                             "Guanajuato", "Guerrero", "Hidalgo", 
                             "Jalisco", "México", "Michoacán", "Morelos", 
                             "Nayarit", "Nuevo León", "Oaxaca", "Puebla", 
                             "Querétaro", "Quintana Roo", "San Luis Potosí", 
                             "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", 
                             "Tlaxcala", "Veracruz", "Yucatán","Zacatecas"),
                     ENT = c(1:32))
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
pars_usuario <- data.frame(pars=colnames(datos)[-c(1)], value=NA)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  pars_usuario[1,"value"] = ifelse(input$Sexo=="Mujer",1,0)# SEXO
  pars_usuario[2,"value"] = input$Edad # EDAD
  pars_usuario[3,"value"] = tab_ent$ENT[tab_ent$EST==input$Estado]#ENT
  pars_usuario[4,"value"] = input$Escolaridad#ESC
  pars_usuario[5,"value"] = ifelse(input$Urbano=="Urbano",1,0)#urbano
  pars_usuario[6,"value"] = #TIEMPO_TOTAL
  pars_usuario[7,"value"] = tab_est_civ$NUM[tab_est_civ$EST==input$EstadoCivil] #SIT_CONYUGAL
  pars_usuario[8,"value"] = ifelse(input$Indigena=="Sí",1,0)#INDIGENA
  pars_usuario[9,"value"] = #TIPO_TRABAJO
  pars_usuario[10,"value"] = ifelse(input$ProgramaSocial=="Sí",1,0)#PROGRAMA_SOCIAL
  pars_usuario[11,"value"] = ifelse(input$Afiliacion=="Sí",1,0)#SEGURO_MEDICO
  pars_usuario[12,"value"] = tab_paren$NUM[tab_paren$PAREN==input$Paren]#PAREN
  pars_usuario[13,"value"] = #TIEMPO_CHOF    
  pars_usuario[14,"value"] = #TIEMPO_COCIN
  pars_usuario[15,"value"] = #TIEMPO_ENF_TEC
  pars_usuario[16,"value"] = #TIEMPO_FISIO
  pars_usuario[17,"value"] = #TIEMPO_CUID
  pars_usuario[18,"value"] = input$nPersonas#NUM_PER
  pars_usuario[19,"value"] = input$EscJefe#esc_jefe
  pars_usuario[20,"value"] = ifelse(input$SexoJefe=="Mujer",1,0)#jefatura_femenina
  pars_usuario[21,"value"] = input$EdadJefe#edad_jefe
    #AMAI
    Puntos_nivel<- ifelse(input$Esc_jefe=="No estudió",0,
                     ifelse(input$Esc_jefe=="Primaria incompleta",6,
                     ifelse(input$Esc_jefe=="Primaria completa",11,
                     ifelse(input$Esc_jefe=="Secundaria incompleta",12,
                     ifelse(input$Esc_jefe=="Secundaria completa",18,
                     ifelse(input$Esc_jefe=="Preparatoria incompleta",23,
                     ifelse(input$Esc_jefe=="Preparatoria completa",27,
                     ifelse(input$Esc_jefe=="Licenciatura incompleta",36,
                     ifelse(input$Esc_jefe=="Licenciatura completa",59,
                     ifelse(input$Esc_jefe=="Diplomado o maestría",85, 0))))))))))
      
      Puntos_automovil<- ifelse(input$Numeroautos == 1, 22,
                         ifelse(input$Numeroautos >= 2, 43, 0))
      
      Puntos_baño<- ifelse(input$Numerobaños == 1, 24, 
                    ifelse(input$Numerobaños >= 2, 47,0))
    
      Puntos_internet<- ifelse(input$Internet == "Si tengo", 32, 0)
      
      Puntos_trab<- ifelse(input$Numerotrabajadores ==0, 0,
                    ifelse(input$Numerotrabajadores ==1, 15,
                    ifelse(input$Numerotrabajadores ==2, 31,
                    ifelse(input$Numerotrabajadores ==3, 46,
                    ifelse(input$Numerotrabajadores >=4, 61, 61)))))
      
      Puntos_cuartos<- ifelse(input$Numerocuartos==1, 8,
                       ifelse(input$Numerocuartos==2, 16,
                       ifelse(input$Numerocuartos==3, 24, 
                       ifelse(input$Numerocuartos>=4, 32, 0))))
      
      Puntaje_AMAI <- Puntos_nivel+Puntos_automovil+Puntos_baño+Puntos_internet+Puntos_trab+Puntos_cuartos
      
      Clasif_AMAI<- ifelse(Puntaje_AMAI>=0 & Puntaje_AMAI<=47, "E",
                    ifelse(Puntaje_AMAI>=48 & Puntaje_AMAI<=94, "D",
                    ifelse(Puntaje_AMAI>=95 & Puntaje_AMAI<=115, "D+",
                    ifelse(Puntaje_AMAI>=116 & Puntaje_AMAI<=140, "C-",
                    ifelse(Puntaje_AMAI>=141 & Puntaje_AMAI<=167, "C",
                    ifelse(Puntaje_AMAI>=168 & Puntaje_AMAI<=201, "C+",
                    ifelse(Puntaje_AMAI>=202, "A/B", "NA")))))))

      output$txtOutput = renderText({
     paste0("Tu Nivel Socioeconómico (NSE) según la Regla AMAI es: ", Clasif_AMAI)})
    output$ingreso = renderText({
      
    })
      
      paste0("Tu Nivel Socioeconómico (NSE) según la Regla AMAI es: ", Clasif_AMAI)
    
    
    
  })

  
