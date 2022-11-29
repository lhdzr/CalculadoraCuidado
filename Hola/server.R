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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    #AMAI

    output$txtOutput = renderText({
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
      
      paste0("Tu Nivel Socioeconómico (NSE) según la Regla AMAI es: ", Clasif_AMAI)
    
    
    
  })


})
  
