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

  # output$distPlot <- renderPlot({
  #   
  #   # generate bins based on input$bins from ui.R
  #   x    <- enoe3$sal_sem
  #   columnas <- seq(min(x), max(x), length.out = input$columnas + 1)
  #   
  #   
  #   # draw the histogram with the specified number of bins
  #   
  #   
  #   enoe_cuidadores = filter(enoe3,p3==5222)
  #   enoe_choferes = filter(enoe3,p3==8343)
  #   enoe_enfermeras = filter(enoe3,p3==2811)
  #   enoe_fisioterapeutas = filter(enoe3,p3==2812)
  #   enoe_cocineros = filter(enoe3,p3==5113)
  #   enoe_mandaderos = filter(enoe3,p3==9722)
  #   x1    <- enoe_cuidadores$sal_sem
  #   x2    <- enoe_choferes$sal_sem
  #   x3    <- enoe_cocineros$sal_sem
  #   x4    <- enoe_enfermeras$sal_sem
  #   x5    <- enoe_fisioterapeutas$sal_sem
  #   x6    <- enoe_mandaderos$sal_sem
  #   columnas1 <- seq(min(x1), max(x1), length.out = input$columnas1 + 1)
  #   columnas2 <- seq(min(x2), max(x2), length.out = input$columnas2 + 1)
  #   columnas3 <- seq(min(x3), max(x3), length.out = input$columnas3 + 1)
  #   columnas4 <- seq(min(x4), max(x4), length.out = input$columnas4 + 1)
  #   columnas5 <- seq(min(x5), max(x5), length.out = input$columnas5 + 1)
  #   columnas6 <- seq(min(x6), max(x6), length.out = input$columnas6 + 1)
  #   par(mfcol = c(3, 3))
  #   hist0<-hist(x, breaks = columnas, col = 'black', border = 'white',
  #               xlab = 'Salario semanal',
  #               main = 'Histograma de salarios general')
  #   hist1<-hist(x1, breaks = columnas1, col = 'blue', border = 'white',
  #               xlab = 'Salario semanal cuidadores',
  #               main = 'Histograma de salarios cuiadadores')
  #   hist2<-hist(x2, breaks = columnas2, col = 'purple', border = 'white',
  #               xlab = 'Salario semanal choferes',
  #               main = 'Histograma de salarios choferes')
  #   hist3<-hist(x3, breaks = columnas3, col = 'yellow', border = 'white',
  #               xlab = 'Salario semanal cocineros',
  #               main = 'Histograma de salarios cocineros')
  #   hist4<-hist(x4, breaks = columnas4, col = 'red', border = 'white',
  #               xlab = 'Salario semanal enfermeras',
  #               main = 'Histograma de salarios enfermeras')
  #   hist5<-hist(x5, breaks = columnas5, col = 'green', border = 'white',
  #               xlab = 'Salario semanal fisioterapeutas',
  #               main = 'Histograma de salarios fisioterapeutas')
  #   hist6<-hist(x6, breaks = columnas6, col = 'orange', border = 'white',
  #               xlab = 'Salario semanal mandaderos',
  #               main = 'Histograma de salarios mandaderos')
  #   
  # })
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
    output$ingreso = renderText({
      
    })

})
  
