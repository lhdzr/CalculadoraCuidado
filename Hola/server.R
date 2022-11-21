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

  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- enoe3$sal_sem
    columnas <- seq(min(x), max(x), length.out = input$columnas + 1)
    
    
    
    
    # draw the histogram with the specified number of bins
    
    
    enoe_cuidadores = filter(enoe3,p3==5222)
    enoe_choferes = filter(enoe3,p3==8343)
    enoe_enfermeras = filter(enoe3,p3==2811)
    enoe_fisioterapeutas = filter(enoe3,p3==2812)
    enoe_cocineros = filter(enoe3,p3==5113)
    enoe_mandaderos = filter(enoe3,p3==9722)
    x1    <- enoe_cuidadores$sal_sem
    x2    <- enoe_choferes$sal_sem
    x3    <- enoe_cocineros$sal_sem
    x4    <- enoe_enfermeras$sal_sem
    x5    <- enoe_fisioterapeutas$sal_sem
    x6    <- enoe_mandaderos$sal_sem
    columnas1 <- seq(min(x1), max(x1), length.out = input$columnas1 + 1)
    columnas2 <- seq(min(x2), max(x2), length.out = input$columnas2 + 1)
    columnas3 <- seq(min(x3), max(x3), length.out = input$columnas3 + 1)
    columnas4 <- seq(min(x4), max(x4), length.out = input$columnas4 + 1)
    columnas5 <- seq(min(x5), max(x5), length.out = input$columnas5 + 1)
    columnas6 <- seq(min(x6), max(x6), length.out = input$columnas6 + 1)
    par(mfcol = c(3, 3))
    hist0<-hist(x, breaks = columnas, col = 'black', border = 'white',
                xlab = 'Salario semanal',
                main = 'Histograma de salarios general')
    hist1<-hist(x1, breaks = columnas1, col = 'blue', border = 'white',
                xlab = 'Salario semanal cuidadores',
                main = 'Histograma de salarios cuiadadores')
    hist2<-hist(x2, breaks = columnas2, col = 'purple', border = 'white',
                xlab = 'Salario semanal choferes',
                main = 'Histograma de salarios choferes')
    hist3<-hist(x3, breaks = columnas3, col = 'yellow', border = 'white',
                xlab = 'Salario semanal cocineros',
                main = 'Histograma de salarios cocineros')
    hist4<-hist(x4, breaks = columnas4, col = 'red', border = 'white',
                xlab = 'Salario semanal enfermeras',
                main = 'Histograma de salarios enfermeras')
    hist5<-hist(x5, breaks = columnas5, col = 'green', border = 'white',
                xlab = 'Salario semanal fisioterapeutas',
                main = 'Histograma de salarios fisioterapeutas')
    hist6<-hist(x6, breaks = columnas6, col = 'orange', border = 'white',
                xlab = 'Salario semanal mandaderos',
                main = 'Histograma de salarios mandaderos')
    
    
    
    
    
  })

})
