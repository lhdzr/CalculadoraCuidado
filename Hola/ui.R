#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Mi primer Shiny"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId="Infectivity",
                        label="Infectividad:",
                        min = 0,
                        max = 1,
                        step = 0.01,
                        value = 0.1),
            sliderInput("Contact.Frequency",
                        "Frecuencia de contacto:",
                        min=0,
                        max=10,
                        value= 2, 
                        step= 1, 
                        animate=FALSE),
            radioButtons("finaltime",
                         "Tiempo final",
                         choices= c("Tiempo 1" = "60",
                                    "Tiempo 2" = "120",
                                    "Tiempo 3" = "200"),
                         selected = "120")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))

