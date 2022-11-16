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
    titlePanel("Calculadora de salario"),
    
    tabsetPanel(
      tabPanel("enut",
               titlePanel("Características personales"),
               flowLayout(selectInput("Estado",
                                      "Estado de residencia",
                                      c("Aguascalientes","Baja California")),
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
                                       "¿Vives en un entorno rural, o urbano?",
                                       c("Rural","Urbano")),
                          radioButtons("Indigena",
                                       "¿Se considera indígena?",
                                       c("Sí","No"))),
               titlePanel("Acceso a seguridad social"),
               flowLayout(
                 radioButtons("Afiliacion",
                              "¿Está afiliado a algún servicio médico?",
                              c("Sí","No")
               )),
               titlePanel("Características del hogar"),
               flowLayout(
                 radioButtons("SexoJefe",
                              "El/la jefe de tu hogar es:",
                              c("Hombre", "Mujer"))
               )),
      tabPanel("enoe")
      ),
))