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
shinyUI(
  fluidPage(

    # Application title
    titlePanel("Calculadora de salario"),
    
    tabsetPanel(
      tabPanel("enut",
               titlePanel("Características personales"),
               flowLayout(selectInput("Estado",
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
                                       "¿Vives en un entorno rural, o urbano?",
                                       c("Rural","Urbano")),
                          radioButtons("Indigena",
                                       "¿Se considera indígena?",
                                       c("Sí","No"))),
               titlePanel("Acceso a seguridad social"),
               flowLayout(radioButtons("Afiliacion",
                                       "¿Está afiliado a algún servicio médico?",
                                       c("Sí","No")),
                          radioButtons("ProgramaSocial",
                                       "¿Recientemente tramitó o cobró un programa social?",
                                       c("Sí","No"))),
               titlePanel("Características del hogar"),
               flowLayout(numericInput("nPersonas",
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
                                      9)
                          )),#tabpanel
      tabPanel("enoe",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("columnas",
                               "Número de columnas general:",
                               min = 1,
                               max = 60,
                               value = 30,
                               animate = TRUE),
                   sliderInput("columnas1",
                               "Número de columnas cuidador:",
                               min = 1,
                               max = 60,
                               value = 30,
                               animate=TRUE),
                   sliderInput("columnas2",
                               "Número de columnas chofer:",
                               min = 1,
                               max = 60,
                               value = 30,
                               animate = TRUE),
                   sliderInput("columnas3",
                               "Número de columnas cocinero:",
                               min = 1,
                               max = 60,
                               value = 30,
                               animate = TRUE),
                   sliderInput("columnas4",
                               "Número de columnas enfermera:",
                               min = 1,
                               max = 60,
                               value = 30,
                               animate = TRUE),
                   sliderInput("columnas5",
                               "Número de columnas fisioterapeuta:",
                               min = 1,
                               max = 60,
                               value = 30,
                               animate = TRUE),
                   sliderInput("columnas6",
                               "Número de columnas mandadero:",
                               min = 1,
                               max = 60,
                               value = 30,
                               animate = TRUE)
                   
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("distPlot")
                 )
               ))
    )#tabsetPanel,
  )#fluidPage
)#ShinyUI