#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(deSolve)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

      
      covid.epidemic <- function(t, state, parameters) {
        with(as.list(c(state,parameters)), {
          #Endogenous auxiliary variables
          Probability.of.Contact.with.Infected<-population.infected.with.COVID/Total.Population #dimensionless
          Susceptible.Contacts<-population.susceptible.to.COVID*Contact.Frequency #[people/time]
          Contacts.bt.Infected.and.Uninfected.People<-Susceptible.Contacts*Probability.of.Contact.with.Infected #[people/time]
          
          #Flow variables
          Infection.Rate<-Infectivity*Contacts.bt.Infected.and.Uninfected.People #[people/time]
          
          #State (stock) variables
          dpopulation.susceptible.to.COVID<-(-1)*Infection.Rate #Stock units: People/time
          dpopulation.infected.with.COVID<-Infection.Rate #Stock units: People/time
          
          list(c(dpopulation.susceptible.to.COVID, dpopulation.infected.with.COVID))
        })
      }
      
      parameters<-c(Infectivity = input$Infectivity, #0.1 [1] dimmensionless
                    Contact.Frequency = input$Contact.Frequency,  #2, # people/day
                    Total.Population = 350 ) #people)
      
      InitialConditions <- c(population.susceptible.to.COVID = 349 ,
                             population.infected.with.COVID = 1)
      
      times <- seq(0 , #initial time, days
                   to = input$finaltime, #120 , #end time, days
                   0.25 ) #time step, days
      
      
      intg.method<-c("rk4")
      
      out <- ode(y = InitialConditions,
                 times = times,
                 func = covid.epidemic,
                 parms = parameters,
                 method =intg.method )
      
      plot(out)

    })

})
