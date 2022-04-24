library(deSolve)

#Función 

sars.epidemic <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
    
    #Variables endógenas auxiliares 
    Probability.of.Contact.with.Infected.Person<-population.infected.with.SARS/Total.Population
    Susceptible.Contacts<-population.susceptible.to.SARS*Contact.Frequency
    Contacts.between.Infected.and.Uninfected.People<-Susceptible.Contacts*Probability.of.Contact.with.Infected.Person #[personas]
    
    #Variables de flujo
    Infection.Rate<-Contacts.between.Infected.and.Uninfected.People*Infectivity #[personas por día]
    
    #Variables de estado
    dpopulation.susceptible.to.SARS<-(-1)*Infection.Rate # [personas]
    dpopulation.infected.with.SARS<-Infection.Rate # [personas]
    list(c(dpopulation.susceptible.to.SARS, dpopulation.infected.with.SARS))
  })
}

#Parámetros de simulación o variables exógenas auxiliares

parameters<-c(Infectivity = 0.1, #sin dimensión
              Contact.Frequency = 2, #personas por día
              Total.Population = 350 ) #personas

#Condiciones inciales

InitialConditions <- c(population.susceptible.to.SARS = 349 , 
                       population.infected.with.SARS = 1) 

#Tiempo

tiempo <- seq(0 , 120 ,0.25 ) #inicio del tiempo (días), final del tiempo (días) y lapso temporal de cambio (días)

#Método
intg.method<-c("rk4")

#Impresión del modelo

out <- ode(y = InitialConditions,
           times = tiempo,
           func = sars.epidemic,
           parms = parameters,
           method =intg.method )

plot(out, col = c("darkgreen"), main = c("Población susceptible al SARS", "Población infectada con SARS"), mfrow = c(1, 2), xlabel="tiempo")
