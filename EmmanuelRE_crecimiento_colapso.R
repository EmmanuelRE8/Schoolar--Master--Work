library("deSolve")

parameters11<-c(normal.birth.rate = 0.0035, #sin dimensiones
                regeneration.rate = 1.2, #sin dimensiones
                carrying.capacity = 7500000, #unidades del recurso
                min.regeneration.rate = 0.01, #sin dimensión
                rapid.resource.depletion.time = 1, #año
                renewable.resource.consumption.per.capita = 1 #unidades de recurso por persona
) 

InitialConditions11 <- c(population = 1000000 ,
                         renewable.resources = 5000000)

overshoot.and.collapse <- function(t, state, parameters11) {
  with(as.list(c(state,parameters11)), {
    
    #Variables endógenas auxiliares
    per.capita.renewable.resource.availability<-renewable.resources/population
    normal.lifetime<-max(15, min(100,66)) #66 es el promedio de vida propuesto.
    resource.availability.dependent.lifetime<-normal.lifetime*per.capita.renewable.resource.availability
    resource.dependent.regeneration<-regeneration.rate*renewable.resources*(renewable.resources/carrying.capacity)*(1-renewable.resources/carrying.capacity)
    min.regeneration<-carrying.capacity*min.regeneration.rate
    
    #Variable de flujo
    births.flow<-population*per.capita.renewable.resource.availability*normal.birth.rate
    deaths.flow<-population/resource.availability.dependent.lifetime
    regeneration<-min.regeneration+resource.dependent.regeneration
    resource.use<-min(renewable.resources/rapid.resource.depletion.time, renewable.resource.consumption.per.capita*population)
    
    #State (stock) variables
    dpopulation<-births.flow-deaths.flow
    drenewable.resources<-regeneration-resource.use
    list(c(dpopulation, drenewable.resources))
  })
}

times <- seq(0 , #inicial time #years
             100 , #end time #years
             0.25 ) #time step #years

intg.method<-c("rk4")

out <- ode(y = InitialConditions11,
           times = times,
           func = overshoot.and.collapse,
           parms = parameters11,
           method =intg.method )

plot(out, col = c("darkgreen"), main = c("Población", "Recurso Renovables"),xlab="Tiempo transcurrido (años)",ylab=c("Personas", "Unidades de recursos renovables"), mfrow = c(1, 2))
