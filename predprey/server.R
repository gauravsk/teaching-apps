# Lotka volterra predator prey shiny app
# server.R
# Gaurav Kandlikar, 2 Feb 2015

library(deSolve) # package for solving diferential equation
library(dygraphs) # package for time series plotting
library(xts) # for time series

######################################################## 
# Global options - define the predator - prey function #
######################################################## 

lvpp <- function(pp.time,pp.init,pp.params) {
  with (as.list(c(pp.time,pp.init,pp.params)), {
    
    # Parameters
    # N = prey population size; P = predator population size
    # r = intrinsic growth rate of prey
    # a = predation efficiency
    # b = conversion efficiency of prey into predator
    # d = intrinsic dseath rate of predator
    # prey_k = carrying capacity for prey; only used if user-defined

    # if no carrying capacity for prey, use this equation
    if (pp.params["prey_k"] == -9999 ){
      dNdt <- (r*N) - (a*N*P)
    }
    
    # else use the defined prey carrying capacity to compute the values.
    else {
      dNdt <- ((r*N)*(1-(N/prey_k))) - (a*N*P)
    }
    
    dPdt <- (b*a*N*P) - (d*P)
    
    # return the vectors as a list
    return(list(c(dNdt,dPdt)))
  })
}
# Define root function for steady state analysis
rootfun <- function(Time, State, Pars) {
  dstate <- unlist(lv(Time, State, Pars))
  # assume equilibrium if last two results differ by less than 1e-3
  sum(abs(dstate)) - 1e-3
}

### Reactive code

pp.init<- c()
pp.params <- c()
shinyServer(
  function(input, output) {
  
    # Generate carrying capacity option if user selects 'set carrying capacity' option
    output$UIpreyk <- renderUI({
      if (input$Prey_K == 2){
      numericInput("K",label=h6("Select a value for K (prey's carrying capacity)."),value=500,min=1)}
      else {""}      
    })
    
    #####################
    #       TABLE       #
    #####################
    output$table1<- renderTable({
      pp.time <- seq(0,input$time,by=1)
      
      pp.params["r"] <- input$r  # intrinsic growth rate of the prey
      pp.params["a"] <- input$a  # predation efficniency
      pp.params["d"] <- input$d  # death rate of predator
      pp.params["b"] <- input$b  # conversion efficiency
      pp.init["N"]   <- input$N  # starting prey population size
      pp.init["P"]   <- input$P  # starting predator population size
      # Check whether carry capacity has been defined and define appropriately
      if (input$Prey_K == 2){
        pp.params["prey_k"] <- input$K
      }
      if (input$Prey_K == 1){
        pp.params["prey_k"] <- -9999
      }

      # Return the table!
      floor(as.data.frame(ode(func=lvpp,y=pp.init,parms=pp.params,times=pp.time)))
    })

    #####################
    #       PLOTS       #
    #####################
    
    output$plot1<- renderPlot({
      pp.time <- seq(0,input$time,by=1)
      
      pp.params["r"] <- input$r  # intrinsic growth rate of the prey
      pp.params["a"] <- input$a  # predation efficniency
      pp.params["d"] <- input$d  # death rate of predator
      pp.params["b"] <- input$b  # conversion efficiency
      pp.init["N"]   <- input$N  # starting prey population size
      pp.init["P"]   <- input$P  # starting predator population size
      
      # Check whether carry capacity has been defined and define appropriately
      if (input$Prey_K == 2){
        pp.params["prey_k"] <- input$K
      }
      if (input$Prey_K == 1){
        pp.params["prey_k"] <- -9999
      }
      
      # Run the ODE, save as 'lvout'
      lvout<-floor(as.data.frame(ode(func=lvpp,y=pp.init,parms=pp.params,times=pp.time)))

      
      # Begin plotting!
      
      par(mfrow=c(1,2))
      # Panel A: Plot P vs N; draw in the starting N and P parameters, draw in the ZNGIs
      plot(lvout$P~lvout$N,ylim=c(0,max(lvout$P)*1.25),xlim=c(0,max(lvout$N)*1.25),type="l",lwd=1.5,
           xlab="Prey population size",ylab="Predator population size")
      points(x=pp.init["N"],y=pp.init["P"],col="red",pch=18,cex=1.75)

      abline(v=pp.params["d"]/(pp.params["b"]*pp.params["a"]))
      if (pp.params["prey_k"] == -9999) {
        abline(h=pp.params["r"]/pp.params["a"])
      }
      if (pp.params["prey_k"] != -9999) {
        abline (b=-(pp.params["r"]/(pp.params["prey_k"]*pp.params["a"])), a = pp.params["r"]/pp.params["a"])
      }
      
      # Panel B: Plot N & P vs pp.time
      plot(lvout$N~pp.time,type="l",xlab="pp.time",lwd=1.5,
           ylab="Population Size",ylim=c(0,max(max(lvout$N),max(lvout$P))*1.25))
      points(lvout$P~pp.time,col="red",type="l",lwd=1.5)
      legend(x="topright",col=c("black","red"),lty=1,legend=c("Prey","Predator"),bty="n",lwd=2)
      
      })
    
    
    
    ############################################
    #       interactive plot with dygraph      #
    ############################################
    
    output$Dy1<- renderDygraph({
      pp.time <- seq(0,input$time,by=1)
      
      pp.params["r"] <- input$r  # intrinsic growth rate of the prey
      pp.params["a"] <- input$a  # predation efficniency
      pp.params["d"] <- input$d  # death rate of predator
      pp.params["b"] <- input$b  # conversion efficiency
      pp.init["N"]   <- input$N  # starting prey population size
      pp.init["P"]   <- input$P  # starting predator population size
      
      # Check whether carry capacity has been defined and define appropriately
      if (input$Prey_K == 2){
        pp.params["prey_k"] <- input$K
      }
      if (input$Prey_K == 1){
        pp.params["prey_k"] <- -9999
      }
      
      # Run ode; save as 'lvout'; convert to a matrix to use dygraphs capabilities
      lvout<-floor(as.data.frame(ode(func=lvpp,y=pp.init,parms=pp.params,times=pp.time)))
      matrix <- matrix(nrow=nrow(lvout),ncol=2)
      
      matrix[,1] <- lvout$N
      matrix[,2] <- lvout$P
      
      # Begin plotting!
      
      dygraph(xts(matrix, Sys.Date()+1:nrow(lvout)))%>% 
        dyRoller(rollPeriod = 5)%>%
        dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))  %>%
        dySeries("V1", label = "Prey population") %>%
        dySeries("V2", label = "Pred population")
      
    })
    
  }
)
    
