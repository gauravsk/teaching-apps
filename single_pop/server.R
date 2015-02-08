
# Define lagged logistic growth
# defining a new custom function that can do exponential growth, logistic growth and lagged logistic growth. Function works, but need to make it shiny-compatible.
# All encompassing single population function
# single.pop <- function(N=10,time=500,k=-9999,tau=0,r=.2) {
#   
#   vect <- numeric()
#   vect[1] <- N
#   
#   ## Do this if no carrying capacity
#   if (k == -9999) {
#     for (i in 2:time) {vect[i] <- vect[i-1]+(vect[i-1]*r)}
#   }
#   
#   ## Do this if carrying capacity defined
#   if (k != -9999) {
#     
#     ## Do this if tau is zero (no lag)
#     if (tau == 0){
#       for (i in 2:time){vect[i] <- vect[i-1]+ (vect[i-1]*r*(1-(vect[i-1]/k)))}
#     }
#     
#     ## Do this if tau is not zero (there is some lag)
#     if (tau != 0) {
#       for (i in 2:time) {
#         ## If time is less than lag, just do logistic growth
#         if (i <= tau) {vect[i] <- vect[i-1] + (vect[i-1]*r*(1- (vect[i-1]/k)))}
#         ## If time is greater than lag, implement the lag
#         else {vect[i] <- vect[i-1]+(vect[i-1]*r*(1- (vect[i-tau]/k)))}
#         
#       }
#     }
#     
#   }  
#   return(vect)
#   
# }

### Back to the normal stuff

library(deSolve) # package for solving diferential equation
library(dygraphs) # package for interactive plots
library(xts) # package for timeseries analysis
########################################################## 
# Global options - define the population growth function #
########################################################## 


growth <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    
    # Parameters
    # N = prey population size
    # r = intrinsic growth rate 
    # K = carrying capacity, not necessarily defined.
    
    if (params["K"] == -9999 ){
      dNdt <- (r*N)
    }
    
    else {
      dNdt <- (r*N)*(1-(N/K))
    }
    
    return(list(dNdt))
  })
}

params <- c()
init <- c()
iter <- c()
############################################
# Run the code - to be updated based on UI #
############################################

shinyServer(
  function(input, output) {
    
    output$UI1 <- renderUI({
      if (input$Density == 2){
        numericInput("K",label=h6("Select a value for K (carrying capacity)."),value=500,min=1)#,
        #numericInput("tau",label=h6("Select a value for tau (time lag)."),value=0)
      }
      else {""}
    })
    
    
    #####################
    #       TABLE       #
    #####################
    output$table1<- renderTable({
      
      iters <- seq(1:input$iter)
      params["r"] <- input$r  # intrinsic growth rate
      init["N"]   <- input$N  # starting population size
      if (input$Density == 2){
        params["K"] <- input$K
      }
      if (input$Density == 1){
        params["K"] <- -9999
      }
      
      # Return the table!
      floor(as.data.frame(ode(func=growth,y=init,parms=params,times=iters)))
    })

    
    #####################
    #       PLOTS       #
    #####################
    output$plot1<- renderPlot({
      
      iters <- seq(1:input$iter)
      params["r"] <- input$r  # intrinsic growth rate
      init["N"]   <- input$N  # starting population size
      if (input$Density == 2){
        params["K"] <- input$K
      }
      if (input$Density == 1){
        params["K"] <- -9999
      }
      
      table <- floor(as.data.frame(ode(func=growth,y=init,parms=params,times=iters)))
      
#      par(mfrow=c(2,2))
      plot (table$N~table$time,type="l",lwd=2,xlab="Time",ylab="N (Population size)",main="population growth",cex.lab=1.5,cex.main=1.75,cex.axis=1.25)
    })

    #####################
    #      iPLOTS       #
    #####################

output$Dy1<- renderDygraph({
  
  iters <- seq(1:input$iter)
  params["r"] <- input$r  # intrinsic growth rate
  init["N"]   <- input$N  # starting population size
  if (input$Density == 2){
    params["K"] <- input$K
  }
  if (input$Density == 1){
    params["K"] <- -9999
  }
  table <- floor(as.data.frame(ode(func=growth,y=init,parms=params,times=iters)))
  matrix <- matrix(nrow=nrow(table),ncol=2)
  
  matrix[,1] <- table$N
#  matrix[,2] <- table$N
  
  #      par(mfrow=c(2,2))
#  plot (table$N~table$time,type="l",lwd=2,xlab="Time",ylab="N (Population size)",main="population growth",cex.lab=1.5,cex.main=1.75,cex.axis=1.25)
  dygraph(xts(matrix, Sys.Date()+1:nrow(table)))

})
  }
)

