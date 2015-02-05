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
        numericInput("K",label=h6("Select a value for K (carrying capacity)."),value=500,min=1)}
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

