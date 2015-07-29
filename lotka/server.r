# Lotka
# server.R

library(deSolve)

lv <- function(time,init,params) {
  with (as.list(c(time,init,params)), {
    # description of parameters:
    # r1 = growthrate of Sp. 1; r2 = growthrate of Sp. 2
    # N = population size of Sp. 1; P = Population Size Sp. 2
    # a = competitive impact of Sp. 2 on Sp. 1; b = competitive impact of Sp 1 on Sp 2
    # K1/K2 = carrying capacities
    dN1dt <- (r1*N1*(1-(N1+a*N2)/K1)) 
    dN2dt <- (r2*N2*(1-(N2+b*N1)/K2))
    return(list(c(dN1dt,dN2dt)))
  })
}

# Root functions define the stopping parameters - when this root is zero, the thing will stop.
rootfun <- function(Time, State, Pars) {
  dstate <- unlist(lv(Time, State, Pars))
  sum(abs(dstate)) - 1e-3
}

shinyServer(
  
  function(input, output) {
    # randomly select starting populations each time a new session is launched
    p <- runif(n=2,min=1,max=1000)
    
    
    #############################################################################################
    # Reactively generate the parameters and the dataframe based on user inputs
    #############################################################################################
    
    # Reactively generate UI- if user selects "run till specified time" then let them choose steps.
    output$UI1 <- renderUI({
      if (input$iter == 2){
        numericInput("steps",label=h6("Time steps to run the model"),value=100,min=1)}
      else {""}
      
    })
    
    # Reactively generate initial conditions. Access later using init()
    init <- reactive ({
      input$goButton
      isolate(c("N1" = input$n1, "N2" = input$n2))
    })
    
    # Reactively generate the params list. Access later using params()
    params <- reactive({
      input$goButton
      isolate(c("r1" = 1, "r2" = 1, "K1" = as.numeric(input$k1), "K2" = as.numeric(input$k2), "a" = as.numeric(input$alpha), "b" = as.numeric(input$beta)))
    })
    
    # Reactively generate the time. Access later using time()
    time <- reactive({
      
      if (input$iter == 2){seq(0,input$steps,by=1)}
      else {seq(0,1000, by=1)}
    })
    
    # Reactively generate the lvout dataframe. Access later using lvout()
    lvout <- reactive({
      init<- init()
      params <- params()
      time <- time()
      
      if (input$iter == 2){floor(as.data.frame(ode(func=lv,y=init,parms=params,times=time)))}
      else {floor(as.data.frame(lsodar(func=lv,y=init,parms=params,times=time,rootfun=rootfun)))}
    })
    
    
    
    
    
    
    
    #############################################################################################
    # Generate the output table graphs!
    #############################################################################################    
    
    # Generate population table
    output$table1<- renderTable({      
      generated_df <- lvout()
      generated_df
    })
    
    # Generate the plots
    output$plot1 <- renderPlot({
      init<- init()
      params <- params()
      time <- time()
      generated_df <- lvout()      
      
      par(mfrow=c(2,2))
      # plot the Zero Net Growth Isoclines based on parameters above.
      plot(1,type="n",xlim=c(0,max(params["K1"],params["K2"]/params["b"])*1.25),
           ylim=c(0,max(params["K2"],params["K1"]/params["a"])*1.25),
           xlab  ="Species 1",ylab="Species 2",main="ZNGIs for Sp.1 and Sp.2",
           xaxs="i",yaxs="i",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
      legend("topright", col = c("Blue", "Red"), lty = 1, legend = c("Species 1", "Species 2"), bty = "n")
      lines(x = c(params["K1"],0),y = c(0,params["K1"]/params["a"]),lwd=2,col="blue")
      lines(x = c(params["K2"]/params["b"], 0) ,y = c(0,params["K2"]),lwd=2,col="red")
      
      # plot the starting population size
      points(x=init["N1"],y=init["N2"],cex=2,pch=20)
      
      # Plot out the results of the ODE. 
      # First plot is for N vs P (sp1 v sp2)
      plot(generated_df$N2~generated_df$N1,type="o",xlim=c(0,max(params["K1"],params["K2"]/params["b"])*1.25),
           ylim=c(0,max(params["K2"],params["K1"]/params["a"])*1.25), main = "Trajectory of population sizes",xlab="Species 1", 
           ylab="Species 2",cex.axis=1.5,cex.lab=1.5,cex.main=1.5, col=rainbow(nrow(generated_df)))
      
      # Second plot for N & P v Time
      plot(generated_df$N1,ylim=c(0,max(max(generated_df$N1),max(generated_df))*1.25),ylab="Population size",
           type="l",lwd=2, col="blue",cex.axis=1.5,cex.lab=1.5,
           main="Population size vs time",cex.main=1.5)
      points(generated_df$N2,col="red",type="l",lwd=2)
      legend("topright", col = c("Blue", "Red"), lty = 1, legend = c("Species 1", "Species 2"), bty = "n")      
    })
    
    plot2save <- function(){
      init<- init()
      params <- params()
      time <- time()
      generated_df <- lvout()      
      
      par(mfrow=c(2,2))
      # plot the Zero Net Growth Isoclines based on parameters above.
      plot(1,type="n",xlim=c(0,max(params["K1"],params["K2"]/params["b"])*1.25),
           ylim=c(0,max(params["K2"],params["K1"]/params["a"])*1.25),
           xlab  ="Species 1",ylab="Species 2",main="ZNGIs for Sp.1 and Sp.2",
           xaxs="i",yaxs="i",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
      legend("topright", col = c("Blue", "Red"), lty = 1, legend = c("Species 1", "Species 2"), bty = "n")
      lines(x = c(params["K1"],0),y = c(0,params["K1"]/params["a"]),lwd=2,col="blue")
      lines(x = c(params["K2"]/params["b"], 0) ,y = c(0,params["K2"]),lwd=2,col="red")
      
      # plot the starting population size
      points(x=init["N1"],y=init["N2"],cex=2,pch=20)
      
      # Plot out the results of the ODE. 
      # First plot is for N vs P (sp1 v sp2)
      plot(generated_df$N2~generated_df$N1,type="o",xlim=c(0,max(params["K1"],params["K2"]/params["b"])*1.25),
           ylim=c(0,max(params["K2"],params["K1"]/params["a"])*1.25), main = "Trajectory of population sizes",xlab="Species 1", 
           ylab="Species 2",cex.axis=1.5,cex.lab=1.5,cex.main=1.5, col=rainbow(nrow(generated_df)))
      
      # Second plot for N & P v Time
      plot(generated_df$N1,ylim=c(0,max(max(generated_df$N1),max(generated_df))*1.25),ylab="Population size",
           type="l",lwd=2, col="blue",cex.axis=1.5,cex.lab=1.5,
           main="Population size vs time",cex.main=1.5)
      points(generated_df$N2,col="red",type="l",lwd=2)
      legend("topright", col = c("Blue", "Red"), lty = 1, legend = c("Species 1", "Species 2"), bty = "n")      
    }
    
    param_text <- reactive({
      init<- init()
      params <- params()
      
      paste("K1 = ", params["K1"], ", K2 = ", params["K2"], ", r1 = ", params["r1"], ", r2 = ", params["r2"], ", Alpha = ", params["a"], ", Beta = ", params["b"], sep = "")
    })
    
    output$downloadPlot <- downloadHandler(
      filename = 
        function() {
          paste('plot-', Sys.Date(), '.png', sep='')
        },
      content = function(filename) {
        png(filename, height = 500, width = 750)
        par(mfrow = c(1,2), oma = c(2,0,0,0))
        plot2save()
        mtext(param_text(), side = 1, outer = TRUE)
        dev.off()
      }
    )
    
  }
)