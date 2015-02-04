#Lotka
#server.R

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

rootfun <- function(Time, State, Pars) {
  dstate <- unlist(lv(Time, State, Pars))
  sum(abs(dstate)) - 1e-3
}

# Start defining the parameters list 
params <- c(r1=1, r2=1)

shinyServer(

  function(input, output) {
    # randomly select starting populations each time a new session is launched
    p <- runif(n=2,min=1,max=1000)
    
    output$UI1 <- renderUI({
      if (input$iter == 2){
        numericInput("steps",label=h6("Time steps to run the model"),value=100,min=1)}
      else {""}
      
    })
    
    # Generate population table
    output$table1<- renderTable({
      init<-c(N1=input$n1,N2=input$n2) 
      
      params["K1"] <- as.numeric(input$k1)
      params["K2"] <- as.numeric(input$k2) 
      params["a"] <- as.numeric(input$alpha)
      params["b"] <- as.numeric(input$beta)
      if (input$iter == 2){time <-seq(0,input$steps,by=1)}
      else {time <- seq(0,1000, by=1)}
      
      if (input$iter == 2){lvout<-floor(as.data.frame(ode(func=lv,y=init,parms=params,times=time)))}
      else {lvout<-floor(as.data.frame(lsodar(func=lv,y=init,parms=params,times=time,rootfun=rootfun)))}
      
      lvout
    })
    
    # Generate the plots
    output$plot1<- renderPlot({
      init<-c(N1=input$n1,N2=input$n2)

      params["K1"] <- as.numeric(input$k1)
      params["K2"] <- as.numeric(input$k2) 
      params["a"] <- as.numeric(input$alpha)
      params["b"] <- as.numeric(input$beta)
      if (input$iter == 2){time <- seq(0,input$steps,by=1)}
      if (input$iter == 1){time <- seq(0,1000, by=1)}
      
      if (input$iter == 2){lvout<-floor(as.data.frame(ode(func=lv,y=init,parms=params,times=time)))}
      if (input$iter == 1){lvout<-floor(as.data.frame(lsodar(func=lv,y=init,parms=params,times=time,rootfun=rootfun)))}
      
      par(mfrow=c(2,2))
      
      
      # plot the Zero Net Growth Isoclines based on parameters above.
      plot(1,type="n",xlim=c(0,max(params["K1"],params["K2"]/params["b"])*1.25),
           ylim=c(0,max(params["K2"],params["K1"]/params["a"])*1.25),
           xlab  ="Species 1",ylab="Species 2",main="ZNGIs for Sp.1 and Sp.2",
           xaxs="i",yaxs="i",cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
      lines(x = c(params["K1"],0),y = c(0,params["K1"]/params["a"]),lwd=2,col="blue")
      lines(x = c(params["K2"]/params["b"], 0) ,y = c(0,params["K2"]),lwd=2,col="red")
      

      
      # plot the starting population size
      points(x=init["N1"],y=init["N2"],cex=2,pch=20)
      
      # Plot out the results of the ODE. 
      # First plot is for N vs P (sp1 v sp2)
      plot(lvout$N2~lvout$N1,type="o",xlim=c(0,max(params["K1"],params["K2"]/params["b"])*1.25),
           ylim=c(0,max(params["K2"],params["K1"]/params["a"])*1.25), main = "Trajectory of population sizes",xlab="Species 1", 
           ylab="Species 2",cex.axis=1.5,cex.lab=1.5,cex.main=1.5, col=rainbow(nrow(lvout)))
      
      # Second plot for N & P v Time
      plot(lvout$N1,ylim=c(0,max(max(lvout$N1),max(lvout$N2))*1.25),ylab="Population size",
                               type="l",lwd=2, col="blue",cex.axis=1.5,cex.lab=1.5,
                               main="Population size vs time",cex.main=1.5)
      points(lvout$N2,col="red",type="l",lwd=2)
      
    })

  }
)