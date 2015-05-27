# Gaurav Kandlikar, gkan@umd.edu
# Last update 27 May 2015
# Modeling growth of a single population growing exponentially or with K
# If density dependent, Tau can be manipulated

growth <- function(N=10,time=500,k=-9999,tau=0,r=.2) {
# growth <- function(N, time, k, tau, r) {

  vect <- numeric(time)
  vect[1] <- N
  
  ## Do this if no carrying capacity
  if (k == -9999) {
    for (i in 2:time) {vect[i] <- vect[i-1]+(vect[i-1]*r)}
  }
  
  ## Do this if carrying capacity defined
  if (k != -9999) {
    
    ## Do this if tau is zero (no lag)
    if (tau == 0){
      for (i in 2:time){vect[i] <- vect[i-1]+ (vect[i-1]*r*(1-(vect[i-1]/k)))}
    }
    
    ## Do this if tau is not zero (there is some lag)
    if (tau != 0) {
      for (i in 2:time) {
        ## If time is less than lag, just do logistic growth
        if (i <= tau) {vect[i] <- vect[i-1] + (vect[i-1]*r*(1- (vect[i-1]/k)))}
        ## If time is greater than lag, implement the lag
        else {vect[i] <- vect[i-1]+(vect[i-1]*r*(1- (vect[i-tau]/k)))}
        
      }
    }
  }  
  return(vect)
}


### Shiny server ---- 
shinyServer(
  function(input, output) {
    
    ### Generate UI ---- 
    output$UI1 <- renderUI({
      if (input$Density == 2){
        numericInput("K",label=h6("Select a value for K (carrying capacity)."),value=500,min=1)
      }
      else {""}
    })
    output$UI2 <- renderUI({
      if (input$Density == 2){
        numericInput("tau",label=h6("Select a value for tau (time lag)."),value=0)
      }
      else {""}
    })
    
    ### Reactively generate initial conditions. Access later using init() ----
    init <- reactive ({ input$N })
    
    tau <- reactive ({
      if(input$Density == 2) {input$tau}
      else {return(0)}
    })
    
    carrycap <- reactive({
      if (input$Density == 2){input$K}
      else(return(-9999))
    })
    growthrate <- reactive({input$r})
    
    
    # Reactively generate the time. Access later using time()
    time <- reactive({
      input$iter
    })
    
    ### output df ------
    outdf <- reactive ({
      N2 <- init()
      k2 <- carrycap()
      time2 <- time()
      tau2 <- tau()
      r2 <- growthrate()
      vec <- growth(N = N2, k = k2, time = time2, tau = tau2, r = r2)
      # growth(N = N2, k = k2, time = time2, tau = tau2, r = r2)
      timevec <- 1:time2
      
      data.frame(cbind("time" = timevec, "N" = vec))
    })
    
    ### Table ---- 
    output$table1 <- renderTable({outdf()})
    
    ### Plot ----
    output$plot1<- renderPlot({
      table <- outdf()
      plot (table$N~table$time,type="l", lwd=2, xlab="Time", ylab="N (Population size)",
            main="population growth", cex.lab=1.25, cex.main=1.75, cex.axis=1.25)
    })

  }
)

