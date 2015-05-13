### server.R
# This shiny app implements the model of annual plant growth described in Levine & HilleRisLambers 2009 (Nature). 
# pdf: http://www.plantecology.ethz.ch/content/dam/ethz/special-interest/usys/ibz/plantecology/publications/Nature-2009-Levine_HilleRisLambers-254-56.pdf
# Updated 11 Feb 2015


######## Describe the function f(), which is called in the function Nt below ##########
f <- function(which,Ni,Nj,p) {
  # Calculates the decrease in per capita growth rate due to competition
  with (as.list(p),  {
    i <- (lambda.i)/(1+alpha.ii*g.i*Ni+alpha.ij*g.j*Nj)
    
    j <- (lambda.j)/(1+alpha.jj*g.j*Nj+alpha.ji*g.i*Ni)
    
    if (which == "i") {return (i)}
    else {return (j)}
  })
}

####### Describe the function to calculate Nt ########
Nt <- function (Ni, Nj, t, ps) {
  with (as.list(ps), {
    
    vectNi <- numeric(t)
    vectNi[1] <- Ni
    
    vectNj <- numeric(t)
    vectNj[1] <- Nj
    
    for (i in 2:t) {
      vectNj[i] <- floor((1-g.j)*s.j*vectNj[i-1] + 
                           g.j*vectNj[i-1]*f(which = "j", Ni=vectNi[i-1], Nj=vectNj[i-1],p=ps))
      
      vectNi[i] <- floor((1-g.i)*s.i*vectNi[i-1] + 
                           g.i*vectNi[i-1]*f(which = "i", Ni=vectNi[i-1], Nj=vectNj[i-1],p=ps))      
    }
      
    return(as.data.frame(list("Ni"=vectNi,"Nj"=vectNj)))
  })
}


params <- c()
shinyServer(
  function(input, output) {

######## Render niche overlap ###########  
    output$overlap <- renderText({
      params["alpha.ii"] = (input$alpha.ii)
      params["alpha.ij"] = (input$alpha.ij)
      params["alpha.jj"] = (input$alpha.jj)
      params["alpha.ji"] = (input$alpha.ji)
      rho <- with(as.list(params), sqrt((alpha.ij*alpha.ji) / (alpha.ii*alpha.jj)) )
      round(rho,3)
    })
    
####### Render Fitness Difference ratio #########
    output$fitnessDifference <- renderText({
      params["alpha.ii"] = input$alpha.ii
      params["alpha.ij"] = input$alpha.ij
      params["alpha.jj"] = input$alpha.jj
      params["alpha.ji"] = input$alpha.ji
      params["lambda.i"] = input$lambda.i
      params["lambda.j"] = input$lambda.j
      params["g.i"] = input$gi
      params["g.j"] = input$gj
      params["s.i"] = input$si
      params["s.j"] = input$sj
      
      nu.j <- with(as.list(params), (lambda.j*g.j) / (1-(1-g.j)*s.j))
      nu.i <- with(as.list(params), (lambda.i*g.i) / (1-(1-g.i)*s.i))
      
      kappa.ji <- with(as.list(params), ((nu.j-1) / (nu.i-1))*(sqrt((alpha.ij*alpha.ii) / (alpha.jj*alpha.ji))))
      
      round(kappa.ji,3)
    })
####### Render maximum allowed niche overlap #########
output$maxOverlap <- renderText({
  params["alpha.ii"] = input$alpha.ii
  params["alpha.ij"] = input$alpha.ij
  params["alpha.jj"] = input$alpha.jj
  params["alpha.ji"] = input$alpha.ji
  params["lambda.i"] = input$lambda.i
  params["lambda.j"] = input$lambda.j
  params["g.i"] = input$gi
  params["g.j"] = input$gj
  params["s.i"] = input$si
  params["s.j"] = input$sj
  
  nu.j <- with(as.list(params), (lambda.j*g.j) / (1-(1-g.j)*s.j))
  nu.i <- with(as.list(params), (lambda.i*g.i) / (1-(1-g.i)*s.i))
  
  kappa.ji <- with(as.list(params), ((nu.j-1) / (nu.i-1))*(sqrt((alpha.ij*alpha.ii) / (alpha.jj*alpha.ji))))

  
  if (kappa.ji < 1) {max.overlap <- kappa.ji}
  else {max.overlap <- 1/kappa.ji}

  round(max.overlap,3)
})

########## Render table ########
    output$table1 <- renderTable({
      params["alpha.ii"] = input$alpha.ii
      params["alpha.ij"] = input$alpha.ij
      params["alpha.jj"] = input$alpha.jj
      params["alpha.ji"] = input$alpha.ji
      params["lambda.i"] = input$lambda.i
      params["lambda.j"] = input$lambda.j
      params["g.i"] = input$gi
      params["g.j"] = input$gj
      params["s.i"] = input$si
      params["s.j"] = input$sj
      
      Nt(Ni = input$Ni, Nj=input$Nj, t=1000, ps = params)
      
    })

########## Render plot #######

    output$plot1 <- renderPlot({
      params["alpha.ii"] = input$alpha.ii
      params["alpha.ij"] = input$alpha.ij
      params["alpha.jj"] = input$alpha.jj
      params["alpha.ji"] = input$alpha.ji
      params["lambda.i"] = input$lambda.i
      params["lambda.j"] = input$lambda.j
      params["g.i"] = input$gi
      params["g.j"] = input$gj
      params["s.i"] = input$si
      params["s.j"] = input$sj
  
      for_plot <- Nt(Ni = input$Ni, Nj=input$Nj, t=1000, ps = params)
  
      plot(for_plot$Ni,type="l",col="blue",ylim=c(0, 1.25*max(max(for_plot$Ni),max(for_plot$Nj))), xlab="time", ylab="N",lwd=2)
      points(for_plot$Nj,type="l",col="red",lwd=2)
  
    })
    
    
  }
)
