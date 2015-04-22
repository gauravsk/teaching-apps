# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
library(readxl)
shinyServer(function(input, output) {

  inFile <- reactive ({input$file1})
  inDataFrame <- reactive ({
    inFile <- inFile()
    read.csv(inFile$datapath, header = TRUE, sep = ",")
  })
  
  output$contents <- renderTable({
    if (is.null(inFile()))
      return(NULL)
    inDataFrame()
  })
  
  
  output$xoption <- renderUI({
    if (is.null(inFile()))
      return(NULL)
    df <- inDataFrame()
    radioButtons("xaxis",label=h6("Select a column for x-axis"), choices=c(colnames(df)))    
  })
  
  output$yoption <- renderUI({
    if (is.null(inFile()))
      return(NULL)
    df <- inDataFrame()
    checkboxGroupInput("yaxis",label=h6("Select a column for y-axis"), choices=c(colnames(df)))
  })

  output$main_title <- renderUI({
    if (is.null(inFile()))
      return(NULL)
    textInput("plot_title", label="Enter Main Title for Plot", value = "Main title goes here")
  })
  
  
  output$xlabel <- renderUI({
    if (is.null(inFile()))
      return(NULL)
    textInput("xlab", label="Enter X-axis Title and Units", value = "X-axis legend goes here")
  })
  
  output$ylabel <- renderUI({
    if (is.null(inFile()))
      return(NULL)
    textInput("ylab", label="Enter Y-axis Title and Units", value = "Y-axis legend goes here")
  })

  output$slope <- renderUI({
    if (is.null(inFile()))
      return(NULL)
    checkboxInput("slope", label="Add best-fit line (linear only)?")
  })

  
  output$testplot <- renderPlot({
    if (is.null(inFile()))
      return(NULL)
    
    df <- inDataFrame()
    y <- input$yaxis
    x <- input$xaxis
    
    if (length(y) == 1) {plot(df[,y] ~ df[,x], xlab=input$xlab, ylab=input$ylab, pch = 19, type="b", main=input$plot_title)}
    if (length(y) == 2) {
      plot(df[,y[1]] ~ df[,x], xlab=input$xlab, ylab=input$ylab, pch = 19, type="b", main=input$plot_title)
      points(df[ ,y[2]] ~ df[ ,x], col="darkblue", pch=19, type = "b")
    }
    
    
    if (input$slope){
      fit <- lm(df[ ,y[1]]~df[ ,x], data = df)
      coefs <- coef(fit)
      b0 <- round(coefs[1], 2)
      b1 <- round(coefs[2], 2)
      r2 <- round(summary(fit)$r.squared, 2)
      eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ r^2 == .(r2))
      text(8, 0.5, eqn)
    }
  })
  
})