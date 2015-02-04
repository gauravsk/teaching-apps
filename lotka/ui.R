# Lotka Volterra competition
# Wishlist:
# Make the "Randomize!" button select new random values of N1 and N2


#ui.R
shinyUI(fluidPage(

  titlePanel("Modeling Lotka Volterra competition"),
  
  sidebarLayout(
    sidebarPanel(
        tabPanel(title="Lotka Volterra Competition",
          
          helpText(h4("Set initial population sizes")),
          numericInput("n1",label=p("Select a value for", span("N1",style = "color:blue")),value=floor(rnorm(1,mean=500,sd=100)),min=1),
          numericInput("n2",label=p("Select a value for", span("N2",style = "color:red" )),value=ceiling(rnorm(1,mean=500,sd=100)),min=1),
                 
          helpText(h4("Set carrying capacities")),
          numericInput("k1",label=p("Select a value for", span("K1",style = "color:blue")),value=500,min=1),
          numericInput("k2",label=p("Select a value for", span("K2",style = "color:red" )),value=500,min=1),
          
          
          helpText(h4("Set competition coefficients")),
          sliderInput("alpha", 
                      label = p("Choose a value for", HTML("&alpha;")),
                      min = 0, max = 2.0, value=0.75, step = 0.01),
          sliderInput("beta", 
                      label = p("Choose a value for", HTML("&beta;")),
                      min = 0, max = 2.0, value=0.75, step = 0.01),
          
          br(),
          radioButtons("iter", label="",
                       choices = list("Run until steady state" = 1,
                                      "Run for specified length" = 2),
                       selected = 1),
          htmlOutput("UI1"))
      
    ),  
    
    mainPanel(

      tabsetPanel(
        
        tabPanel(title = "Equation",
                 p("Include some background information about LV competition here (eg. assumptions, etc.)"),
                 h4("These are the Lotka-Volterra equations for two species:"),
                 p(withMathJax("$$ \\frac{dN_{1}}{dt} = (r_{1}N_{1}) \\left( 1-\\frac{N_{1} + \\alpha*N_{2}}{K_{1}} \\right) $$")),
                 p(withMathJax("$$ \\frac{dN_{2}}{dt} = (r_{2}N_{2}) \\left( 1-\\frac{N_{2} + \\beta*N_{1}}{K_{2}} \\right) $$")),
                 br(),
                 h4("These equations calculate equilibrium values of N1 and N2:"),
                 p(withMathJax("$$ \\frac{dN_{1}}{dt} = 0 \\text{  when  } N_{1} = K_{1} - \\alpha*N_{2} $$")),
                 p(withMathJax("$$ \\frac{dN_{2}}{dt} = 0 \\text{  when  } N_{2} = K_{2} - \\beta*N_{1} $$"))                 
        ),

        tabPanel(title = "Plots",                 
                 h4("These plots show changes in population sizes with time"),
                 plotOutput("plot1",height="700px",width="700px")
        ),
        
        tabPanel(title = "Tables",
                 h4("This table shows the population sizes at each time step"),
                 tableOutput("table1")
        )
      )
    )
  )
))