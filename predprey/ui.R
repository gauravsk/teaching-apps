################################
# Lotka Volterra Predator-Prey #
################################
library(dygraphs)
# ui.R file for Shiny app
# gaurav kandlikar, gkan@umd.edu

# start defining the page
shinyUI(fluidPage(
  
  titlePanel("Modeling Lotka Volterra Predator-Prey relations"),
  
  sidebarLayout(
    sidebarPanel(
        tabPanel(title="Lotka Volterra Predator-Prey",
                 helpText(h3("Set parameters for the L-V predator-prey model.")),
                 
                 helpText(h4("Set starting population sizes")),
                 numericInput("N",label=p("Select a value for", span("N", style = "color:black"),"(starting pop of prey)"),value=25,min=1,max=50),
                 numericInput("P",label=p("Select a value for", span("P", style = "color:red"), "(starting pop of predator)"),value=10,min=1,max=25),
                 

                 br(),
                 helpText(h4("Set prey carrying capacity")),
                 radioButtons("Prey_K", label="",
                              choices = list("No prey carring capacity" = 1,
                                             "Set prey carrying capacity" = 2),
                              selected = 1),
                 # If users select "Prey carrying capacity" above, then generate the input option
                 htmlOutput("UIpreyk"),


                 br(),
                 helpText(h4("Set demographic parameters")),
                 sliderInput("r", 
                             label = "Choose a value for r (prey intrinsic growth rate)",
                             min = 0.01, max = 1.99, value=1.0, step = NULL),
                 sliderInput("a", 
                             label = "Choose a value for a (predation efficiency)",
                             min = .0001, max = .5, value=.1, step = NULL),
                 sliderInput("d", 
                             label = "Choose a value for d (predator death rate)",
                             min = 0.01, max = 1, value=0.6, step = NULL),
                 sliderInput("b", 
                             label = "Choose a value for b (conversion efficienct)",
                             min = 0.01, max = 1, value=.5, step = NULL),
                 
                 numericInput("time",label="Number of time steps to run the model",value = 100, min=1)
        )
      ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Equations",
                 p("Some background text on the model goes here"),
                 h4("These are the equations behind the L-V predator prey model"),
                 p(withMathJax("$$ \\frac{dN}{dt} = rN - aNP $$")),
                 p(withMathJax("$$ \\frac{dP}{dt} = baNP - dP $$")),
                 p(withMathJax("Where \\(r\\) is the per capita growth rate of the prey, \\(a\\) is the prey conversion efficiency, \\(b\\) is predation efficiency, and \\(d\\) is predator death rate.")),
                 br(),
                 p("The equilibrium isoclines can be computed with these equations:"),
                 p(withMathJax("$$ \\frac{dN}{dt} = 0 \\text{  when  } P = \\frac{r}{a} $$")),
                 p(withMathJax("$$ \\frac{dP}{dt} = 0 \\text{  when  } N = \\frac{d}{ab}$$")),
                 br(),
                 br(),
                 h4("Model modifications:"),
                 p("A carrying capacity can be introduced onto the prey:"),
                 p(withMathJax("$$ \\frac{dN}{dt} = rN - aNP - \\left(1-\\frac{N}{K} \\right) $$")),
                 p("This changes the equilibrium isocline for the prey:"),
                 p(withMathJax("$$ \\frac{dN}{dt}=0 \\text{  when  } P = \\frac{r}{a}-\\frac{rP}{aK} $$"))
        ),
        
        tabPanel(title = "Plots",
                 h4("Here are plots showing changes in populations"),
                 plotOutput("plot1")
        ),
        
        
        tabPanel(title = "Tables",
                 h4("This table shows population sizes at each time step"),
                 tableOutput("table1")),

        
        tabPanel(title = "Interactive plot",
                h4("Testing interactive graphs"),
                p("Wishlist: disassociate output with fake 'dates' "),
                dygraphOutput("Dy1")
      )
    )
  )
)))