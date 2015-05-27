##############################
# Single population dynamics #
##############################
library(shinythemes) # make things look pretty

# ui.R file for Shiny app
# gaurav kandlikar, gkan@umd.edu
library(dygraphs)

# start defining the page
shinyUI(fluidPage(
  theme = shinytheme("flatly"),

  titlePanel("Modeling single population dynamics"),
  
  sidebarLayout(
    sidebarPanel(
      tabPanel(title="Single population dynamics",
               helpText(h3("Set parameters for single population growth")),
               
               br(),
               helpText(h4("Growth type")),
               radioButtons("Density", label="",
                            choices = c("Density independent" = 1,
                                           "Density dependent" = 2)),
               
               br(),
               helpText(h4("Set model parameters")),
               numericInput("r",label="Select a value for r (per capita growth rate)",value=.1,min=0,max=2),
               numericInput("N",label="Select a value for N (starting population size)",value=10,min=1,max=25),

               # If users select "Prey carrying capacity" above, then generate the input option
               htmlOutput("UI1"),
               htmlOutput("UI2"),
               br(),
               numericInput("iter",label="Time steps to run model",value=100,min=2,max=9999)
               
            )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = "plots",
                 h4("This plot shows population growth"),
                 plotOutput("plot1")
        ),
        
        tabPanel(title = "Tables",
                 h4("This table shows population size at each time step"),
                 tableOutput("table1")
        ),
        
        tabPanel(title = "Interactive plot",
                 h4("Plots!"),
                 dygraphOutput("Dy1")
        )
      )
    )
  )))