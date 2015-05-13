#### ui.R for annual plant growth model shiny app
library(shinythemes) # make things look pretty

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Annual plants model"),
  
  sidebarLayout(
    sidebarPanel(
      tags$style(type="text/css", ".span4 { max-height: 550px ; }"),
      tags$style(type="text/css", ".span4 { overflow: scroll; }"),
      
      helpText(h4("Select parameters for the Annual plant growth models")),
      helpText(("This is an implementation of the model developed in Levine & HilleRisLambers 2009 (Nature). Fitness and Niche difference terms are defined as in Chesson (2012).")),
      
      fluidRow(
        helpText(h5("Select starting population sizes")),
        numericInput("Ni",label = p("Choose a starting population for species ", withMathJax("\\(i\\)")),
                     value = "200",min = 1, max = 500),
        numericInput("Nj",label = p("Choose a starting population for species ", withMathJax("\\(j\\)")),
                     value = "200",min = 1, max = 500),
      
      br(),
      br(),
      helpText(h5("Select seed germination rates")),
      column(width=5,
             sliderInput("gi", label = p("Select a value for ",withMathJax("\\(g_{i}\\)")),
                         min = 0, max=1, value=0.5)),
      column(width=5,
             sliderInput("gj", label = p("Select a value for ",withMathJax("\\(g_{j}\\)")),
                         min = 0, max=1, value=0.5)),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      helpText(h5("Select seed bank survival")),
      column(width=5,
             sliderInput("si", label = p("Select a value for ", withMathJax("\\(s_{i}\\)") ),
                         min = 0, max=1, value=0.5)),
      column(width=5,
             sliderInput("sj", label = p("Select a value for ",withMathJax("\\( s_{j}\\)")),
                         min = 0, max=1, value=0.5)),
      
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      helpText(h5("Select intrinsic growth rates")),
      column(width=5,
             numericInput("lambda.i", label = p("Select a value for ", withMathJax("\\(\\lambda_{i}\\)") ),
                         min = 0, value=300)),
      column(width=5,
             numericInput("lambda.j", label = p("Select a value for ",withMathJax("\\(\\lambda_{j}\\)")),
                         min = 0, value=300)),
      
      br(),
      br(),
      br(),
      br(),
      helpText(h5("Select competition coefficients")),
      column(width=5,
             sliderInput("alpha.ii", label = p("Select a value for ", withMathJax("\\(\\alpha_{ii}\\)") ),
                         min = 0, max=1, value=0.5)),
      column(width=5,
             sliderInput("alpha.ij", label = p("Select a value for ",withMathJax("\\( \\alpha_{ij}\\)")),
                         min = 0, max=1, value=0.5)),
      
      br(),
      br(),
      column(width=5,
             sliderInput("alpha.jj", label = p("Select a value for ", withMathJax("\\(\\alpha_{jj}\\)") ),
                         min = 0, max=1, value=0.5)),
      column(width=5,
             sliderInput("alpha.ji", label = p("Select a value for ",withMathJax("\\( \\alpha_{ji}\\)")),
                         min = 0, max=1, value=0.5))
      )),
    
    
    
    mainPanel(
      tabsetPanel(
        ######## Parameter description tab ##########
        tabPanel(title="Parameter Description",
                 p(h3("This panel explains the parameters to enter into the model")),
                 p(div("\\(g_{i}\\) and \\(g_{j}\\) are the germination rates of species \\(i\\) and \\(j\\).")),
                 br(),
                 p(div("\\(s_{i}\\) and \\(s_{j}\\) are the survival rates of ungerminated seeds in the soil over a year of species \\(i\\) and \\(j\\).")),
                 br(),
                 p(div("\\(\\lambda_{i}\\) and \\(\\lambda_{j}\\) are per germinant fecundities of species \\(i\\) and \\(j\\) in the absence of competition.")),
                 br(),
                 p(div("The \\(\\alpha\\) parameters describe the reduction in per germinant fecundity due to each individual. For instance, \\(\\alpha_{ii}\\) describes the decrease in \\(\\lambda_{i}\\) due to each individual of species \\(i\\), and \\(\\alpha_{ij}\\) describes the reduction in \\(\\lambda_{i}\\) due to each individual of species \\(j\\).")),
                 p("The model uses these parameters, which can be estimated with field experiments, to estimate population growth rates using the following equation:"),
                 p(div("$$ N_{i,t+1} = (1-g_{i})s_{i}N_{i,t} + g_{i}N_{i,t}F_{i} $$")),
                 p(div("where     ", span("\\(F_{i} = \\frac{\\lambda_{i}}{1+\\alpha_{ii}g_{i}N_{i,t}+\\alpha_{ij}g_{j}N_{j,t}} \\)",style="font-size:15px"),align="center"))
                 
                 ),
        ######## Summary parameters tab ##########
        tabPanel(title="Niche/Fitness Difference",
                 p(div(span("\\( \\rho = \\sqrt{\\frac{\\alpha_{ij}\\alpha_{ji}}{\\alpha_{ii}\\alpha_{jj}}} = \\)",textOutput("overlap", inline = TRUE), style="font-size:20px"), align="center")),
                 br(),
                 br(),
                 
                 p(div(span("\\(\\frac{\\kappa_{j}}{\\kappa_{i}} = \\left( \\frac{\\eta_{j}-1}{\\eta_{i}-1} \\right) \\sqrt{\\frac{\\alpha_{ij}\\alpha_{ii}}{\\alpha_{jj}\\alpha_{ji}}} = \\)",textOutput("fitnessDifference", inline = TRUE), style="font-size:20px"),align="center")),
                 
                 p(div("where     ", span("\\(\\eta_{i} = \\frac{\\lambda_{i}g_{i}}{1-(1-g_{i})(s_{i})} \\)",style="font-size:17px"),align="center")),
                 p(div("Coexistence occurs when \\(\\rho \\lt \\frac{\\kappa_{j}}{\\kappa_{i}}\\), where \\(i\\) is the competitive dominant.")),
                 p(div(span("Given the current parameters, the maximum niche overlap that will permit stable coexistence is",textOutput("maxOverlap",inline =  TRUE)), style="font-size:18px; color:blue"))
                 
                ),
        
        ######## Plot tab ##########
        tabPanel(title="Plot",
                 plotOutput("plot1")
        ),
        
        ######## Table tab ##########
        tabPanel(title="Table",
                tableOutput("table1")
                 )

      )
  ))
))
