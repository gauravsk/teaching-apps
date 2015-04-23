library(shiny)
shinyUI(fluidPage(
  titlePanel("graphr"),
  sidebarLayout(
    sidebarPanel(
      helpText("I only take csv files for now!"),
      fileInput('file1', 'Choose file to upload',
                accept = c('.csv')
      ),
      tags$hr(),
      htmlOutput("main_title"),
      htmlOutput("xoption"),
      htmlOutput("xlabel"),
      htmlOutput("yoption"),
      htmlOutput("ylabel"),
      htmlOutput("yrange"),
      
      htmlOutput("slope"),
      htmlOutput('downloader'),
      tags$hr()
    ),
    mainPanel(
      tableOutput('contents'),
      plotOutput("plot", width = 500, height=500)
      
    )
  )
))