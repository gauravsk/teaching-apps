shinyUI(fluidPage(
  titlePanel("graphr"),
  sidebarLayout(
    sidebarPanel(
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
      tags$hr()
    ),
    mainPanel(
      tableOutput('contents'),
      plotOutput("testplot")
    )
  )
))