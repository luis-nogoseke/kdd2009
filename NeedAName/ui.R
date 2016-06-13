library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Título"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values, text/plain', 
                         '.csv')),
      downloadButton('downloadData', 'Download Result'),
      actionButton('plotButton', 'Display Statistics', icon = NULL, width = NULL)
    ),
    
    mainPanel(
      plotOutput('plot')
    )
  )
))
