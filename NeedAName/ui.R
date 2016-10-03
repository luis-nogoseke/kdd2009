library(shiny)

shinyUI(fluidPage(

    titlePanel("Predict Customer Churn, Appetency and Upselling"),

    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose Input File",
                accept = c("text/csv", "text/comma-separated-values",
                           "text/plain", ".csv", ".data")),
            downloadButton("downloadData", "Download Result"),
            actionButton("plotButton", "Display Statistics", icon = NULL,
                         width = NULL)
            # checkboxInput("somevalue", "Some value", FALSE)

    ),

    mainPanel(
      plotOutput("plot")
    )
  )
))
