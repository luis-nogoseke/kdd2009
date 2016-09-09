library(shiny)

source("../lib/prepare.R")

# Load the trained model and the static indo to load the new data
model <- readRDS("../gmbModel165.RDS");
classes <- readRDS("../smallclass.RDS")
attributes <- readRDS("../cols165.RDS");

shinyServer(function(input, output) {
    raw.data <- reactiveValues(df_data = NULL)

    # To allow the upload of files up to 30 MB
    options(shiny.maxRequestSize=30*1024^2)

    # Monitor the upload button, when used read the file
    observeEvent(input$file1, {
        temp <- read.table(input$file1$datapath, header = TRUE,
                                       sep = '\t', colClasses = c(classes),
                                       stringsAsFactors = TRUE,
                                       na.strings = c(" ", "", "\t"))
        raw.data$df_data <- temp[, attributes]
        rm(temp)
        raw.data$df_data <- TreatNumeric(raw.data$df_data)
        raw.data$df_data <- TreatFactor(raw.data$df_data)
    })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste('Results-', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
            results <- predict(model, newdata = raw.data$df_data)
            write.csv(results, file)
      },
      contentType = "text/csv"
    )

    observe({
        if (input$plotButton == 0) return ()
        isolate({
          output$plot <- renderPlot({
            plot(cars$speed, cars$dist)
          })
        })
    })

# rederUi and uiOutput -> create UI elements dynamically

# Do stuff inside here to show progress
# withProgress(message = 'Making plot', value = 0, {})
#withProgress(message = 'Making predictions', value = 0, {
#    n <- 10
#    for (i in 1:n) {
#      incProgress(1/n, detail = paste("Doing part", i))
#      Sys.sleep(1)
#    }
#    })



})
