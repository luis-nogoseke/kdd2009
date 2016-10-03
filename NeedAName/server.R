library(shiny)
library(randomForest)
source("../lib/prepare.R")

# Load the trained model and the static indo to load the new data
model <- readRDS("../rf.RD");
classes <- readRDS("../smallclass.RDS")
attributes <- readRDS("../adaAttributes.RD");
facs <- readRDS('../adaFacLevels.RD')

print('Ready')

shinyServer(function(input, output) {
    raw.data <- reactiveValues(df_data = NULL)
    results <- reactiveValues(res = NULL)

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
        for (n in names(facs)){
              raw.data$df_data[,n] <- fct_collapse(raw.data$df_data[,n], Other = subset(levels(raw.data$df_data[,n]), !(levels(raw.data$df_data[,n]) %in% facs[[n]])))
        }
    })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste('Results-', Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
            results$res <- predict(model, newdata = raw.data$df_data)
            output$plot <- renderPlot({
                plot(cars$speed, cars$dist)
            })
            write.csv(results$res, file)
      },
      contentType = "text/csv"
    )

    observe({
        if (input$plotButton == 0 || is.null(results$res)) return ()
        isolate({
          output$plot <- renderPlot({
            barplot(table(results$res))
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
