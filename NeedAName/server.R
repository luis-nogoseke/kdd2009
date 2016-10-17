library(shiny)
library(randomForest)
source("../lib/prepare.R")
source("../lib/TCC_RandomForest.R")

# Load the trained model and the static indo to load the new data
model_a <- readRDS("../SForest_Appetency20.RDS")
model_u <- readRDS("../SForest_Upselling20.RDS")
model_c <- readRDS("../SForest_Churn20.RDS")


classes <- readRDS("../smallclass.RDS")
attributes <- readRDS("../attributes_Churn200.RDS")
facs <- readRDS('../facts_Churn200.RDS')

print('Ready')

shinyServer(function(input, output) {
    raw.data <- reactiveValues(df_data = NULL)
    results <- reactiveValues(appetency = NULL)
    results <- reactiveValues(upselling = NULL)
    results <- reactiveValues(churn = NULL)

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
            results$appetency <- predictForest(model_a, raw.data$df_data, length(model_a))
            results$upselling <- predictForest(model_u, raw.data$df_data, length(model_u))
            results$churn <- predictForest(model_c, raw.data$df_data, length(model_c))
            df <- data.frame(results$appetency, results$upselling, results$churn)
            write.csv(df, file)
      },
      contentType = "text/csv"
    )

    observe({
        if (input$plotButton == 0 || is.null(results$churn)) return ()
        isolate({
          output$plot_a <- renderPlot({
            barplot(table(results$appetency), main='Appetency Results', names.arg=c('0', '1'))
            text(0.7, 20000, as.character(sum(results$appetency == 1)))
            text(1.9, 20000, as.character(sum(results$appetency == 2)))
          })
          output$plot_u <- renderPlot({
           barplot(table(results$upselling), main='Upselling Results', names.arg=c('0', '1'))
           text(0.7, 20000, as.character(sum(results$upselling == 1)))
           text(1.9, 20000, as.character(sum(results$upselling == 2)))
          })
          output$plot_c <- renderPlot({
           barplot(table(results$churn), main='Churn Results', names.arg=c('0', '1'))
           text(0.7, 20000, as.character(sum(results$churn == 1)))
           text(1.9, 20000, as.character(sum(results$churn == 2)))
          })
        })
    })
})
