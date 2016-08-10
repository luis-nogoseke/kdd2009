library(shiny)

shinyServer(function(input, output) {
  observe({
    if (input$plotButton == 0) return ()
    isolate({
      output$plot <- renderPlot({
        plot(cars$speed, cars$dist)
      })
    })
  })
})
