#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # Our dataset
  data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQSZmj5iLaK2jFEbF-zEFLEDdzJnh5e1qJHCrEsVMhowbfN_W11JVKNKtCqVXJgiGwsFsNZVSrFJ7Qg/pub?gid=0&single=true&output=csv")

  output$dt <- renderDataTable({
    DT::datatable(data, options = list(pageLength = 3, Scroll.X = T))
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  datasetInput <- reactive({
    switch(input$dataset,
           "dt1" = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQSZmj5iLaK2jFEbF-zEFLEDdzJnh5e1qJHCrEsVMhowbfN_W11JVKNKtCqVXJgiGwsFsNZVSrFJ7Qg/pub?gid=0&single=true&output=csv"),
           "dt2" = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTJLtfjjUM4VK6aM177ly9GCKyMHFrFqQdsqjhJCtpe4DUGuZWOe2fZWB5xTZEx3WAcW08BVEBFfn2C/pub?gid=0&single=true&output=csv"),
           "dt3" = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSoBfvhztFgALk1fcljBbYP03D-fRIEy7mu1DrHKZ--BXYZWHFxUujac_-gFSteM99p7CFQILT_eXcC/pub?gid=0&single=true&output=csv"),
           "dt4" = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSAYHrkL4ZfJJMV_Y9dL9iT3jM5IT2MguLXl56fPqHuTDyO36Uazy9KJMhHWbp-eeu4O3LjGVdhngQ_/pub?gid=0&single=true&output=csv"))
  })

  output$plot <- renderPlot({
    chart::chart(datasetInput(), weight ~ date %col=% as.factor(id) | species) +
      ggplot2::geom_point(show.legend = F) +
      ggplot2::geom_line(show.legend = F)
  })


})
