
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  datasetInput <- reactive({
    switch(input$dataset,
           "dt1" = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQSZmj5iLaK2jFEbF-zEFLEDdzJnh5e1qJHCrEsVMhowbfN_W11JVKNKtCqVXJgiGwsFsNZVSrFJ7Qg/pub?gid=0&single=true&output=csv"),
           "dt2" = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTJLtfjjUM4VK6aM177ly9GCKyMHFrFqQdsqjhJCtpe4DUGuZWOe2fZWB5xTZEx3WAcW08BVEBFfn2C/pub?gid=0&single=true&output=csv"),
           "dt3" = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSoBfvhztFgALk1fcljBbYP03D-fRIEy7mu1DrHKZ--BXYZWHFxUujac_-gFSteM99p7CFQILT_eXcC/pub?gid=0&single=true&output=csv"))
  })
  output$dataset <- renderTable({datasetInput()})
  output$plot <- renderPlot({
    chart::chart(datasetInput(), temperature ~ salinity) +
      ggplot2::geom_point()
  })
})
