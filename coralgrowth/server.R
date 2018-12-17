#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source(file = "../R/fonctions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # Import data
    googlesheets_as_csv <- "https://docs.google.com/spreadsheets/d/{id}/export?format=csv"
    coral_id <- "1iMH4YXh80SxG0Rg6miMVABglIB7fnH42cYenml9WsRM"
    coral_url <- glue::glue(googlesheets_as_csv, id = coral_id)
    # coral <- read.csv(coral_url, dec = ',')
    # coral <- coral[,1:7]
    # coral$sample_date <- lubridate::dmy_hm(coral$sample_date)
    # coral$id <- as.factor(coral$id)
    coral <- coral_import(coral_url)
    coral <- coral_growth(coral)


  output$choixvar <- renderUI({
    checkboxGroupInput("var","Choix des variables a afficher :",
                       names(coral)[-c(1, 10:17)], names(coral)[-c(1, 7:17)])
  })

  output$ID <- renderUI({
    selectInput("id","Choix de l'ID a afficher :", choices = c("head", "All",levels(coral$id)))
  })

  output$date_ <- renderUI({
    dateRangeInput("daterange","Date range :",
                   start = min(lubridate::date(coral$date), na.rm = TRUE),
                   end = max(lubridate::date(coral$date), na.rm = TRUE),
                   min = min(lubridate::date(coral$date), na.rm = TRUE),
                   max = max(lubridate::date(coral$date), na.rm = TRUE))
  })

  output$tableau <- renderTable({
    if ("head" %in% input$id) {
      head(coral[,input$var], n = 5 )
    } else if ("All" %in% input$id) {
      coral[coral$datefilter >= input$daterange[1] &
            coral$datefilter <= input$daterange[2],input$var] %>% na.omit()
    } else {
      coral[coral$id %in% input$id,input$var]
    }
  })

  output$tableauGrowth <- renderTable({
    variables <- c("localisation", "species", "id", "sample_date",
                   "skel_weight", "day", "growth", "growth_ln")
    if ("head" %in% input$id) {
      head(coral[,variables], n = 5 )
    } else if ("All" %in% input$id) {
      coral[coral$datefilter >= input$daterange[1] &
              coral$datefilter <= input$daterange[2],variables] %>% na.omit()
    } else {
      coral[coral$id %in% input$id,variables]
    }
  })

  #### PLOT ####
  output$ID2 <- renderUI({
    dropdown(label = "Choix des ID a afficher",
             checkboxGroupInput("id2",NULL,
                                choices = levels(coral$id),
                                selected = levels(coral$id)))
  })

  selectedData <- reactive({
    coral[coral$id %in% input$id2,]
  })

  output$coralplot <- renderPlot({
    if("non" %in% input$reg){
      coral_plot(obj = selectedData(), x = input$day, y = input$ln) -> coralplot
      coralplot +
        geom_line() -> coralplot
    } else {
      coral_plot(obj = selectedData(), x = input$day, y = input$ln) -> coralplot
      coralplot +
        geom_smooth(method = "lm", se = FALSE, size = 0.5) -> coralplot
    }
    coralplot
  })


  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

})
