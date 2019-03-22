
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

  output$plot <- renderPlotly({
    skeleton_weight <- function(S = tablo$salinity, T = tablo$temperature, P = 0,
                                buoyant_weight = tablo$weight, rho_aragonite = 2930){

      rho_water <- seacarb::rho(S = S, T = T , P = P)
      skl_wgt <- buoyant_weight / (1 - (rho_water / rho_aragonite))
      skl_wgt <- round(skl_wgt, digits = 3)
      return(skl_wgt)
    }
    tablo <- datasetInput()
    #Ajout de la colonne du poids squelettique
    tablo <- mutate(tablo, skw = skeleton_weight())

    # changer le type de l'ID de "int" a "factor"
    tablo$id <- factor(tablo$id)

    #changer le type (mode) de la date
    tablo$date <- ymd_hms(tablo$date)

    #parse_date_time(tablo$date, locale = locale("fr"), orders = "dmy HMS")
    tablo$date <- as_datetime(tablo$date)

    # Nombre de ID different
    nbr_id <- unique(tablo$id)
    p <- ggplot(tablo, aes(x = date, y = weight, colour = id)) +
      geom_point(size = 2, show.legend = FALSE) + geom_line(show.legend = F) +
      xlab("Date")
    ggplotly(p)
  })
})
