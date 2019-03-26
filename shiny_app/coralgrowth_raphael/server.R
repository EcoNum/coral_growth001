#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source(file = "../../R/fonctions.R")

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

  # PARTIE DATASET
  # Création du choix des variables pour DATA - Renvoie vers le fichier ui.R
  output$choixvar <- renderUI({
    dropdown(label = "Choix des variables pour Data :",
             checkboxGroupInput("var", NULL,
                                names(coral)[-c(1, 10:26)], names(coral)[-c(1, 7:26)]))
  })
  # Création du choix des variables pour DATA - Renvoie vers le fichier ui.R
  output$choixvar2 <- renderUI({
    dropdown(label = "Choix des variables pour Growth :",
             checkboxGroupInput("var2", NULL,
                                names(coral)[c(2:6, 12, 13, 16:23)],
                                names(coral)[c(2:6, 12, 13, 16)]))
  })

  # Création du choix des ID - Renvoie vers le fichier ui.R
  output$ID <- renderUI({
    selectInput("id","Choix de l'ID a afficher :", choices = c("head", "All",levels(coral$id)))
  })

  # Création du range de date - Renvoie vers le fichier ui.R
  output$date_ <- renderUI({
    dateRangeInput("daterange","Date range :",
                   start = min(lubridate::date(coral$date), na.rm = TRUE),
                   end = max(lubridate::date(coral$date), na.rm = TRUE),
                   min = min(lubridate::date(coral$date), na.rm = TRUE),
                   max = max(lubridate::date(coral$date), na.rm = TRUE))
  })

  # Tableau pour la sortie Dataset/Data
  output$tableau <- DT::renderDataTable({
    if ("head" %in% input$id) {
      head(coral[,input$var], n = 5 )
    } else if ("All" %in% input$id) {
      coral[coral$datefilter >= input$daterange[1] &
              coral$datefilter <= input$daterange[2],input$var] %>% na.omit()
    } else {
      coral[coral$id %in% input$id,input$var]
    }
  })

  # Tableau pour la sortie Dataset/Growth
  output$tableauGrowth <- DT::renderDataTable({
    if ("head" %in% input$id) {
      head(coral[,input$var2], n = 5)
    } else if ("All" %in% input$id) {
      coral[coral$datefilter >= input$daterange[1] &
              coral$datefilter <= input$daterange[2],input$var2] %>% na.omit()
    } else {
      coral[coral$id %in% input$id,input$var2]
    }
  })

  # PARTIE PLOT
  # Création des ID - Renvoie vers le fichier ui.R
  output$ID2 <- renderUI({
    dropdown(label = "Choix des ID a afficher", # permet le menu déroulant
             checkboxGroupInput("id2",NULL,
                                choices = levels(coral$id),
                                selected = levels(coral$id)))
  })
  # Création d'un range pour la date
  output$date_2 <- renderUI({
    dateRangeInput("date_range","Date range :",
                   start = min(lubridate::date(coral$date), na.rm = TRUE),
                   end = max(lubridate::date(coral$date), na.rm = TRUE),
                   min = min(lubridate::date(coral$date), na.rm = TRUE),
                   max = max(lubridate::date(coral$date), na.rm = TRUE))
  })

  # Fonction de recalcul permettant d'utiliser les ids sélectionnées
  selectedData <- reactive({
    coral[coral$id %in% input$id2 &
            coral$datefilter >= input$date_range[1] &

            coral$datefilter <= input$date_range[2],]
  })

  # Création du graphique
  output$coralplot2 <- renderPlot({
    # Définition de la variable y (yvar) pour le graphe
    if("log" %in% input$lin_log){
      if("growth_diff" %in% input$calc){
        yvar <- "log_growth_diff"
      } else if ("growth_ratio" %in% input$calc){
        yvar <- "log_growth_ratio"
      } else if ("sp_growth_ratio" %in% input$calc){
        yvar <- "log_sp_growth_ratio"
      }
    } else {
      yvar <- input$calc
    }

    # Graphe avec option du calcul de la régression
    if("non" %in% input$reg){
      coral_plot(obj = selectedData(), x = input$day_date, y = yvar) -> coralplot
      coralplot +
        ggplot2::geom_line() -> coralplot
    } else {
      coral_plot(obj = selectedData(), x = input$day_date, y = yvar) -> coralplot
      coralplot +
        ggplot2::geom_smooth(method = "lm", se = FALSE, size = 0.5) -> coralplot
    }
    coralplot
  })

  # Sortie Texte : Description du calcul pour la variable y
  output$expli <- renderText({
    if("growth_diff" %in% input$calc){
      "growth_diff : poids du squelette - poids du squelette initial"
    } else if ("growth_ratio" %in% input$calc){
      "growth_ratio : (poids du squelette - poids du squelette initial) / poids du squelette initial"
    } else if ("sp_growth_ratio" %in% input$calc){
      "sp_growth_ratio : (poids du squelette - poids précédant du squelette) / poids précédant du squelette"
    }
  })

})
