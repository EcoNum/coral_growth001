library(shiny)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(plotly)
library(googlesheets)
library(flow)
SciViews::R



#if (interactive()) {
###--------------------------------------------------------------------------###
### ----------------------- Partie logique du serveur ---------------------- ###
shinyServer(function(input, output, session) {

  #comment faire un retour a la ligne ci-dessous ?
   coral_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSoBfvhztFgALk1fcljBbYP03D-fRIEy7mu1DrHKZ--BXYZWHFxUujac_-gFSteM99p7CFQILT_eXcC/pub?gid=0&single=true&output=csv"
   tablo <- read.csv(coral_url)

   output$dataset <- renderTable({datasetInput()})
  datasetInput <- reactive({
    switch(input$dataset,
           "template" = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQSZmj5iLaK2jFEbF-zEFLEDdzJnh5e1qJHCrEsVMhowbfN_W11JVKNKtCqVXJgiGwsFsNZVSrFJ7Qg/pub?gid=0&single=true&output=csv"),
           "plateau" = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTJLtfjjUM4VK6aM177ly9GCKyMHFrFqQdsqjhJCtpe4DUGuZWOe2fZWB5xTZEx3WAcW08BVEBFfn2C/pub?gid=0&single=true&output=csv"),
           "jordan" = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSoBfvhztFgALk1fcljBbYP03D-fRIEy7mu1DrHKZ--BXYZWHFxUujac_-gFSteM99p7CFQILT_eXcC/pub?gid=0&single=true&output=csv"),
           "victor" = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTRCuaoMOuoV_iQUscot1OqXLd0PZxFd2fz01LnEfVxuF3pUPj221CwslIThTUWWtPV1XAWFgZcAyfN/pub?gid=0&single=true&output=csv"),
           "jessica" = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vSAYHrkL4ZfJJMV_Y9dL9iT3jM5IT2MguLXl56fPqHuTDyO36Uazy9KJMhHWbp-eeu4O3LjGVdhngQ_/pub?gid=0&single=true&output=csv"))
  })

  # tablo <- datasetInput()
  # observeEvent(eventExpr = input$button, {
  #   cat("probleme", input$url)
  # })
  #
  # eventReactive(eventExpr = input$button, {
  #   coral_url <- input$url
  #   cat("ok", coral_url)
  # })




  ### Calcul du poids squelettique :
  #a corriger : rho_aragonite
  #P = Pression hydrostatique, elle vaut 0 a la surface
  skeleton_weight <- function(S = tablo$salinity, T = tablo$temperature, P = 0,
                              buoyant_weight = tablo$weight, rho_aragonite = 2930){

    rho_water <- seacarb::rho(S = S, T = T , P = P)
    skl_wgt <- buoyant_weight / (1 - (rho_water / rho_aragonite))
    skl_wgt <- round(skl_wgt, digits = 3)
    return(skl_wgt)
  }
  #
  # #Ajout de la colonne du poids squelettique
  tablo <- mutate(tablo, skw = skeleton_weight())

  # changer le type de l'ID de "int" a "factor"
  tablo$id <- factor(tablo$id)

  #changer le type (mode) de la date
  tablo$date <- ymd_hms(tablo$date)

  #parse_date_time(tablo$date, locale = locale("fr"), orders = "dmy HMS")
  tablo$date <- as_datetime(tablo$date)

  # Nombre de ID different
  nbr_id <- unique(tablo$id)

  # Taux de croissance
  tablo %>.%
    group_by(., id) %>.%
    arrange(., date) %>.%
    mutate(., delta_date = difftime(date, date[1], units = "days" ),
           ratio = round(((skw - skw[1]) / skw[1] / as.double(delta_date)) * 100, digits = 5)) -> tablo1

  # a cause du group_by je ne peux pas modifier directement "tablo"
  tablo <- mutate(tablo, ratio = tablo1$ratio)

#============================================================================
# --------------------------Selection des ID---------------------------------

# Recuperation de l'ID du fichier ui.R
  output$ID <- renderUI({

#Menu deroulant
    dropdown(
      checkboxGroupInput(inputId = "choix_id", label = NULL,
                         choices = c("All", "None", nbr_id), selected = c("All")),
      width = "200px", size = "default", label = "ID",
      tooltip = tooltipOptions(placement = "right", title = "Choix des ID")
    )
  })

  #----------------------Choix taux de croissance-----------------------------
  output$choice_plot <- renderUI({
      radioButtons(inputId = "choix_graph", label = NULL,
                         choices = c("Masse squelettique", "Masse immerge", "Taux de croissance"),
                         selected = "Masse immerge")
  })

  # --------------------------Output de mon graphique---------------------------
  output$monplot <- renderPlotly({
    tablo <- datasetInput()
    # observeEvent(eventExpr = input$button, {
    #   cat("probleme", input$url)
    # })
    #
    # eventReactive(eventExpr = input$button, {
    #   coral_url <- input$url
    #   cat("ok", coral_url)
    # })

    # tablo <- read.csv(coral_url)

    ### Calcul du poids squelettique :
    #a corriger : rho_aragonite
    #P = Pression hydrostatique, elle vaut 0 a la surface
    skeleton_weight <- function(S = tablo$salinity, T = tablo$temperature, P = 0,
                                buoyant_weight = tablo$weight, rho_aragonite = 2930){

      rho_water <- seacarb::rho(S = S, T = T , P = P)
      skl_wgt <- buoyant_weight / (1 - (rho_water / rho_aragonite))
      skl_wgt <- round(skl_wgt, digits = 3)
      return(skl_wgt)
    }

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

    # Taux de croissance
    tablo %>.%
      group_by(., id) %>.%
      arrange(., date) %>.%
      mutate(., delta_date = difftime(date, date[1], units = "days" ),
             ratio = round(((skw - skw[1]) / skw[1] / as.double(delta_date)) * 100, digits = 5)) -> tablo1

    # a cause du group_by je ne peux pas modifier directement "tablo"
    tablo <- mutate(tablo, ratio = tablo1$ratio)

    #Filtrer les lignes par rapport a ce qui a ete selectionne
    if ("All" %in% input$choix_id) {
      updateCheckboxGroupInput(session, inputId = "choix_id", label = "select All",
                         choices = c("All", "None", nbr_id), selected = c("All", nbr_id)
      )
    }

    if ("None" %in% input$choix_id) {
      updateCheckboxGroupInput(session, inputId = "choix_id", label = "select All",
                               choices = c("All", nbr_id), selected = NULL
      )
    }

    #Par defaut on calcule le poids squelettique
    else {
      tablo <- filter(tablo, tablo$id %in% input$choix_id)
      yvar = tablo$skw
      y_nom_axe <- "Masse squelettique (g)"
    }

    # Choix du taux de croissance
    if ("Taux de croissance" %in% input$choix_graph) {
      yvar = tablo$ratio
      y_nom_axe <- "Taux de croissance"
    }

    #Choix de la masse immerge
    if ("Masse immerge" %in% input$choix_graph) {
      yvar = tablo$weight
      y_nom_axe <- "Masse immerge"
    }
      ggplot(tablo, aes(x = date, y = yvar, colour = id)) +
      geom_point(size = 2, show.legend = FALSE) + geom_line(show.legend = F) +
      xlab("Date") + ylab(y_nom_axe) -> p

    #Pour remettre plotly, il faut changer : renderPlotly (server.R), plotlyOutput (ui.R) et decommenter la ligne d'en dessous :
    p <- ggplotly(p)
  })
  #------------------------------Sortie console----------------------------------#
  output$boutures_mortes <- renderPrint({
    ### Cette partie sert a compter les boutures mortes
    #Affichage de la formule utilisé
    if ("Taux de croissance" %in% input$choix_graph) {
      formule <- "Taux de croissance = ( (masse_squelettique_n - masse_squelettique_n-1) / masse_squelettique_n-1 ) / (temps_n - temps_n-1) * 100"
    }
    if ("Masse immerge" %in% input$choix_graph) {
      formule <- "Masse immerge brute"
    }
    if ("Masse squelettique" %in% input$choix_graph) {
      formule <- "Masse squelettique"
    }
    url_txt <- input$url

    var = input$choice_var
    cat(formule, "\n\n", url_txt)
  })

  # -------------------------Onglet tableau-------------------------------------
  # Recuperation de l'ID du fichier ui.R
  output$choice_table <- renderUI({
    radioButtons(inputId = "choix_table", label = "Filtrer",
                 choices = c("Yes", "No"),
                 selected = "Yes")
  })

  output$subchoice_table <- renderUI({
    dropdown(
        radioButtons(inputId = "souschoix_table", label = "by",
                           choices = c("skeleton weight", "growth rates"),
                           selected = c("skeleton weight")),
        width = "200px", size = "default", label = "Variable type",
        tooltip = tooltipOptions(placement = "right", title = "Choice variable type")
      )
  })


  output$var_txt <- renderPrint({
    var = input$choice_var
    # cat("var :", var)
  })

  output$tableau <- DT::renderDataTable({

    if ("Yes" %in% input$choix_table) {
      updateRadioButtons(session, inputId = "choix_table2", label = "filtrer?",
                               choices = input$choix_table , selected = input$choix_table)
      var = input$choice_var

      if ("skeleton weight" %in% input$souschoix_table) {
        tablo %>.%
          filter(., skw > var, date == max(tablo$date)) %>.%
          arrange(., desc(skw)) %>.%
          group_by(., id) -> tablo
      }
      if ("growth rates" %in% input$souschoix_table) {
        tablo %>.%
          filter(., ratio > var, date == max(tablo$date)) %>.%
          arrange(., desc(ratio)) %>.%
          group_by(., id) -> tablo
      }
    }

    if ("No" %in% input$choix_table) {
      updateRadioButtons(session, inputId = "choix_table2", label = "filtrer?",
                         choices = input$choix_table , selected = NULL)
    }
    DT::datatable(tablo)
  })

})

