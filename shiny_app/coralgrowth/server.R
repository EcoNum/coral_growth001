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

  # Madeleine :
  #coral_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTJLtfjjUM4VK6aM177ly9GCKyMHFrFqQdsqjlhJCtpe4DUGuZWOe2fZWB5xTZEx3WAcW08BVEBFfn2C/pub?gid=0&single=true&output=csv"

  # Jordan :
  coral_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSoBfvhztFgALk1fcljBbYP03D-fRIEy7mu1DrHKZ--BXYZWHFxUujac_-gFSteM99p7CFQILT_eXcC/pub?gid=0&single=true&output=csv"
  read_csv(coral_url,
           col_types = cols( .default = col_character(),
                             date = col_datetime(),
                             weight = col_double(),
                             temperature = col_double(),
                             salinity = col_double() )) %>.%
    mutate(., project = factor(project), author = factor(author),
           aqua = factor(aqua),
           condition = factor(condition),
           species = factor(species),
           id = factor(id, levels = 1:length(unique(id))),
           statut = factor(statut)
    ) -> tablo

  ### Calcul du poids squelettique :
  #a corriger : rho_aragonite
  #P = Pression hydrostatique, elle vaut 0 a la surface
  skeleton_weight <- function(S, T, P = 0,
                              buoyant_weight,
                              rho_aragonite = 2930){

    rho_water <- seacarb::rho(S = S, T = T , P = P)
    skl_wgt <- buoyant_weight / (1 - (rho_water / rho_aragonite))
    skl_wgt <- round(skl_wgt, digits = 3)
    return(skl_wgt)
  }

  # #Ajout de la colonne du poids squelettique
  tablo <- mutate(tablo,
                  skw = skeleton_weight(S = salinity,
                                        T = temperature,
                                        buoyant_weight = weight))

  # Nombre de ID different
  nbr_id <- unique(tablo$id)

  # Taux de croissance
  tablo %>.%
    group_by(., id) %>.%
    arrange(., date) %>.%
    mutate(., delta_date = as.numeric(difftime(date, date[1], units = "days")),
           ratio = round(((skw - skw[1]) / skw[1] / delta_date) * 100, digits = 5)) %>.%
    ungroup(.) -> tablo


  #===========================__Fin du mainbloc__============================

 # --------------------------Selection des dates---------------------------
  output$choice_date <- renderUI({
    dateRangeInput(inputId = "dateRange",
                   label = 'Date range input: yyyy-mm-dd',
                   start = min(tablo$date), end = max(tablo$date)
    )
  })
  # --------------------------Selection des ID---------------------------------

  # Recuperation de l'ID du fichier ui.R
  output$ID <- renderUI({

    #Menu deroulant
    dropdown(
      checkboxGroupInput(inputId = "choix_id", label = NULL,
                         choices = c("All", "None", nbr_id),
                         selected = c("All")),
      width = "200px", size = "default", label = "ID",
      tooltip = tooltipOptions(placement = "right", title = "Choix des ID")
    )
  })

  #----------------------Choix taux de croissance-----------------------------
  output$choice_plot <- renderUI({
    radioButtons(inputId = "choix_graph", label = NULL,
                 choices = c("Masse squelettique",
                             "Masse immerge",
                             "Taux de croissance"),
                 selected = "Masse immerge")
  })

  # --------------------------Output de mon graphique---------------------------
  output$monplot <- renderPlotly({
    tablo <- filter(tablo, date >= input$dateRange[1] & date <= input$dateRange[2])
    tablo <- filter(tablo, tablo$id %in% input$choix_id)
    #Filtrer les lignes par rapport a ce qui a ete selectionne
    if ("All" %in% input$choix_id) {
      updateCheckboxGroupInput(session,
                               inputId = "choix_id",
                               label = "select All",
                               choices = c("All", "None", nbr_id),
                               selected = c( nbr_id)
      )
    }

    if ("None" %in% input$choix_id) {
      updateCheckboxGroupInput(session,
                               inputId = "choix_id",
                               label = "select All",
                               choices = c("All", nbr_id),
                               selected = NULL
      )
    }

    if ("Masse squelettique" %in% input$choix_graph) {
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
    formule <- "ok"
    if ("Taux de croissance" %in% input$choix_graph) {
      formule <- "Taux de croissance = ( (masse_squelettique_n - masse_squelettique_n-1) / masse_squelettique_n-1 ) / (temps_n - temps_n-1) * 100"
    }
    if ("Masse immerge" %in% input$choix_graph) {
      formule <- "Masse immerge brute"
    }
    if ("Masse squelettique" %in% input$choix_graph) {
      formule <- "Masse squelettique"
    }



    cat(formule, "\n")
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
      radioButtons(inputId = "souschoix_table",
                   label = "by",
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
      updateRadioButtons(session,
                         inputId = "choix_table2",
                         label = "filtrer?",
                         choices = input$choix_table ,
                         selected = input$choix_table)
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
      updateRadioButtons(session,
                         inputId = "choix_table2",
                         label = "filtrer?",
                         choices = input$choix_table ,
                         selected = NULL)
    }
    DT::datatable(tablo)
  })

})
