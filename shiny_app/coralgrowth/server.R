library(shiny)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(plotly)
library(googlesheets)
library(flow)
SciViews::R



### ----------------------- Partie logique du serveur ---------------------- ###
shinyServer(function(input, output, session) {

  # Madeleine :
  #coral_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTJLtfjjUM4VK6aM177ly9GCKyMHFrFqQdsqjhJCtpe4DUGuZWOe2fZWB5xTZEx3WAcW08BVEBFfn2C/pub?gid=0&single=true&output=csv"

  # Jordan :
  coral_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSoBfvhztFgALk1fcljBbYP03D-fRIEy7mu1DrHKZ--BXYZWHFxUujac_-gFSteM99p7CFQILT_eXcC/pub?gid=0&single=true&output=csv"
  read_csv(coral_url,
           col_types = cols( .default = col_character(),
                             date = col_datetime(),
                             weight = col_double(),
                             temperature = col_double(),
                             salinity = col_double() )) %>.%
    mutate(.,
           project = factor(project), author = factor(author),
           aqua = factor(aqua),
           condition = factor(condition),
           species = factor(species),
           id = factor(id, levels = 1:length(unique(id))),
           status = factor(status)
    ) -> df

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

  # Ajout de la colonne du poids squelettique
  df <- mutate(df,
                  skw = skeleton_weight(S = salinity,
                                        T = temperature,
                                        buoyant_weight = weight))

  # Nombre de ID different
  nbr_id <- unique(df$id)

  # Conditions
  nbr_condition <- unique(df$condition)

  # Projet
  nbr_projet <- unique(df$project)

  # Statut
  nbr_status <- unique(df$status)

  # Taux de croissance
  df %>.%
    group_by(., id) %>.%
    arrange(., date) %>.%
    mutate(., delta_date = as.numeric(difftime(date, date[1], units = "days")),
           ratio = round(((skw - skw[1]) / skw[1] / delta_date) * 100, digits = 5)) %>.%
    ungroup(.) -> df


  #========================__Fin du mainbloc__===========================

  # ----------------------- Selection des dates ---------------------------
  output$u_choice_date <- renderUI({

    dateRangeInput(inputId = "s_choice_date",
                   label = 'Date range input: ',
                   start = min(df$date), end = max(df$date)
    )
  })
  # ----------------------- Selection des ID -------------------------------

  # Recuperation de l'ID du fichier ui.R
  output$u_id <- renderUI({

    #Menu deroulant
    dropdown(
      checkboxGroupInput(inputId = "s_id",
                         label = NULL,
                         choices = c("All", "None", nbr_id),
                         selected = c("All")),
      width = "200px", size = "default", label = "ID",
      tooltip = tooltipOptions(placement = "right", title = "Choose ID")
    )
  })

  #----------------------Choix graphique (variable y)-----------------------
  output$u_choice_plot <- renderUI({

    radioButtons(inputId = "s_choice_plot", label = NULL,
                 choices = c("Skeleton mass",
                             "Growth rate"),
                 selected = "Skeleton mass")
  })

  #--------------------------Choix projet----------------------------------
  output$u_choice_project <- renderUI({

    selectInput(inputId = "s_choice_project",
                label = "Projet :",
                choices = nbr_projet,
                multiple = TRUE,
                selected = nbr_projet)
  })

  #-------------------------Choix condition--------------------------------
  output$u_choice_condition <- renderUI({

    selectInput(inputId = "s_choice_condition",
                label = "Condition :",
                choices = nbr_condition,
                multiple = TRUE,
                selected = nbr_condition)
  })
  #--------------------------Choix statut---------------------------------
  output$u_choice_status <- renderUI({

    selectInput(inputId = "s_choice_status",
                label = "Status :",
                choices = nbr_status,
                multiple = TRUE,
                selected = nbr_status)
  })

  #-------------------------Output de mon graphique----------------------
  output$u_plot <- renderPlotly({

    #updateSelectInput(session, inputId = "choix_condition", choices = nbr_condition, selected = if (input$select_allnone_condition) nbr_condition)
    df %>.%
      filter(.,
             project %in% input$s_choice_project,
             condition %in% input$s_choice_condition,
             status %in% input$s_choice_status,
             date >= input$s_choice_date[1] & date <= input$s_choice_date[2],
             id %in% input$s_id
             ) -> df

    # df <- filter(df, df$project %in% input$s_choice_project)
    # df <- filter(df, df$condition %in% input$s_choice_condition)
    # df <- filter(df, df$status %in% input$s_choice_status)
    # df <- filter(df, date >= input$s_choice_date[1] & date <= input$s_choice_date[2])
    # df <- filter(df, id %in% input$s_id)

    #Filtrer les lignes par rapport a ce qui a ete selectionne
    if ("All" %in% input$s_id) {
      updateCheckboxGroupInput(session,
                               inputId = "s_id",
                               label = "select All",
                               choices = c("All", "None", nbr_id),
                               selected = c( nbr_id)
      )
    }

    if ("None" %in% input$s_id) {
      updateCheckboxGroupInput(session,
                               inputId = "s_id",
                               label = "select All",
                               choices = c("All", nbr_id),
                               selected = NULL
      )
    }

    if ("Skeleton mass" %in% input$s_choice_plot) {
      yvar = df$skw
      y_axis_name <- "Skeleton mass (g)"
    }

    # Choix du taux de croissance
    if ("Growth rate" %in% input$s_choice_plot) {
      yvar = df$ratio
      y_axis_name <- "Growth rate"
    }

    ggplot(df, aes(x = date, y = yvar, colour = id)) +
      geom_point(size = 2, show.legend = FALSE) +
      geom_line(show.legend = FALSE) +
      xlab("Date") + ylab(y_axis_name) -> p

    #Pour remettre plotly, il faut changer : renderPlotly (server.R), plotlyOutput (ui.R) et decommenter la ligne d'en dessous :
    p <- ggplotly(p, show.legend = FALSE)
  })
  #------------------------------Sortie console-------------------------------#
  output$u_info <- renderPrint({

    #Affichage de la formule utilisé

    formule <- ""
    if ("Growth rate" %in% input$s_choice_plot) {
      formule <- "Growth rate = ( (skeleton_mass_n - skeleton_mass_n-1) / skeleton_mass_n-1 ) / (time_n - time_n-1) * 100"
    }
    if ("Skeleton mass" %in% input$s_choice_plot) {
      formule <- "Skeleton mass"
    }
    cat(formule, "\n")
  })

  # -------------------------Onglet tableau-----------------------------------
  # Recuperation de l'ID du fichier ui.R
  output$u_choice_table <- renderUI({

    radioButtons(inputId = "s_choice_table", label = "Filtrer",
                 choices = c("Yes", "No"),
                 selected = "No")
  })

  output$u_subchoice_table <- renderUI({

    dropdown(
      radioButtons(inputId = "s_subchoice_table",
                   label = "by",
                   choices = c("skeleton weight", "growth rates"),
                   selected = c("skeleton weight")),
      width = "200px",
      size = "default",
      label = "Variable type",
      tooltip = tooltipOptions(placement = "right", title = "Choice variable type")
    )
  })

  output$u_table <- DT::renderDataTable({

    if ("Yes" %in% input$s_choice_table) {
      updateRadioButtons(session,
                         inputId = "se_choice_filter",
                         label = NULL,
                         choices = input$s_choice_table ,
                         selected = input$s_choice_table)
      var = input$ui_choice_var
      if ("skeleton weight" %in% input$s_subchoice_table) {
        df %>.%
          filter(., skw > var, date == max(df$date)) %>.%
          arrange(., desc(skw)) %>.%
          group_by(., id) -> df
      }
      if ("growth rates" %in% input$s_subchoice_table) {
        df %>.%
          filter(., ratio > var, date == max(df$date)) %>.%
          arrange(., desc(ratio)) %>.%
          group_by(., id) -> df
      }
    }

    if ("No" %in% input$s_choice_table) {
      updateRadioButtons(session,
                         inputId = "se_choice_filter",
                         label = NULL,
                         choices = input$s_choice_table ,
                         selected = NULL)
    }
    DT::datatable(df)
  })
})
