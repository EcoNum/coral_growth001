library(shiny)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(plotly)
library(flow)
library(shinyWidgets)
library(coral.growth)

### ----------------------__Partie logique du serveur__----------------------
shinyServer(function(input, output, session) {

  # Madeleine :
  #coral_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTJLtfjjUM4VK6aM177ly9GCKyMHFrFqQdsqjhJCtpe4DUGuZWOe2fZWB5xTZEx3WAcW08BVEBFfn2C/pub?gid=0&single=true&output=csv"

  # Jordan :
  coral_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSoBfvhztFgALk1fcljBbYP03D-fRIEy7mu1DrHKZ--BXYZWHFxUujac_-gFSteM99p7CFQILT_eXcC/pub?gid=0&single=true&output=csv"

  #Importation et format des colonnes
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
           status = factor(status),
           skw = skeleton_weight(buoyant_weight = weight, S = salinity,
                                 T = temperature)
    ) -> df

  df %>.%
    group_by(., id) %>.%
    arrange(., date) %>.%
    mutate(.,
           lag_date = dplyr::lag(date),
           delta_date = (as.numeric(difftime(date, date[1], units = "days"))),
           delta_lag_date = dplyr::lag(delta_date),
           lag_skw = dplyr::lag(skw),
           ratio = round(((skw - lag_skw) / lag_skw / (delta_date - delta_lag_date))*100, digits = 3),
           gr_expo = coral.growth::growth_rate(skw_t = skw, skw_ini = lag_skw,
                                               date_t = delta_date, date_ini = delta_lag_date,
                                               method = "exponential"),
           gr_lin = coral.growth::growth_rate(skw_t = skw, skw_ini = lag_skw,
                                              date_t = delta_date, date_ini = delta_lag_date,
                                              method = "linear"),
           gr_lin_std = coral.growth::growth_rate(skw_t = skw, skw_ini = lag_skw,
                                                  date_t = delta_date, date_ini = delta_lag_date,
                                                  method = "linear_std"),
           delta_date = round(delta_date, digits = 0)) %>.%
    ungroup(.) -> df

  # Nombre de ID different
  nbr_id <- unique(df$id)
  # Conditions
  nbr_condition <- unique(df$condition)
  # Projet
  nbr_projet <- unique(df$project)
  # Statut
  nbr_status <- unique(df$status)

  ### -----------__Fin traitement du tableau de données__ ----------- ###

  #=====================================================================#

  # ---------------------- Selection des dates --------------------------
  output$u_choice_date <- renderUI({

    dateRangeInput(inputId = "s_choice_date",
                   label = 'Date range input: ',
                   start = min(df$date), end = Sys.Date(),
                   min = min(df$date)
    )
  })
  # ----------------------- Selection Xvar ------------------------------
  output$u_choice_nbr_day <- renderUI({

    radioButtons(inputId = "s_choice_nbr_day",
                 label = 'Xvar : ',
                 choices = c("Date", "Number of days"),
                 selected = "Number of days"
    )
  })

  #--------------------------Selection id-------------------------------
  output$u_choice_id <- renderUI({
    pickerInput(inputId = "s_choice_id",
                label = "Choice ID :",
                choices = nbr_id,
                options = list(`actions-box` = TRUE),
                multiple = T,
                selected = nbr_id)

  })

  # ------------------------- Choix des ID ------------------------------
  observe({
    print(input$s_choice_id)
  })

  #----------------------Choix graphique (variable y)--------------------
  output$u_choice_plot <- renderUI({

    radioButtons(inputId = "s_choice_plot", label = "Yvar :",
                 choices = c("Buoyant mass", "Skeleton mass",
                             "Exponential growth rate", "Linear growth rate",
                             "Linear standard growth rate"),
                 selected = "Buoyant mass")
  })

  #--------------------------Choix projet--------------------------------
  output$u_choice_project <- renderUI({

    selectInput(inputId = "s_choice_project",
                label = "Project :",
                choices = nbr_projet,
                multiple = TRUE,
                selected = nbr_projet)
  })

  #-------------------------Choix condition------------------------------
  output$u_choice_condition <- renderUI({

    selectInput(inputId = "s_choice_condition",
                label = "Condition :",
                choices = nbr_condition,
                multiple = TRUE,
                selected = nbr_condition)
  })

  #--------------------------Choix statut--------------------------------
  output$u_choice_status <- renderUI({

    selectInput(inputId = "s_choice_status",
                label = "Status :",
                choices = nbr_status,
                multiple = TRUE,
                selected = nbr_status)
  })



  ###----------------------Output de mon graphique--------------------###
  output$u_plot <- renderPlotly({

# Filtre en fonction des choix
    df %>.%
      filter(.,
             project %in% input$s_choice_project,
             condition %in% input$s_choice_condition,
             status %in% input$s_choice_status,
             date >= input$s_choice_date[1] & date <= input$s_choice_date[2],
             id %in% input$s_choice_id
             ) -> df

    # Choix de la masse squelettique
    if ("Skeleton mass" %in% input$s_choice_plot) {
      yvar = df$skw
      y_axis_name <- "Skeleton mass (g)"
    }

    # Choix de la masse immergée
    if ("Buoyant mass" %in% input$s_choice_plot) {
      yvar = df$weight
      y_axis_name <- "Buoyant mass (g)"
    }

    # choix du taux de croissance exponentiel
    if ("Exponential growth rate" %in% input$s_choice_plot) {
      yvar = df$gr_expo
      y_axis_name <- "Exponential growth rate [%/d]"
    }

    # choix du taux de croissance exponentiel
    if ("Linear growth rate" %in% input$s_choice_plot) {
      yvar = df$gr_lin
      y_axis_name <- "Linear growth rate [g/d]"
    }

    # choix du taux de croissance exponentiel
    if ("Linear standard growth rate" %in% input$s_choice_plot) {
      yvar = df$gr_lin_std
      y_axis_name <- "Linear growth rate [%/d]"
    }

    # Choix par nombre de jour
    if ("Number of days" %in% input$s_choice_nbr_day) {
      xvar = df$delta_date
      xlabel = "Day"
    }

    # Choix par date du jour
    if ("Date" %in% input$s_choice_nbr_day) {
      xvar = df$date
      xlabel = "Date"
    }

    ggplot(df, aes(x = xvar, y = yvar, colour = id)) +
      geom_point(size = 2, show.legend = FALSE, na.rm = TRUE) +
      geom_line(show.legend = FALSE, na.rm = TRUE) +
      xlab(xlabel) + ylab(y_axis_name) -> p

    p <- ggplotly(p, show.legend = FALSE)
  })


  ###-------------------------Sortie console-------------------------###
  output$u_info <- renderPrint({

    #Affichage de la formule utilisé
    formule <- ""


    if ("Buoyant mass" %in% input$s_choice_plot) {
      formule <- "Buoyant mass (g)"
    }
    if ("Skeleton mass" %in% input$s_choice_plot) {
      formule <- "Skeleton mass (g)"
    }

    if ("Exponential growth rate" %in% input$s_choice_plot) {
      formule <- "Exponential growth rate = ((log(skeleton_mass_n) - log(skeleton_mass_n-1))) / (time_n - time_n-1) * 100"
    }

    if ("Linear growth rate" %in% input$s_choice_plot) {
      formule <- "Linear growth rate = (skeleton_mass_n - skeleton_mass_n-1) / (time_n - time_n-1)"
    }

    if ("Linear standard growth rate" %in% input$s_choice_plot) {
      formule <- "Linear standard growth rate = ( (skeleton_mass_n - skeleton_mass_n-1) / skeleton_mass_n-1 ) / (time_n - time_n-1) * 100"
    }



    # Calculs boutures mortes
    nbr_dead <- as.numeric(count(unique(subset(df, status == "dead",id))))
    death_rate <- as.numeric(round((nbr_dead / length(levels(nbr_id))) * 100, digits = 2))
    id_dead <- unique(subset(df, status == "dead",id))
    id_dead <- id_dead$id

    cat("Yvar : ", formule, "\n", "\n",
        "Species :", as.character(unique(df$species)), "\n", "\n",
        "Number of deads cuttings :",  nbr_dead, "\n",
        "ID dead cuttings : ", paste(id_dead, collapse = ", "), "\n",
        "Death rate :", death_rate, "%")
  })


  # --------------------------Onglet tableau----------------------------#
  output$u_table <- renderDT({
    datatable(df, filter = "top")
  })

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

  output$u_choice_var <- renderUI({

    numericInput(inputId = "s_choice_var",
                 label = if (input$s_subchoice_table == "growth rates")
                   {"Growth rates higher than :"}
                 else {"Skeleton weight higher than :"},
                 value = 1)
  })

  output$u_lien <- renderUI({
    url1 <- a("Bookdown CoralGrowth", href = "https://econum.github.io/coral_growth001_book/")
    url2 <- a("Github repository", href = "https://github.com/EcoNum/coral_growth001")
    tagList(tags$h2( "Help page at the following adress : " ),
            tags$h3(url1),
            tags$h3(url2))
  })

})

