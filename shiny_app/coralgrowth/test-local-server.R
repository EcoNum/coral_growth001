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


  # Madeleine :
  #coral_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTJLtfjjUM4VK6aM177ly9GCKyMHFrFqQdsqjhJCtpe4DUGuZWOe2fZWB5xTZEx3WAcW08BVEBFfn2C/pub?gid=0&single=true&output=csv"
#coral_url <-"https://docs.google.com/spreadsheets/d/e/2PACX-1vTJLtfjjUM4VK6aM177ly9GCKyMHFrFqQdsqjhJCtpe4DUGuZWOe2fZWB5xTZEx3WAcW08BVEBFfn2C/pub?gid=0&single=true&output=csv"
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
  nbr_statut <- unique(df$status)

  # Taux de croissance
  df %>.%
    group_by(., id) %>.%
    arrange(., date) %>.%
    mutate(., delta_date = as.numeric(difftime(date, date[1], units = "days")),
           ratio = round(((skw - skw[1]) / skw[1] / delta_date) * 100, digits = 5)) %>.%
    ungroup(.) -> df


  df %>.%
    group_by(., id) %>.%
    arrange(., date) %>.%
    mutate(., delta_skw = round((skw - skw[1]) / skw[1],digits = 2) ) %>.%
    ungroup(.) -> df

  #arrondir delta
  df$delta_date <- round(df$delta_date, 0)

  df %>.%
    filter(., delta_date == 0) %>.%
    ggplot(., aes(x = skw)) +
    geom_histogram()
  df$weight <- na.omit(df$weight)
  df  %>.%
    drop_na(., weight)  %>.%
  ggplot(., aes(x = date, y = weight, colour = id)) +
    geom_point(size = 2, show.legend = FALSE, na.rm = TRUE) +
    geom_line(show.legend = FALSE, na.rm = TRUE)  -> p

  p <- ggplotly(p, show.legend = FALSE)
