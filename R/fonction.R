SciViews::R
library(shiny)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(plotly)




#Determination du nombre de ligne de tableau a utiliser
#Baser sur la premiere valeur NA rencontre dans la colonne "temp"
ma_derniere_ligne <- function(){
  a <- 0
  for (i in tablo$temp) {
    if (!is.na(i)) {
      a <- a + 1
    }
  }
  return(a)
}

#Calcul du poids squelettique
#A verifier : la pression et le rho aragonite

skeleton_weight <- function(S = tablo$salinity, T = tablo$temp, P = 0, buoyant_weight = tablo$weight, rho_aragonite = 2930){

  if (is.numeric(tablo$weight)) {
    mutate(tablo, skel_wgt = )
  }
  rho_water <- seacarb::rho(S = S, T = T , P = P)
  skl_wgt <- buoyant_weight / 1 - (rho_water / rho_aragonite)
  return(skl_wgt)
}
