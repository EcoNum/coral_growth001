#importation de la librairie shiny
library(shiny)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(dplyr)
library(plotly)
library(scales)

#Fonction de Raphael :
source(file = "../R/fonctions.R")
#Mes fonctions
source(file = "../R/fonction.R")


#importation de mes donnees (format csv)
#correction a faire : chemin relatif
tablo <-  read.table("~/shared/Github/coral_growth001/data/raw/monBordel/tablo2.csv", header = TRUE, sep = ";", dec = ",")


#Determination du nombre de ligne de tableau a utiliser                             ^
#/!\  >>>>>>>>  Baser sur la premiere valeur NA rencontre dans la colonne "temp"  <<<<<<<  /!\
#Fonction a ameliorer de facon a ne garder seulement les lignes completes (ID, weight, temp, salinity, date)

ma_derniere_ligne <- function(){
  a <- 0
  for (i in tablo$temp) {
    if (!is.na(i)) {
      a <- a + 1
    }
  }
  return(a)
}

#Extraction des 5 colonnes (id, weight,temp,salinity et date)
tablo <- tablo[1:ma_derniere_ligne(), 1:5]



### Calcul du poids squelettique :
#a corriger : rho_aragonite
#P = Pression hydrostatique, elle vaut 0 a la surface
skeleton_weight <- function(S = tablo$salinity, T = tablo$temp, P = 0, buoyant_weight = tablo$weight, rho_aragonite = 2930){

  rho_water <- seacarb::rho(S = S, T = T , P = P)
  skl_wgt <- buoyant_weight / (1 - (rho_water / rho_aragonite))
  return(skl_wgt)
}

#Ajout de la colonne du poids squelettique
tablo <- mutate(tablo, skw = skeleton_weight())

#Fonction reactive qui permet de changer le type de l'ID de "int" a "factor"
#N'est pas permise pour mon "botablo"
#
# factor_id <- reactive({
#   factor_ID <- factor(tablo$ID)
#   return(factor_ID)
# })



# changer le type de l'ID de "int" a "factor"
factor_ID <- factor(tablo$ID)

#changer le type de la date de "character" a "date"
madate <- as.Date(dmy(tablo$date))

#Tableau a afficher sur l'app Shiny :
botablo <- transmute(tablo, ID = factor_ID, "Masse immerge (g)" = tablo$weight, "Masse squelettique (g)" = skeleton_weight(), "Temperature (c)" = tablo$temp, "Salinite (g/L)" = tablo$salinity, Date = madate)

#Je fais une copie pour pouvoir travailler dessus sans creer de probleme d'affichage
cp_tablo <- tablo

### ----------------------- Partie logique du serveur ---------------------- ###
shinyServer(function(input, output) {

  # --------------------------Selection des ID---------------------------------
  output$ID <- renderUI({
    #Suppression des doublons des ID pour le menu deroulant
    ID_doublon <- which(duplicated(tablo$ID))
    choix_ID <- tablo$ID[-ID_doublon]

    #Menu deroulantl
    dropdown(label = "Selection des ID",
    checkboxGroupInput(inputId = "choix_id", label = NULL,
                       choices = choix_ID, selected = choix_ID))

  })
  # -------Fonction de recalcul permettant d'utiliser les ID selectionnes-------
  # selected_ID <- reactive({
  #
  #   # factor_ID <- factor_ID %in% input$choix_id
  # })

  # --------------------------Output de mon graphique---------------------------
  output$monplot <- renderPlotly({

    #changer le type de l'ID de "int" a "factor"
    # factor_ID <- factor(tablo$ID)

    #changer le type de la date de "character" a "date"
    # madate <- as.Date(dmy(tablo$date))

    #tablo[tablo$ID] <- input$choix_id
    #correction a faire : remettre a la ligne
    p <- ggplot(tablo, aes(x = madate, y = skw, colour = factor_ID)) + geom_point(size = 2, show.legend = FALSE) + geom_line(show.legend = F) + xlab("Date") + ylab("Poids squelettique (g)") #+ theme( axis.line = element_line(color = "darkgray", size = 2, linetype = "solid"))
    #p + scale_x_date(labels = date_format("%d-%m-%y"))
    p <- ggplotly(p)

    ### Legende qui ne fonctionne pas, probleme d'attribution...
    #return(p)
    # Legende lorsque l'on passe son curseur :
    # ma_legende <- paste("ID :", factor_ID, "\n", "Poids :", tablo$weight, "\n", "Date :", madate)
    # pp <- ggplotly(p)
    # pp <- style(pp, text = ma_legende, hoverinfo = "text")
    # return(pp)

  })
  #------------------------------Sortie console----------------------------------#
  output$boutures_mortes <- renderPrint({

    #remplacer les weight de valeurs NA des id 81 a 84 par "oublie"
    #cela va servir pour ne pas les compter dans les boutures mortes
    cp_tablo[81:84, 2] <- "oublie"

    #la valeur de la bouture 16 est a rejeter
    cp_tablo[16, 2] <- "a rejeter"


#Compter les id de valeur NA pour weight
    #les 2 lignes ci-dessous empeche la visualisation du graphique
    ID_NA <- subset(cp_tablo, is.na(weight) == TRUE, ID)
    ID_NA <- unique(ID_NA)
    ID_NA <- ID_NA$ID

    #nombre de boutures mortes :
    nbr_bouture_morte <- length(ID_NA)

    cat("Nombre de bouture morte : ", nbr_bouture_morte, "\n ma_derniere_ligne() :", ma_derniere_ligne(), "\n test:","\n")

  })

  # -----------------------------Tableau----------------------------------------
  output$tableau <- DT::renderDataTable({DT::datatable(botablo)
  })

})
