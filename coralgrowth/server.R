#importation de la librairie shiny
library(shiny)
library(ggplot2)
library(lubridate)
library(tidyverse)

#importation de mes donnees (format csv)
#correction a faire : faire un lien symbolique
botablo <-  read.table("~/shared/Github/coral_growth001/data/raw/monBordel/tablo2.csv", header = TRUE, sep = ";", dec = ",")



#Determination du nombre de ligne de tableau a utiliser
#Baser sur la premiere valeur NA rencontre dans la colonne "temp"
ma_derniere_ligne <- function(){
  a <- 0
  for (i in botablo$temp) {
    if (!is.na(i)) {
      a <- a + 1
    }
  }
  return(a)
}

#Extraction des lignes contenant des valeurs et des 5 colonnes (id, weight,temp,salinity et date)
botablo <- botablo[1:ma_derniere_ligne(), 1:5]

#Je fais une copie pour pouvoir travailler dessus sans creer de probleme d'affichage
cp_botablo <- botablo

#Partie logique du serveur
shinyServer(function(input, output) {
  #variable de mon graphique :
  output$monplot <- renderPlot({

    #Mon graphique
    #changer le type de l'ID de "int" a "factor"
    fID <- factor(botablo$ID)

    #changer le type de la date de "character" a "date"
    madate <- dmy(botablo$date)


    #correctiom a faire : remettre a la ligne
    ggplot(botablo, aes(x = as.Date(madate), y = weight, colour = as.factor(fID))) + geom_point(size = 2, show.legend = FALSE) + geom_line(show.legend = F)

  })

  output$mes_morts <- renderPrint({

    #remplacer les weight de valeurs NA des id 81 a 84 par "oublie"
    #cela va servir pour ne pas les compter dans les boutures mortes
    cp_botablo[81:84, 2] <- "oublie"

    #la valeur de la bouture 16 est a rejeter
    cp_botablo[16, 2] <- "a rejeter"


    #Compter les id de valeur NA pour weight
    # les 2 lignes ci-dessous empeche la visualisation du graphique
    ID_NA <- subset(cp_botablo, is.na(weight) == TRUE, ID)
    ID_NA <- unique(ID_NA)
    ID_NA <- ID_NA$ID

    #nombre de boutures mortes :
    bouture_morte <- length(ID_NA)
    cat("Nombre de bouture morte : ", bouture_morte, "\n ma_derniere_ligne() :", ma_derniere_ligne())



  })


})
