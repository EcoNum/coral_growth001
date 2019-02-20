#importation de la librairie shiny
library(shiny)
library(ggplot2)
library(tidyverse)
library(forcatsl)

#importation de mes donnees (format csv)
botablo <-  read.table("~/shared/Github/coral_growth001/data/raw/monBordel/tablo2.csv", header=TRUE, sep =";", dec = ",")

#Extraction des 80 premieres lignes et des 5 colonnes (id, weight,temp,salinity et date)
botablo <- botablo[1:168, 1:5]

#Partie logique du serveur

monplot <- ggplot(botablo, aes(x = as.Date(date),y = weight, colour =as.factor(ID)))
monplot <- monplot + geom_point(size = 2, show.legend = FALSE)
monplot <- monplot + geom_line(show.legend = F)

monplot

