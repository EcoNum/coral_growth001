#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(DT)

# Define UI for application that draws a histogram
shinyUI(

  navbarPage(
    "Coral Growth", # titre
    tabPanel("Dataset", # Onglet principal 1
      # Sidebar : volet de gauche avec les options
      sidebarPanel(
        h3("Options des tableaux"), # titre de niveau 3
        hr(), # ligne
        uiOutput("choixvar"), # sélection des variables pour DATA- Créer dans le fichier server (permet d'adapter en fct de ce qui est dans le jeu de données)
        br(),
        uiOutput("choixvar2"),# sélection des variables pour GROWTH- Créer dans le fichier server (permet d'adapter en fct de ce qui est dans le jeu de données)
        br(),
        uiOutput("ID"), # sélection des id - Créer dans le fichier server
        uiOutput("date_") # range de date - Créer dans le fichier server
      ),

      # Show Data : Volet de droite - Sortie
      mainPanel(
        tabsetPanel( # onglet Data et Growth
          tabPanel("Data", DT::dataTableOutput("tableau")),
          tabPanel("Growth", DT::dataTableOutput("tableauGrowth"))
        )
      )
    ),
    tabPanel("Plot", # Onglet principale 2
      sidebarPanel(
        h3("Options graphiques"),
        hr(),
        selectInput("calc", "calcul", choices = c("growth_diff", "growth_ratio", "sp_growth_ratio"),
                    selected = "growth_diff"), # choix du type de calcul
        selectInput("lin_log", "linear or log", choices = c("linear", "log"),
                    selected = "linear"), # linéaire ou transfo log
        selectInput("day_date", "date or day",  choices = c("day", "by_day", "date"),
                    selected = "day"), # Option jour ou date
        selectInput("reg", "linear model ?",  choices = c("oui", "non"),
                    selected = "non"), # Option régression
        uiOutput("date_2"), # range de date - Créer dans le fichier server
        uiOutput("ID2") # Sélection des id - Créer dans le fichier server
      ),
      # Show Plot
      mainPanel(
        plotOutput("coralplot2"),
        textOutput("expli")
      )
    )
  )
)
