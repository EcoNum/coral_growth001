library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)

shinyUI(

  navbarPage(
    "Evolution de la croissance des coraux", # Titre onglet 1
    tabPanel("Graphique", # Onglet principal 1
             # Sidebar : volet de gauche - Input
             sidebarPanel(
               uiOutput("ID") # Selection des ID a afficher
             ),

             # mainPanel : Volet de droite - Output
             mainPanel(
               tabsetPanel( # Sous-onglet
                 tabPanel("Sous-onglet 1 : Graphique",
                          plotlyOutput("monplot"),
                          #sortie console
                          verbatimTextOutput("boutures_mortes")),
                 tabPanel("Sous-onglet 2 : ")
               )
             )
    ),
    tabPanel("Tableau de donnee", # Onglet principal 2
             # sidebar : volet de gauche - Input
             sidebarPanel(


             ),
             # mainPanel : Volet de droite - Output
             mainPanel(
               tabsetPanel(
                 tabPanel("Beau tablo", DT::dataTableOutput("tableau"))
               )
             )
    )
  )
)
