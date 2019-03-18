library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)
library(shinythemes)

shinyUI(
  navbarPage(
    # theme = shinytheme("slate"),
    title = "Evolution de la croissance des coraux", # Titre onglet 1
    tabPanel("Graphique", # Onglet principal 1
             # Sidebar : volet de gauche - Input
             sidebarPanel(
               uiOutput(outputId = "ID"),# Selection des ID a afficher
               uiOutput(outputId = "Ratio")
             ),

             # mainPanel : Volet de droite - Output
             mainPanel(
               tabsetPanel( # Sous-onglet
                 tabPanel("Sous-onglet 1 : Graphique",
                          plotlyOutput(outputId = "monplot"),
                          #sortie console
                          verbatimTextOutput(outputId = "boutures_mortes")),
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
