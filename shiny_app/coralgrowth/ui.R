library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)
library(shinythemes)
library(shinyWidgets)


shinyUI(
  navbarPage(
    #theme = shinytheme("slate"),
    title = "Coral growth", # Titre onglet 1
    #### Onglet principal : Graphique
    tabPanel(title = "Plot",
             ## Sidebar : volet de gauche - Input
             sidebarPanel(
               uiOutput(outputId = "u_choice_project"),
               uiOutput(outputId = "u_choice_condition"),
               uiOutput(outputId = "u_choice_status"),
               uiOutput(outputId = "u_choice_id"), # Sélection des ID à afficher
               uiOutput(outputId = "u_choice_plot"),     #Sélection du graphique
               uiOutput(outputId = "u_choice_nbr_day"),      # Sélection de Xvar
               uiOutput(outputId = "u_choice_date")              #Sélection date
             ),
             ## MainPanel : Volet de droite - Output
             mainPanel(
               tabsetPanel(
                 # Sous-onglet
                 tabPanel(title = "Main plot",
                          plotlyOutput(outputId = "u_plot", height = "600px" ),
                          #sortie console
                          verbatimTextOutput(outputId = "u_info"))
                 #tabPanel(title = "Test plot")
               )
             )
    ),
    ### Onglet principal : Tableau de donnée
    tabPanel("Data table",
             # Sidebar : Volet de gauche - Input
             # sidebarPanel(
             # ),
             # MainPanel : Volet de droite - Output
             mainPanel(
               tabsetPanel(
                 tabPanel(title = "Table", DTOutput(outputId = "u_table")
                          )
               )
             )
    ),
    ### Onglet principal : Aide
    tabPanel(title = "Help",
             fluidRow(
               uiOutput(outputId = "u_lien")
             ))
  )

)
