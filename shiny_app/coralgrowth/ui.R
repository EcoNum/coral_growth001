library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)
library(shinythemes)

#if (interactive()) {
shinyUI(
  navbarPage(
    # theme = shinytheme("slate"),
    title = "Evolution de la croissance des coraux", # Titre onglet 1
    tabPanel("Graphique", # Onglet principal 1
             # Sidebar : volet de gauche - Input
             sidebarPanel(
               uiOutput(outputId = "ID"),# Selection des ID a afficher
               uiOutput(outputId = "choice_plot"), #Selection du graphique
               uiOutput(outputId = "choice_date") #selection date
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
               #uiOutput(outputId = "var_weight") # Choix du filtre à appliquer
               uiOutput(outputId = "choice_table"),
               uiOutput(outputId = "subchoice_table"),
               numericInput(
                 inputId = "choice_var", label = "Masse squelettique supérieur à :", value = 2),
               verbatimTextOutput(outputId = "var_txt")
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
