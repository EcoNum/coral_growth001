library(shiny)
library(shinyWidgets)
library(DT)
library(plotly)
library(shinythemes)


shinyUI(
  navbarPage(
    # theme = shinytheme("slate"),
    title = "Coral growth", # Titre onglet 1
    tabPanel("Plot", # Onglet principal 1
             # Sidebar : volet de gauche - Input
             sidebarPanel(
               uiOutput(outputId = "ID"),# Selection des ID a afficher
               uiOutput(outputId = "choice_plot"), #Selection du graphique

               # checkboxInput(inputId = "select_allnone_condition",
               #               label = "Select All/None",
               #               value = TRUE),
               uiOutput(outputId = "choice_project"),
               uiOutput(outputId = "choice_condition"),
               uiOutput(outputId = "choice_status"),
               uiOutput(outputId = "choice_date")  #selection date
             ),

             # mainPanel : Volet de droite - Output
             mainPanel(
               tabsetPanel( # Sous-onglet
                 tabPanel("Main plot",
                          plotlyOutput(outputId = "monplot"),
                          #sortie console
                          verbatimTextOutput(outputId = "boutures_mortes")),
                 tabPanel("Test plot")
               )
             )
    ),
    tabPanel("Data table", # Onglet principal 2
             # sidebar : volet de gauche - Input
             sidebarPanel(
               #uiOutput(outputId = "var_weight") # Choix du filtre à appliquer
               uiOutput(outputId = "choice_table"),
               uiOutput(outputId = "subchoice_table"),
               numericInput(
                 inputId = "choice_var", label = "Skeleton mass higher than :", value = 2),
               verbatimTextOutput(outputId = "var_txt")
             ),
             # mainPanel : Volet de droite - Output
             mainPanel(
               tabsetPanel(
                 tabPanel("Table", DT::dataTableOutput("tableau"))
               )
             )
    )
  )
)
