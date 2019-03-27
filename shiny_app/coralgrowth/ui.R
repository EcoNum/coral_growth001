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
               uiOutput(outputId = "u_id"),# Selection des ID a afficher
               uiOutput(outputId = "u_choice_plot"), #Selection du graphique

               # checkboxInput(inputId = "select_allnone_condition",
               #               label = "Select All/None",
               #               value = TRUE),
               uiOutput(outputId = "u_choice_project"),
               uiOutput(outputId = "u_choice_condition"),
               uiOutput(outputId = "u_choice_status"),
               uiOutput(outputId = "u_choice_date")  #selection date
             ),

             # mainPanel : Volet de droite - Output
             mainPanel(
               tabsetPanel( # Sous-onglet
                 tabPanel(title = "Main plot",
                          plotlyOutput(outputId = "u_plot"),
                          #sortie console
                          verbatimTextOutput(outputId = "u_info")),
                 tabPanel(title = "Test plot")
               )
             )
    ),
    tabPanel("Data table", # Onglet principal 2
             # sidebar : volet de gauche - Input
             sidebarPanel(
               #uiOutput(outputId = "var_weight") # Choix du filtre à appliquer
               uiOutput(outputId = "u_choice_table"),
               uiOutput(outputId = "u_subchoice_table"),
               numericInput(inputId = "ui_choice_var",
                            label = "Skeleton mass higher than :",
                            value = 2)
             ),
             # mainPanel : Volet de droite - Output
             mainPanel(
               tabsetPanel(
                 tabPanel(title = "Table", DT::dataTableOutput("u_table"))
               )
             )
    )
  )
)
