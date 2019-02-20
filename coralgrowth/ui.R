library(shiny)

#Definie le UI, fluidpage modifie la taille des fenetres
shinyUI(
  fluidPage(
    titlePanel("Evolution de la croissance des coraux"),

    #Sidebar
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = "Coul", label = "Couleur du graphique",
                     choices = c("Bleu claire" = "lightblue", "Gris" = "gray"),
                                 selected = "lightblue")),
      mainPanel(
        plotOutput("monplot"),
        verbatimTextOutput("mes_morts"))
    )
  )
)

