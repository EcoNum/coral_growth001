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

# Define UI for application that draws a histogram
shinyUI(

  navbarPage(
    "Coral Growth",
    tabPanel("Dataset",
      # Sidebar
      sidebarPanel(
        h3("Options du tableau"),
        hr(),
        uiOutput("choixvar"),
        uiOutput("ID"),
        uiOutput("date_")
      ),

      # Show Data
      mainPanel(
        tabsetPanel(
          tabPanel("Data", tableOutput("tableau")),
          tabPanel("Growth", tableOutput("tableauGrowth"))
        )
      )
    ),
    tabPanel("Plot",
      sidebarPanel(
        h3("Options graphiques"),
        selectInput("ln", "ln ?", choices = c("growth", "growth_ln"), selected = "growth"),
        selectInput("day", "by day ?",  choices = c("day", "by_day"), selected = "day"),
        selectInput("reg", "linear model ?",  choices = c("oui", "non"), selected = "non"),
        uiOutput("ID2")
      ),
      # Show Plot
      mainPanel(
        plotOutput("coralplot")
      )
    )
  )
)





