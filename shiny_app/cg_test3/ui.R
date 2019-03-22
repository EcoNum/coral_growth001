#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  navbarPage("coral growth dashboard",
             tabPanel("Template",
                      h3("The template"),
                      dataTableOutput("dt"),
                      hr(),
                      h3("The data dictionnary"),
                      a("https://docs.google.com/document/d/1ArMMA2AbAsnM-itw_0Z0kA6FvMDcpU9po-rmbsYkcl0/edit?usp=sharing"),
                      hr(),
                      h3("A csv template is available here"),
                      downloadLink("downloadData", "Download")),
             tabPanel("Data import",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("dataset", "Dataset Choices",
                                      choices = c("dt1", "dt2", "dt3", "dt4"), selected = "dt1")),

                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput("plot"))
                      )
                      )
  )
  )
)
