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
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
        ),
        mainPanel("insights from data",
                  fluidRow(
                        splitLayout(cellWidths = c("33%", "33%", "33%"),
                    DT::dataTableOutput("contents"), 
                    plotOutput("myPlot"), verbatimTextOutput("myResult")))
            
                )
                )
                )
    )
