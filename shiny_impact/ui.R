#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# here is the documentationi for the shiny dashboard https://rstudio.github.io/shinydashboard/get_started.html

library(shiny)
library(shinydashboard)
library(DT)

dashboardPage(skin = "purple",
    dashboardHeader(title = "Evaluation dashboard"),
    dashboardSidebar(
        fileInput("file1", "Choose CSV File to load data",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        # dateInput('date',
        #           label = 'input date of intervention: yyyy-mm-dd',
        #           value = Sys.Date())
        dateRangeInput('predateRange',
                       label = 'input date range for pre-intervention period: yyyy-mm-dd (purple shade in plot)',
                       start = Sys.Date() - 28, end = Sys.Date() - 14),
        dateRangeInput('postdateRange',
                       label = 'input date range for post-intervention period: yyyy-mm-dd (turquoise shade in plot)',
                       start = Sys.Date() - 13, end = Sys.Date()),
        dateRangeInput('zoomdateRange',
                   label = 'input date range for field of view of plot: yyyy-mm-dd',
                   start = Sys.Date() - 200, end = Sys.Date()+200)
                    ),
    
    dashboardBody(
        # fluidRow(
        # # box(tableOutput("contents")), 
        #             box(plotOutput("myPlot"), width=600)), 
        fluidRow(
            box(plotOutput("nicerPlot"), width=600)),
        fluidRow(
            box(title="Warning!",
                width = 600, background = "black",
                "For research purposes only: we here display simple summary statistics for symptom 1:"
            )),
        fluidRow(
            # A static infoBox
            infoBoxOutput("progress1Box"),
            # Dynamic infoBoxes
            infoBoxOutput("progressBox"),
            infoBoxOutput("approvalBox")),
        box(title="Warning!",
            width = 600, background = "black",
            "For research purposes only: we here report summary from impact analysis for target variable symptom 1 and dependent but not affected variable symptom 2:"
        ),
        fluidRow(
            box(verbatimTextOutput("myResult"), width = 600
                     )
                 )
        )
)


# # Define UI for application that draws a histogram
# shinyUI(fluidPage(
#     sidebarLayout(
#         sidebarPanel(
#             fileInput("file1", "Choose CSV File",
#                       accept = c(
#                           "text/csv",
#                           "text/comma-separated-values,text/plain",
#                           ".csv")
#             ),
#             tags$hr(),
#             checkboxInput("header", "Header", TRUE)
#         ),
#         mainPanel("insights from data",
#                   fluidRow(
#                         splitLayout(cellWidths = c("33%", "33%", "33%"),
#                     DT::dataTableOutput("contents"), 
#                     plotOutput("myPlot"), verbatimTextOutput("myResult")))
#             
#                 )
#                 )
#                 )
#     )
