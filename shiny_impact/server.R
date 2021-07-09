#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(zoo)
library(DT)
library(CausalImpact)

# DT::dataTableOutput("table")output$contents <- DT::renderDataTable({ gapminder })

# Define server logic required to draw a histogram
shinyServer(function(input, output) 
    {output$contents <- DT::renderDataTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, header = input$header)
    }, caption="Electronic momentary assesment allows symptom monitoring and evaluation. Day by day smartphone based symptom assesment and a visualisation of the course for the symptomsmight allow a better evaluation of sympotms at a follow-up appointment.")
    
    output$myPlot = renderPlot({   
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        data<-read.csv(inFile$datapath)
        time.points <- seq.Date(as.Date("2021-01-01"), by = 1, length.out = 366)
        
        data<-zoo(cbind(symptom1=data$V1, symptom2=data$V2), time.points)
        autoplot(data, label=T, facets = NULL)+
            geom_vline(xintercept = as.numeric(as.Date("2021-05-02")), 
                       color = "black", 
                       linetype=4)+theme_classic() +ggtitle("symptom change over time")
})
    
     output$myResult = renderPrint({
         inFile <- input$file1
         
         if (is.null(inFile))
             return(NULL)
         
         data<-read.csv(inFile$datapath)
         time.points <- seq.Date(as.Date("2021-01-01"), by = 1, length.out = 366)
         
         data<-zoo(cbind(symptom1=data$V1, symptom2=data$V2), time.points)
         #define pre and post period
         pre.period <- as.Date(c("2021-01-01", "2021-05-01"))
         post.period <- as.Date(c("2021-05-02", "2021-05-15"))
         #estimate causal impact
         impact <- CausalImpact(data,  pre.period, post.period)
         summary(impact, "report")
         
    
                         })
    
                            
})

