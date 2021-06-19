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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$contents <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, header = input$header)
    })
    
    output$myPlot = renderPlot({   
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        data<-read.csv(inFile$datapath)
        time.points <- seq.Date(as.Date("2021-01-01"), by = 1, length.out = 366)
        
        data<-zoo(cbind(symptom1=data$V1, symptom2=data$V2), time.points)
        autoplot(data, label=T, facets = NULL )+
            geom_vline(xintercept = as.numeric(as.Date("2021-05-02")), 
                       color = "black", 
                       linetype=4)+theme_classic() 
})
    
    # output$myPlot = renderPlot({
    #     data = data.frame(x = 1:10, y = runif(10))
    #     myPlot(ggplot(data, aes(x = x, y = y)) + geom_point())
    #     data
    #                             })
    # 
    
                            
})

