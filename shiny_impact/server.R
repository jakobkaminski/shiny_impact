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
library(tidyverse)


# DT::dataTableOutput("table")output$contents <- DT::renderDataTable({ gapminder })

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    my_data <- reactive({
        inFile <- input$file1
        if (is.null(inFile)){return(NULL)}else{
                              
                              data<-read.csv(inFile$datapath)
                              time.points <- seq.Date(as.Date("2021-01-01"), by = 1, length.out = 366)
                              
                              data<-zoo(cbind(symptom1=data$V1, symptom2=data$V2), time.points)
                              
                              data<-as.data.frame(data)
                              data$day<-row.names(data)
                              
                              # data <- data.frame(
                              #   day = as.Date("2017-06-14") - 0:364,
                              #   value = runif(365) + seq(-140, 224)^2 / 10000
                              # )
                              data_long<-data %>% gather(key="symptom",value="value", `symptom1`,`symptom2`)
                              data_long$day<-as.Date(data_long$day)
                              return(data_long)}})
        my_dates <- reactive({
            inDate <- input$predateRange
            inDatepost <- input$postdateRange
            zoomdateRange <- input$zoomdateRange
            dates<-data.frame(inDate,inDatepost,zoomdateRange)
            return(dates)})
    # output$contents <- renderTable({
    #     # input$file1 will be NULL initially. After the user selects
    #     # and uploads a file, it will be a data frame with 'name',
    #     # 'size', 'type', and 'datapath' columns. The 'datapath'
    #     # column will contain the local filenames where the data can
    #     # be found.
    #     inFile <- input$file1
    #     
    #     if (is.null(inFile))
    #         return(NULL)
    #     
    #     table_1<-read.csv(inFile$datapath, header = input$header)
    #     table_1
    # }, caption="Electronic momentary assesment allows symptom monitoring and evaluation. Day by day smartphone based symptom assesment and a visualisation of the course for the symptomsmight allow a better evaluation of sympotms at a follow-up appointment.")
    
    # output$myPlot = renderPlot({   
    #     inFile <- input$file1
    #     inDate <- input$date
    #     if (is.null(inFile))
    #         return(NULL)
    #     
    #     data<-read.csv(inFile$datapath)
    #     time.points <- seq.Date(as.Date("2021-01-01"), by = 1, length.out = 366)
    #     
    #     data<-zoo(cbind(symptom1=data$V1, symptom2=data$V2), time.points)
    #     autoplot(data, label=T, facets = NULL)+
    #         geom_vline(xintercept = as.numeric(inDate), 
    #                    color = "black", 
    #                    linetype=4)+theme_classic() +ggtitle("symptom change over time")
    #                             })
    
    output$nicerPlot = renderPlot({   
        data_long<-my_data()
        dates<-my_dates()
        #Most basic timeline plot
        p <- ggplot(data_long, aes(x=day, y=value, col=symptom)) +
            geom_line() +
            scale_color_manual(values=c('#999999','#E69F00'))+
            geom_rect(aes(xmin=dates$inDate[1], xmax=dates$inDate[2], ymin=0, ymax=3, fill="pre-intervention"),color = NA, alpha=0.03)+
            geom_rect(aes(xmin=dates$inDatepost[1], xmax=dates$inDatepost[2], ymin=0, ymax=3, fill="post-intervention"),color = NA, alpha=0.03)+
            xlab("") +
            theme(axis.text.x=element_text(angle=60, hjust=1))+
            scale_x_date(limit=dates$zoomdateRange)+
            scale_fill_manual('Period',
                              values = c("#ebe6ff", "#e6fff6"),
                              guide = guide_legend(override.aes = list(alpha = 1)))+
            theme_minimal()

        p
    })
    output$progress1Box <- renderInfoBox({
        data_long<-my_data()
        dates<-my_dates()
        averagepre<-data_long %>% subset(symptom=="symptom1") %>% subset(day > dates$inDate[1] & day < dates$inDate[2]) %>% mutate(sum=sum(value)/length(value))

        infoBox(
            "pre-period", paste0(round(averagepre$sum[1], digits=2), " points"), icon = icon("list"),
            color = "light-blue"
        )
    })
    output$progressBox <- renderInfoBox({
        data_long<-my_data()
        dates<-my_dates()
        averagepost<-data_long %>% subset(symptom=="symptom1") %>% subset(day > dates$inDatepost[1] & day < dates$inDatepost[2]) %>% mutate(sum=sum(value)/length(value))

        infoBox(
            "post period", paste0(round(averagepost$sum[1], digits=2), " points"), icon = icon("list"),
            color = "purple"
        )
    })
    # 
    output$approvalBox <- renderInfoBox({
        data_long<-my_data()
        dates<-my_dates()
        averagepre<-data_long %>% subset(symptom=="symptom1") %>% subset(day > dates$inDate[1] & day < dates$inDate[2]) %>% mutate(sum=sum(value)/length(value))
        averagepost<-data_long %>% subset(symptom=="symptom1") %>% subset(day > dates$inDatepost[1] & day < dates$inDatepost[2]) %>% mutate(sum=sum(value)/length(value))
        infoBox(
            "difference", paste0(round(averagepre$sum[1]-averagepost$sum[1], digits=2)), icon = icon("thumbs-up", lib = "glyphicon"),
            color = "yellow"
        )
    })
     output$myResult = renderPrint({
         inFile <- input$file1
         inDate <- input$predateRange
         inDatepost <- input$postdateRange
         # inDate<-as.numeric(inDate)
         if (is.null(inFile))
             return(NULL)

         data<-read.csv(inFile$datapath)
         time.points <- seq.Date(as.Date("2021-01-01"), by = 1, length.out = 366)

         data<-zoo(cbind(symptom1=data$V1, symptom2=data$V2), time.points)
         #define pre and post period
         pre.period <- inDate
         post.period <- inDatepost
         #estimate causal impact
         impact <- CausalImpact(data,  pre.period, post.period)
         summary(impact, "report")


                         })

                            
})

