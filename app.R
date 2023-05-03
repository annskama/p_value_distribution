#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(ggplot2)
library(dplyr)
library(magrittr)
library(plotly)
library(tidyverse)

here()
list.files(here("data"))
my_data <- read.csv(here
                    ("data", "150211FullFile_AllStatcheckData_Automatic1Tail.csv"),
                    sep = ";")
colnames(my_data)[18] <- "journal"
colnames(my_data)[19] <- "year"

#cleaning the dataset from missing values in reported_p_value
my_data <- my_data[!is.na(my_data$Reported_P_Value) & !is.na(my_data$Computed) &
                     !is.na(my_data$Reported_Comparison) &!is.na(my_data$year),] 

#transforming p_values into numeric format
my_data$p_rep_num<- as.numeric(gsub(",",".", my_data$Reported_P_Value))
my_data$p_calc_num<- as.numeric(gsub(",",".", my_data$Computed))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("P-value Distribution Project"),
    
    h4("The first option to show the distribution"),
    
    plotOutput("distPlot"),

    # Sidebar with a slider input for the year to report 
    fluidRow(
      
      column(6,
             offset = 1,
             sliderInput("scale",
                        "Please select the p-values range (x axis scale)",
                        min = 0,
                        max = 1,
                        value = c(0,0.1),
                        width = "150%"),
              sliderInput("years",
                  "Please select the year",
                  min = 1985,
                  max = 2013,
                  value = 2013,
                  animate = animationOptions(interval = 1000, loop = FALSE, 
                                             playButton = icon('play', "fa-2x"), 
                                             pauseButton = icon('pause', "fa-2x")),
                  width = "150%")),   
      
      column(3,
             offset = 1,
              selectInput("p_filter",
                        label = "Show p-values reported",
                        choices = c("all", "only as exact values"),
                        selected = "all",
                        multiple = FALSE) # not allow more than one selection
        )),

        # Show a plot of the generated distribution
      
    hr(),
    h4("The second option to show the distribution (with plotly)"),       
           plotlyOutput("animated"),
      
    hr(),
    hr(),
    h4("Rounding Errors"),
    hr(),
    
    fluidRow(
      
      column(6,
             offset = 1,
             plotOutput("rounding")),
      
      column(4,
             selectInput("threshold",
                         label = "Please select the threshold",
                         choices = c(0.05, 0.01, 0.001),
                         selected = 0.05,
                         multiple = FALSE),
             textOutput("accuracy"))),
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate dataset for the specified year
        temp_df    <- my_data[my_data$year==input$years,]
        if (input$p_filter == "only as exact values") {
          temp_df <- temp_df[temp_df$Reported_Comparison =="=",]}
        else {}

        # draw the histogram with the specified number of bins
        p <- ggplot(data=temp_df)
        p + geom_density(aes(x=p_calc_num, 
                               fill="Computed"), na.rm=TRUE, alpha=0.5)+ 
          geom_density(aes(x=p_rep_num, fill="Reported"), 
                         na.rm=TRUE, alpha=0.5) + 
          geom_vline(xintercept = 0.05, linetype="dotted", 
                     color = "blue", size=0.7) +
          geom_vline(xintercept = 0.01, linetype="dotted", 
                     color = "blue", size=0.7) +
          xlim(input$scale[1],input$scale[2]) + ylim(0,100)+
          xlab("p-values")+ylab("Density")+
          ggtitle(paste("Comparison of reported and computed p-values, year", input$years))+
          theme(plot.title = element_text(hjust = 0.5, size = 15, face="bold"), 
                axis.title = element_text(size=13))+
          scale_fill_manual(values = c("orange", "darkblue"), name="Type of p-value")
        
        
        #CODE FOR HISTOGRAM
        #p <- ggplot(data=temp_df)
        #p + geom_histogram(aes(x=p_calc_num, y=..density.., 
        #                     fill="Computed"), na.rm=TRUE, bins=20, alpha=0.5)+ 
        #  geom_histogram(aes(x=p_rep_num, y=..density.., fill="Reported"), 
        #               na.rm=TRUE, bins = 20, alpha=0.5) + 
        #  xlim(input$scale[1],input$scale[2]) + ylim(0,50)+
        #  xlab("p-values")+ylab("Density")+
        #  ggtitle("Comparison of reported and computed p-values")+
        #  theme(plot.title = element_text(hjust = 0.5, size = 15, face="bold"), 
        #        axis.title = element_text(size=13))+
        #  scale_fill_manual(values = c("orange", "darkblue"), name="Type of p-value")
        
              
    })
    
    
    output$animated <- renderPlotly({
      
      if (input$p_filter == "only as exact values") {
        my_data <- my_data[my_data$Reported_Comparison =="=",]}
      else {}
      
      q <- ggplot(data=my_data, aes(frame=year)) +
        geom_density(aes(x=p_calc_num, fill="Computed"), alpha=0.5, size=0.6)+ 
        geom_density(aes(x=p_rep_num, fill="Reported"), alpha=0.5, size=0.6) + 
        geom_vline(xintercept = 0.05, linetype="dotted", 
                   color = "blue", linewidth=0.7) +
        geom_vline(xintercept = 0.01, linetype="dotted", 
                   color = "blue", linewidth=0.7) +
        geom_vline(xintercept = 0.001, linetype="dotted", 
                   color = "blue", linewidth=0.7) +
        # Setting the limits via the coordinate system to adjst the a axis without changing
        # the dataset drawn (that is not drawn data points are not deleted)
        coord_cartesian(xlim = c(0,0.1), 
                        ylim = c(0,100), expand = FALSE) +
        xlab("p-values")+ylab("Density")+
        ggtitle("Comparison of reported and computed p-values, year")+
        theme(plot.title = element_text(hjust = 0.5, size = 12, face="bold"), 
              axis.title = element_text(size=10))+
        scale_fill_manual(values = c("orange", "darkblue"), name="Type of p-value")
      
      q <- ggplotly(q)
      animation_opts(q, frame=1000, transition =0, easing = "linear",
                     redraw = FALSE, 
                     mode = "immediate")
      animation_button(q, x = 0, xanchor = "right", y=-0.3, yanchor = "bottom", 
                       label_play = "Play", label_pause = "Pause", visible = TRUE)
      animation_slider(q, currentvalue = list(prefix = "Year ", xanchor = "right", yanchor=100,
                                              font = list(color = "darkviolet", size = 15)))
      
    })
 
    output$rounding <- renderPlot({
      # generate dataset for the specified year
      
      my_temp1 <- my_data[my_data$Reported_Comparison=="<",]
      my_temp1 <- my_temp1[my_temp1$p_rep_num == input$threshold,]
      # calculating the share of correctly reported values
      correct_share <- nrow(my_temp1[my_temp1$p_calc_num <input$threshold,])/nrow(my_temp1)*100
      
      # rearranging the dataset 
      vars <- c("N", "p_rep_num", "p_calc_num", "journal", "year")
      my_temp1 <- select(my_temp1, all_of(vars))
      my_temp1 <- pivot_longer(data =my_temp1, cols=c("p_rep_num", "p_calc_num"),
                              names_to='p_type',
                              values_to='values')
      q <- ggplot(data = my_temp1, 
                  mapping = aes(x=p_type, y=values, group = N))
      q + geom_point(na.rm = TRUE, size=0.1)+ geom_line(size=0.5, aes(color= year))+
        geom_hline(yintercept = as.numeric(input$threshold), linetype="dotted", 
                   color = "blue", size=0.7)+
        xlab("") + ylab("p-values") +
        ggtitle(paste("Calculated P-values for p<", input$threshold))+
        theme(plot.title = element_text(hjust = 0.5, size = 15, face="bold"), 
              axis.title = element_text(size=13))+
        scale_x_discrete(labels=c("p_calc_num" = "Calculated", "p_rep_num" = "Reported threshold"))+
        ylim(0,1)+
        geom_text(x=1.5, y=0.8, size=4, label= paste0("Accuracy ratio\n", 
                                                      round(correct_share,0), "%"),
                  colour="darkblue")
      
      })
    
    output$accuracy <- renderText({
      my_temp1 <- my_data[my_data$Reported_Comparison=="<",]
      my_temp1 <- my_temp1[my_temp1$p_rep_num == input$threshold,]
      # calculating the share of correctly reported values
      
      paste0("P-values below ",input$threshold, 
                                         " were reported correctly in ", 
             round(nrow(my_temp1[my_temp1$p_calc_num <input$threshold,])/nrow(my_temp1)*100,0), 
                                         "% of cases")
      
      })%>%
      bindCache(input$threshold)
    
       
}

# Run the application 
shinyApp(ui = ui, server = server)
