#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(dplyr)
library(forecast)
library(fpp3)
library(fable)
library(tsibble)
library(tidyverse)
library(tsibbledata)
library(rsconnect)


rsconnect::setAccountInfo(name='region-and-retail', token='BCD29FFD3F7EB5896C72153A27AC8C9B', secret='fHlfWeYKQ7JBdM+bJvCKBxhX7yzP6wHYooFzQ2HQ')



# Define UI for application that draws a histogram
ui <- fluidPage(
   

   tags$head(
     tags$style(HTML("
       @import url('//fonts.googleapis.com/css?family=Judson');
       
       body {
          background-color: #72646B;
          color: black;
          font-size: 20px;
          
       }
       h2 {
          font-family: 'Judson';
          color: white;
          margin-top: 100px;
          margin-left: 70px;
       }
       h3{
          font-family: 'Judson';
          color: white;
          margin-top: 45px;
          margin-left: 70px;
       }
       .shiny-input-container {
          font-family: 'Judson';
       }
       .navbar-default{
          font-family: 'Judson';
          font-size: 15px;
       }
       img{
          margin-left: 100px;
       }
     "))
   ),
   
   # Application title
   navbarPage("Region & Retail",
    
     tabPanel("Home",
              
          mainPanel(
              fluidRow(
                column(width = 3, h2("Learn the future trends of the consumers within the Midwest Region and the Retail Consumer Sales")),
                column(width = 9, align = "right", img(src='shopping_trend.jpg', height = '500px', width='750px',style = 'border-radius: 7%'))
              ),
              h3("Team Members: Keerti Kolakaluri, Shreya Varghese, and Chaitrali Ghanekar")
          )    
     ),
     tabPanel("Vizualize trend",
   
           # Sidebar with a slider input for number of years for Region 
           sidebarLayout(
              sidebarPanel(
                 sliderInput("h",
                             "Select Number of Years:",
                             min = 1,
                             max = 10,
                             value = 3)
              ),
              
              # Show a plot of the generated distribution
              mainPanel(
                 plotOutput("regPlot", height = "400px", width = "700px"),
                 br()
              )
           ),
           
           # Sidebar with a slider input for number of years for Retail 
           sidebarLayout(
             sidebarPanel(
               sliderInput("h2",
                           "Select Number of Years:",
                           min = 1,
                           max = 10,
                           value = 3)
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("retailPlot", height = "400px", width = "700px"),
               br()
             )
            )
         ),
     tabPanel("Learn More",
              br(),
              fluidRow(
                column(width = 3, 
                       tags$div(
                         style = "margin: 10px;",
                         plotOutput("regionalPlot", height = "400px", width = "110%")
                       )
                ),
                column(width = 3, 
                       tags$div(
                         style = "margin: 10px;",
                         plotOutput("regionchange", height = "400px", width = "110%")
                       )
                ),
                column(width = 3, 
                       tags$div(
                         style = "margin: 10px;",
                         plotOutput("quarter_retail", height = "400px", width = "110%")
                       )
                )
              ),
              # Add spacing between the rows
              tags$style(type="text/css", "#Learn\\ More .row {margin-bottom:20px;}"),
              br(),
              tags$div(
                style = "background-color: lightblue; color: black; padding: 10px; text-align: center;style = 'border-radius: 15%'",
                tags$p("There were two datasets 'Regional Data' and 
                                         'Retail Data' which were used for the time series
                                         decomposition and visualization. For the regional 
                                         dataset, the team had chosen to analyze the Midwest region data."),
                tags$p("Therefore, for the time series analysis, the regional portion focused 
                                         on the 'Total Midwest' column for the number of consumer units and 
                                         the retail portion focused on the 'Total Retail Sales' over the time 
                                         period of 2005 - 2021."),
                tags$p("For the regional dataset, we visualized the total 
                                         consumers in the midwest region against the years from 2005 to 2019 using
                                         a line plot. The inference from the graph is that the data shows an upward trend.
                                         The other plot also shows the change in sales over the years. 
                                         The plot generally shows an increasing trend except for the ranges $50,000 to 69,999
                                         and 70,000 to and more."),
                tags$p("For the retail data, we can see that the number of retail 
                                         sales went significantly higher after the year 2012. The sales for the month of April 
                                         dropped in 2020.")
              )
     )
     )
     
    )

# Define server logic required to draw both models for Region and Retail
server <- function(input, output) {
   
   output$regPlot <- renderPlot({
      # generate h based on input$h from ui.R
     regional <- readxl::read_excel("Regional_merged_data.xlsx")
     
     region_cons <- regional %>%
       filter(Item == "Number of consumer units (in thousands)") %>%
       select(Year, Item, `Total midwest`) %>%
       as_tsibble(index = Year)
     
     fit <- region_cons %>%
       model(ARIMA(`Total midwest`))
     
     fc <- forecast(fit, h = input$h)
      
      # draw the plot with the specified number of years
     region_cons %>%
       autoplot(`Total midwest`) +
       autolayer(fc) +
       labs(y = "Total Consumers", title = "Predicting for Midwest Total Consumers")
   })
   
   output$retailPlot <- renderPlot({
     # generate h based on input$h from ui.R
     
     retail <- readxl::read_excel("retail_data.xlsx")
     
     retail <- retail %>%
       filter(`Kind of Business` == "Retail sales, total") %>%
       as_tsibble(index = Year)
     
     retaildata <- retail %>%
       pivot_longer(c(Jan., Feb., Mar., Apr., May, Jun., Jul., Aug., Sep., Oct., Nov., Dec.), names_to = "key", values_to = "value")
     
     
     retaildata$Month <- paste(retaildata$Year, retaildata$key, sep = " ")
     
     fit1 <- retaildata %>% model(
       arima012 = ARIMA((value) ~ 1 + pdq(0, 1, 2))
     )
     
     fc1 <- forecast(fit1, h = input$h2)
     
     
     
     # draw the plot with the specified number of years
     retaildata %>%
       autoplot(`value`) +
       autolayer(fc1) +
       labs(y = "Total Consumers", title = "Predicting for Retail Sales")
   })
   # Plot for total consumers  in the midwest
   output$regionalPlot <- renderPlot({
     regional <- readxl::read_excel("Regional_merged_data.xlsx")
     
     region_cons <- regional %>%
       filter(Item == "Number of consumer units (in thousands)") %>%
       select(Year, Item, `Total midwest`) %>%
       as_tsibble(index = Year)
     
     region_dcmp <- region_cons %>%
       model(
         STL = STL(`Total midwest`)
       ) 
     
     components(region_dcmp) %>%
       as_tsibble(region_dcmp) %>%
       autoplot(`Total midwest`, colour = "gray") +
       geom_line(aes(y=season_adjust), colour = "#0072B2") +
       labs(y = "Total Consumers", title = "Midwest Total Consumers from 2005 - 2019")
   })
   
   # Plot for changes in sales over the years:
   output$regionchange <- renderPlot({
     regional <- readxl::read_excel("Regional_merged_data.xlsx")
     
     region_consumer <- regional %>%
       filter(Item == "Number of consumer units (in thousands)") %>%
       as_tsibble(index = Year)
     
     cons_unit <- region_consumer %>%
       pivot_longer(c(`Total midwest`, `Less than $5,000`, `$5,000 to $9,999`, `$10,000  to $14,999`,`$15,000 to $19,999`, `$20,000 to $29,999`, `$30,000 to $39,999`, `$40,000 to $49,999`, `$50,000 to $69,999`,`$70,000 and more`), names_to = "key", values_to = "value")
     
     cons_unit %>%
       autoplot(.vars = value) +
       facet_grid(vars(key), scales = "free_y")
   })
   
   # Plot for quarter-wise retail sales from 2005 to 2021:
   output$quarter_retail <- renderPlot({
     retail <- readxl::read_excel("retail_data.xlsx")
     
     retail <- retail %>%
       filter(`Kind of Business` == "Retail sales, total") %>%
       as_tsibble(index = Year)
     
     retaildata <- retail %>%
       pivot_longer(c(Jan., Feb., Mar., Apr., May, Jun., Jul., Aug., Sep., Oct., Nov., Dec.), names_to = "key", values_to = "value")
     
     
     retaildata$Month <- paste(retaildata$Year, retaildata$key, sep = " ")
     
     quarter_retail <- retaildata %>%
       mutate(Quarter = yearquarter(Month)) %>%
       as_tsibble(index = Quarter)
     autoplot(quarter_retail, value) +
       labs(y = " Number of Retail Sales", title = "Total Retail Sales from 2005 - 2021")
   })
   
   output$graphDescription <- renderText("There were two datasets 'Regional Data' and 
                                         'Retail Data' which were used for the time series
                                         decomposition and visualization. For the regional 
                                         dataset, the team had chosen to analyze the Midwest region data. 
                                         Therefore, for the time series analysis, the regional portion focused 
                                         on the 'Total Midwest' column for the number of consumer units and 
                                         the retail portion focused on the 'Total Retail Sales' over the time 
                                         period of 2005 - 2021. For the regional dataset, we visualized the total 
                                         consumers in the midwest region against the years from 2005 to 2019 using
                                         a line plot. The inference from the graph is that the data shows an upward trend.
                                         The other plot also shows the change in sales over the years. 
                                         The plot generally shows an increasing trend except for the ranges $50,000 to 69,999
                                         and 70,000 to and more. For the retail data, we can see that the number of retail 
                                         sales went significantly higher after the year 2012. The sales for the month of April 
                                         dropped in 2020.")
   
   shinyAppDir(".")
}

# Run the application 
shinyApp(ui = ui, server = server)

