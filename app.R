#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

####Imports
library(shiny)
library(googledrive)
library(googlesheets4)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)

##### DATA INGESTION/CLEANING
#read data
covid.data <- read_sheet("https://docs.google.com/spreadsheets/d/1gK8ZFq5h66SqXX4P2Kjltfmu81pe0BSpSmSf1KBjcWY/edit#gid=0", "Data & Notes")
#print(covid.data)
View(covid.data)
#Clean sheet
names(covid.data)[2] <- "date"
#fix column names
for (x in 1:ncol(covid.data)) { 
  names(covid.data)[x] <- gsub(" ", "_", names(covid.data)[x])
  names(covid.data)[x] <- gsub(":", "_", names(covid.data)[x])
  print(names(covid.data)[x]) 
  }


#######  DATA MUNGING DFs FOR VIZ  #########
#Format data for case count over time plot
total.cases.df <- covid.data %>%
                    select(date, Cases_Outside_China_less__Princess_Diamond, Suspected_China_Cases, Princess_Diamond_Cases) %>%
                    gather(key = "variable", value = "value", -date)
#View(total.cases.df)

#Format data for daily growth rates
growth.rates.df <- covid.data %>%
                select(date, China_Growth_Rate, Outside_China_Growth_Rate) %>%
                gather(key = "variable", value = "value", -date)


#############  UI/UX CODE  ############
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID19 Tracker"),
    
    fluidRow(
      
      column(12, 
             h5("Desription Text"),
             helpText("Lorem IpsumLorem IpsumLorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum Lorem Ipsum")),
      
      column(6, plotOutput("totalcases")),
      
      column(6, plotOutput("dailygrowthrates")),
      
      
    )

    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel("test"
    #                  
    #     ),

        # Show a plot of the generated distribution
        # mainPanel(
        #    #1plotOutput("distPlot")
        #    # plotOutput("totalcases")
        #    # plotOutput("dailygrowthrates")
        # )
   # )
)

######## Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$totalcases <- renderPlot({
      # ggplot(data=covid.data, aes(x=date, y=Cases_Outside_China, group=1)) +
      #   geom_line()+
      #   geom_point()
      p1 <- ggplot(total.cases.df, aes(x = date, y = value)) + 
        geom_line(aes(color = variable), size = 1) +
        theme_minimal()
      p1 + ggtitle("Case Counts Over Time") + xlab("Date") + ylab("Number of Cases")
      
    })
    
    output$dailygrowthrates <- renderPlot({
      
    p2 <-ggplot(growth.rates.df, aes(x = date, y = value)) + 
      geom_line(aes(color = variable), size = 1) +
      theme_minimal()
    p2 + ggtitle("Daily Growth Rates") + xlab("Date") + ylab("Growth Rate") 
    #ggplotly(p2, tooltip = c("value"))
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
