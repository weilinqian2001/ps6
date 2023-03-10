library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(htmltools)

# load the dataset:
data <- read_delim("UAH.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
  # Giving the theme to the project
  theme = shinytheme("cosmo"),
  title = "UAH Pages",
  
  # Creating the multi-pages website:
  # The home page
  tabPanel("About", 
           
           # Titles and Descriptions 
           h1("About the data"),
           htmlOutput("description1"),
           tags$head(tags$style("#description1{font-size: 18px;}")),
           br(),
           
           htmlOutput("description2"),
           tags$head(tags$style("#description2{font-size: 18px;}")),
           br(),
           
           htmlOutput("description3"),
           tags$head(tags$style("#description3{font-size: 18px;}")),
           br(),
           htmlOutput("description4"),
           tags$head(tags$style("#description4{font-size: 18px;}")),
           
           tableOutput("dataTable")
           ),
          
  # First interactive page with plots
  tabPanel("Plots",
           
           # Create side bar layout
           sidebarLayout(
             # Sidebar panel
             sidebarPanel(
               p("On this page, you can analyze'",
                 strong("global temperature for different regions."),
                 "Select the regions you are interested in. You may see a monthly", strong("scatterplot"),
                 " and the corresponding", strong("trend lines.")),
               br(),
               
               # choosing the country for plot and table 1
               p("This widget can let you choose the", em("region"),
                 "that you are interested in."),
               selectInput("region",
                           "Choose the region:",
                           choices = unique(data$region),
                           multiple = TRUE,
                           selected = "globe"),
               
               # select the year range for plot and table 1
               p("You can operate this widget to choose", 
                 em("year range"),),
               sliderInput("year",
                           "Select the year range",
                           min = min(data$year, na.rm = TRUE),
                           max = max(data$year, na.rm = TRUE),
                           value = c(min(data$year, na.rm = TRUE),
                                     max(data$year, na.rm = TRUE))),
               br(),
               
               # select if you want the trend line for plot
               p("You can choose if you want the", em("trend line"),
                 "on the plot here."),
               radioButtons("type",
                            "Choose the plot type:",
                            choices = c("Scatter points",
                                        "Scatter points with trend line"),
                            selected = "Scatter points")
             ),
             
             # main Panel
             mainPanel(
               # set as tab sets
               tabsetPanel(type = "tabs",
               
                    # plot panel
                    tabPanel("Plot", 
                    br(),
                    textOutput("text"),
                    plotOutput("time")),
               )
             )
           )),
  # 2nd interactive page
  tabPanel("Tables",
           
           # Create side bar layout
           sidebarLayout(
             # Sidebar panel
             sidebarPanel(
               p("This panel displays average temperature over different 
                 time periods: months, years, and decades."),
               br(),
               
               # choosing the service you would like to see on plot and table
               p("By choosing the variables you can see the 
                 comparision between each time period"),
               radioButtons("variable",
                            "Choose the variable:",
                            choices = c("month",
                                        "year"),
                            selected = "year")
             ),
             
             # main Panel
             mainPanel(
               # set as tab sets
               tabsetPanel(type = "tabs",
                           # table
                           tabPanel("Table",
                                    br(),
                                    textOutput("messages"),
                                    tableOutput("tables")),
              )
             )
           )),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # The home page
  # descriptions 
  output$description1 <- renderText("This app uses satellite temperature data from <b>UAH</b>.")
  
  output$description2 <- renderText("Temperature <i> temp</i> is measured as deviation
                                    (deg C) from 1991 - 2020 baseline")
  
  output$description4 <- renderText("The dataset contains 14310 observations and variables,
                                    and here is a little random sample of data:")

  avg_Temp_data <- reactive({
    data %>%
      sample_n(5)
  })
  # data
  random <- reactive({
    avg_Temp_data()
  })
  # table
  output$dataTable <- renderTable({
    random()
  })
  
  # 1st interactive page
  # filter data to react with the year range
  filter_data <- reactive({
    data %>% 
      filter(year >= input$year[1],
             year <= input$year[2])
  })
  
  # set the plot
  # plot text
  output$text <- renderText({
    countries <- paste(input$country, collapse = ", ")
    paste("Here is presenting the Temperature",
          countries, "between",
          input$year[1], "to", 
          input$year[2], ".")
  })
  # plot
  output$time <- renderPlot({
    plots <- ggplot(filter_data() %>% 
                      filter(region %in% input$region),
                    aes(x = year, y = temp,
                        group = region, col = factor(region))) + 
      geom_point() +
      labs(x = "Year", y = "Temperature: deviation from 1991 - 2020 baseline, deg C", 
           main = "Temperature over Time")
    if(input$type == "Scatter points with trend line"){
      plots <- plots +
        geom_smooth(method = "lm", se = FALSE)
    }
    plots
  })
  # 2nd interactive page 
  Data <- reactive({
    data %>%
      rename(Year = colnames(data)[1],
             Month = colnames(data)[2])
  })
  # set the table
  # table message
  output$messages <- renderText({
    paste("You have choose", input$variable, "to show in the table.")
  })
  
  # table
  
  output$tables <- renderTable({
    dataset <- Data()
    if (nrow(dataset) == 0) {
      return("No data available for selected month and year.")
    }
    dataset <- dataset %>%
      na.exclude() %>%
      select(Month, Year, temp)
  })
  
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)