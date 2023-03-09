library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(htmltools)

# load the datasets:
data1 <- read_delim("dataset/Facilities.csv")
data2 <- read_delim("dataset/suicide_all_ages.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
  # Giving the theme to the project
  theme = shinytheme("cosmo"),
  title = "World Suicide Rate Report",
  
  # Creating the multi-pages website:
  # The home page
  tabPanel("Project Overview", 
           # Image
           br(),
           imageOutput("image"),
           
           # Titles and Descriptions 
           h1("World Suicide Rate Report"),
           htmlOutput("description1"),
           tags$head(tags$style("#description1{font-size: 18px;}")),
           br(),
           
           textOutput("title2"),
           tags$head(tags$style("#title2{font-size: 28px;}")),
           htmlOutput("description2"),
           br(),
           
           tags$head(tags$style("#description2{font-size: 18px;}")),
           textOutput("title3"),
           tags$head(tags$style("#title3{font-size: 28px;}")),
           htmlOutput("description3"),
           br(),
           
           tags$head(tags$style("#description3{font-size: 18px;}")),
           textOutput("title4"),
           tags$head(tags$style("#title4{font-size: 28px;}")),
           htmlOutput("description4"),
           br(),
           
           tags$head(tags$style("#description4{font-size: 18px;}")),
           textOutput("title5"),
           tags$head(tags$style("#title5{font-size: 28px;}")),
           htmlOutput("description5"),
           tags$head(tags$style("#description5{font-size: 18px;}"))),
  
  # First interactive page about all_age suicide rates
  tabPanel("Suicide Rates over Time",
           h3("World-wide Suicide Rate over ", strong("Time"), "Analysis"),
           
           # Create side bar layout
           sidebarLayout(
             # Sidebar panel
             sidebarPanel(
               p("On this page, we can see the different countries'",
                 strong("suicide rate"),
                 "list by year. Also, you could learn which country has 
                 the", strong("highest"), "suicide rates in each year."),
               br(),
               
               # choosing the country for plot and table 1
               p("This widget can let you choose the", em("countries"),
                 "that you are interested in."),
               selectInput("country",
                           "Choose the country:",
                           choices = unique(data2$Country),
                           multiple = TRUE,
                           selected = "Afghanistan"),
               
               # select the year range for plot and table 1
               p("You can operate this widget to choose", 
                 em("year range"),),
               sliderInput("year",
                           "Select the year range",
                           min = min(data2$Year, na.rm = TRUE),
                           max = max(data2$Year, na.rm = TRUE),
                           value = c(min(data2$Year, na.rm = TRUE),
                                     max(data2$Year, na.rm = TRUE))),
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
                           
                           # table panel sets
                           navbarMenu("Table", 
                                      # table 1
                                      tabPanel("Average Suicide Rate 
                                               over 2000 - 
                                               2019 for Countries",
                                               br(),
                                               textOutput("texts"),
                                               tableOutput("times")),
                                      # table 2
                                      tabPanel("Highest Suicide Rate 
                                               for Each Year",
                                               br(),
                                               p("This table shows the country 
                                                 that gain the highest average
                                                 suicide rates over the year."),
                                               tableOutput("highest"))
                           ),
                           
                           # summary panel
                           tabPanel("Summary", 
                                    br(),
                                    p("As table showed from previous tab, 
                                      we can learn that:", br(),
                                      strong("Russian Federation"), 
                                      "has the", em("highest"), 
                                      "suicide rate over",
                                      em("2000 - 2001"),";", br(),
                                      strong("Eswatini"), 
                                      "has the", em("highest"), 
                                      "suicide rate over",
                                      em("2002 - 2007"),";", br(),
                                      strong("Lesotho"), 
                                      "has the", em("highest"), 
                                      "suicide rate over",
                                      em("2008 - 2019"), "."),
                                    br(),
                                    p("We can learn from the plot 
                                      by choosing the countries, 
                                      that the most country's suicide rate is",
                                      strong("decreasing"),"."),
                                    br(),
                                    p("As ", strong("Lesotho"), "having a high
                                      average suicide rate over long term,
                                      we choose to see the trend in plot. 
                                      We could see that", em("Lesotho"),
                                      "has a", strong("increasing"), "trend."),
                                    br(),
                                    p(strong("2014"), "has the highest
                                      average suicide rate in the whole 
                                      dataset, which is", em("119.25"),
                                      "in", strong("Lesotho"), ".")))
             )
           )),
  
  # Second interactive page about 15-19yrs suicide rates
  tabPanel("Suicide Rates between Sex",
           h3("World-wide Suicide Rate over ", strong("Sex"), "Analysis"),
           
           # Create side bar layout
           sidebarLayout(
             # Sidebar panel
             sidebarPanel(
               p("You can analyze the suicide rate for", em("different sex"),
                 "on this page. By interact with the bar chart you can see 
                 which sex has the", strong("higher"),
                 "suicide rate in each year"),
               br(), 
               
               # selecting the year for bar chart
               p("Select the", strong ("year"), 
                 "you are interested in. You can see the histogram 
               on this page that show the", em("strong relationship"), 
                 "between Suicide Rate and different sex."),
               uiOutput("selectInput_year")
             ),
             
             # main Panel
             mainPanel(
               textOutput("sex_text1"),
               br(),
               
               # table 1
               textOutput("sex_text2"),
               tableOutput("Table_sex"),
               br(),
               
               # plot
               textOutput("sex_text3"),
               plotOutput("Histo_sex"),
               br(),
               
               # table 2 and summary
               textOutput("sex_text5"),
               br(),
               textOutput("sex_text4"),
               tableOutput("Table_allsex"),
               br()
             )
           )),
  
  # Third interactive page about 
  # the mental health service facilities
  tabPanel("Mental Health Service Availability",
           h3("Mental Health Service Availability by ", strong("Country"),
              "Analysis (Facilities)"),
           
           # Create side bar layout
           sidebarLayout(
             # Sidebar panel
             sidebarPanel(
               p("On this page, you can see the availability of the 
               mental health service facilities by country."),
               br(),
               
               # choosing the service you would like to see on plot and table
               p("By choosing the variables you can see the 
                 comparision between each country."),
               radioButtons("variable",
                            "Choose the variable:",
                            choices = c("Hospitals",
                                        "Mental health unit in hospitals",
                                        "Outpatient",
                                        "Day treatments",
                                        "Community facilities"),
                            selected = "Hospitals")
             ),
             
             # main Panel
             mainPanel(
               # set as tab sets
               tabsetPanel(type = "tabs",
                           # plot
                           tabPanel("Plot", 
                                    br(),
                                    textOutput("message"),
                                    plotOutput("bplot")),
                           # table
                           tabPanel("Table",
                                    br(),
                                    textOutput("messages"),
                                    tableOutput("tables")),
                           # summary
                           tabPanel("Summary",
                                    p("From the plot, 
                                      we can see there still a lot of ",
                                      em("missing values"), 
                                      "on the plot which means there still ",
                                      strong("many"), 
                                      "countries are experiencing",
                                      strong("lack of service"), 
                                      "for mental health"),
                                    p("Comparing plot for different services, 
                                      we could notice that", 
                                      strong("mental health unit in hospitals"),
                                      "was owned more than other services."),
                                    p("The", strong("rarest"),
                                      "service is the", em("day treatments"),
                                      "."),
                                    p("From table, here is the list of the
                                      country with most amout of services:",
                                      br(), "- Hospitals: Japan",
                                      br(), "- Mental health unit in hospitals: 
                                      Ghana",
                                      br(), "- Outpatient: Estonia",
                                      br(), "- Day treatments: Estonia",
                                      br(), "- Community facilities: Greece"),
                                    p("We can see that ", strong("Estonia"),
                                      "was on the top of the data", 
                                      em("twice"), "which are the", 
                                      em("Outpatient and Day treatments"), 
                                      "and also on the top 5 for ", 
                                      em("Mental health unit in hospitals"), 
                                      ".", br(),
                                      "We can know that ", strong("Estonia"),
                                      "is having a good support for 
                                      the mental health for public."),
                                    p("Similarly, we can see that ", 
                                      strong("Japan"),
                                      "was on the top of the data", 
                                      em("Hospitals"), "which also have ranked", 
                                      em("twice"), 
                                      "on the top 5 for ", 
                                      em("Outpatient and Day treatments"), 
                                      ".", br(),
                                      "We can know that ", strong("Japan"),
                                      "is having a nice support for 
                                      the mental health for public too.")
                           ))
             )
           )),
  
  # The conclusion page
  tabPanel("Conclusion",
           h2("Conclusion for the report"),
           
           # table 1
           p("The average suicide rate of countries with the",
             strong("rarest"), "of each mental health service:"),
           tableOutput("conclusion_table1"),
           
           # table 2
           p("The average suicide rate of countries with the ", 
             strong("most"), " of each mental health service:"),
           tableOutput("conclusion_table2"),
           br(),
           
           # Analysis
           h3("Data analysis"),
           p("- From our data, we found that even though the suicide rate is  ", 
             strong("decreased"), " by each year, ", em("different sex"), " 
             has the impact on suicide rate, and the suicide rate also impact 
             some of  ", em("mental health facility services"), ". To be 
             more specific,  ", em("male"), " has  ", strong("higher"), 
             " suicide rate than  ", em("Female"), 
             " which can be seen in the plot in the  ", 
             em("Suicide Rate and Sex"), " tab.
             Also, from the table above, we found that the countries with  ",
             strong("rarest"), " service have  ",  em("lower"), "
             suicide rate, but countries with  ", strong("most"), 
             " of service have  ", em("higher"), " suicide rate because 
             countries with  ", em("high"), " suicide rates have a  ", 
             strong("greater"), " need for mental health hospital, mental health
             units, mental health outpatient facilities and day treatment 
             facilities to care for their populations to  ",
             strong("reduce"), " suicide rates. 
             However, It may be difficult to implement changes that 
             request governments to  ", em("increase"), " support to", 
             strong("reduce"), " suicide rates in countries that 
             lack mental health-related facilities. 
             But through the data we can see that the suicide rate is  ", 
             em("decreasing"), " every year, which is a  ",
             strong("desirable trend.")),
           p("- Although the datasets from World Suicide Report 
             by the World Health Organization are ", 
             strong("reasonable and unbiased"), " , there is ", 
             em("coverage error"), " in the dataset that the dataset 
             of Facilities has many ", strong("missing values"), 
             " , which means there are still many countries are experiencing ", 
             strong("lack of service"), " for mental health or lack of data 
             focus on these countries. This may lead to a continued ", 
             strong("lack of improvement"), " in the construction of ", 
             em("mental health services"), "  in these areas to rapidly ", 
             strong("reduce"), " suicide rates."),
           p("- With this project, we wanted to ", strong("target"), 
             " general population, schools, and governments to ", 
             strong("provide support"), " for ", em("weak"), 
             " mental health people. With additional research and data of 
             the specific country and reason of suicide, 
             the project may be able to showcase to our targets to ", 
             em("give support"), "that will ", 
             strong("improve to decrease"), 
             " suicide rate and people's mental health."))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # The home page
  # image
  output$image <- renderImage({
    list(src = "image/image.jpg",
         width = 900,
         height = 300)
  }, deleteFile = FALSE)
  
  # descriptions 
  output$description1 <- renderText("The report provides a observation of
    suicide rate based on countries by different factors. 
    With the results, we hope to display 
    <b>which group has the highest suicide rate</b> 
    the most in order to encourage better 
    mental health education and support that 
    can lead to decrease in suicide rate and 
    overall mental health growth.")
  
  output$title2 <- renderText("Audience")
  output$description2 <- renderText("We believe that anyone in the 
  global population can benefit from this report, since learning 
  about severe mental health problems, improving self-awareness, 
  and trying to resolve them are important to reduce the suicide rate. 
  The general population can gain the first one by reading our data and 
  analysis, yet the latter two require our target audience, 
    who are <b>schools and governments able to provide education 
                                    and support to weak mental health</b>.")
  
  output$title3 <- renderText("Dataset")
  output$description3 <- renderText({
    paste0("We will be working with the ",
           "<a href = 
             'https://apps.who.int/gho/data/node.main.MENTALHEALTH?lang=en' 
             target = '_blank'>World Suicide Report</a> ",
           "dataset collected and reported 
             by the World Health Organization(WHO).
             The dataset includes data ranges from <i>2000 - 2019</i> 
             depending on the specific dataset, with suicide rates estimate 
             in different age groups from <i>10 - 49 years</i>, 
             mental health service availability and governace, 
             and human resources by country. Most of the data are 
             from <i>2019</i>, as <i>2019</i> was the latest update version. 
             Although the dataset contains multiple suicidal aspects, 
             we narrowed down the dataset and only utilized 
             the following two datasets: <b>Mental Health Service Availbility, 
             and Suicide rate estimates age-standardized</b>.")
  })
  
  output$title4 <- renderText("Questions")
  output$description4 <- renderText({
    HTML("<ul>
          <li>Which country has the highest suicide rate over time?</li>
          <li>How does the suicide rate in different sex?</li>
          <li>Which country has the best mental health service facilities?</li>
        </ul>")
  })
  
  output$title5 <- renderText("Creators")
  output$description5 <- renderText({
    paste("Linda Wang", "Katy Ye", "Maggie Qian", sep="<br/>")
  })
  
  # First interactive page about all_age suicide rates
  # filter data to react with the year range
  filter_data <- reactive({
    data2 %>% 
      filter(Year >= input$year[1],
             Year <= input$year[2])
  })
  
  # set the plot
  # plot text
  output$text <- renderText({
    countries <- paste(input$country, collapse = ", ")
    paste("Here is presenting the Suicise Rate of",
          countries, "between",
          input$year[1], "to", 
          input$year[2], ".")
  })
  # plot
  output$time <- renderPlot({
    plots <- ggplot(filter_data() %>% 
                      filter(Country %in% input$country),
                    aes(x = Year, y = Numeric,
                        group = Country, col = factor(Country))) + 
      geom_point() +
      labs(x = "Year", y = "Suicide Rate (Deaths per 100,000 population)", 
           main = "World-wide Suicide Rate over Time")
    if(input$type == "Scatter points with trend line"){
      plots <- plots +
        geom_smooth(method = "lm", se = FALSE)
    }
    plots
  })
  
  # set the table
  # table text
  output$texts <- renderText({
    countries <- paste(input$country, collapse = ", ")
    paste("Here is presenting the Avereage Suicise Rate of",
          countries, "between 2000 to 2019.")
  })
  # first table
  output$times <- renderTable({
    data2 %>% 
      filter(Country %in% input$country) %>% 
      group_by(Country) %>% 
      summarize(mean =  mean(Numeric), .groups = "drop")
  })
  
  # second table
  output$highest <- renderTable({
    filter_data() %>% 
      select(Year, Country, Numeric) %>% 
      group_by(Year, Country) %>% 
      summarize(mean = mean(Numeric), .groups = "drop") %>% 
      arrange(Year, desc(mean)) %>% 
      group_by(Year) %>% 
      slice(1) %>% 
      arrange(Year)
  })
  
  
  # Second interactive page about suicide rates over sex
  # set the choice for select box
  output$selectInput_year <- renderUI({
    selectInput( "num","Select a year:",
                 choices = c(sort(unique(data2$Year))),
                 selected = "2000")
  })
  
  # text message on the main panel
  output$sex_text1 <- renderText({
    paste("The table on this page show the strong relationship 
          between suicide rate and different sex in each year.")
  })
  output$sex_text2 <- renderText({
    paste("Table of suicide rate with different sex in", input$num)
  })
  
  # filter data for table 1 and plot
  sex_year <- reactive({
    data2 %>%
      filter(Year %in% input$num)%>%
      group_by(Year, Sex)%>%
      summarize(suicide_rate = mean(Numeric), .groups = "drop")
  })
  
  # table
  output$Table_sex <- renderTable({
    sex_year()
  })
  output$sex_text3 <- renderText({
    paste("Histogram of suicide rate with different sex in", input$Year,":")
  })
  
  # plot
  output$Histo_sex <- renderPlot({
    sex_year() %>%
      ggplot(aes(x = Sex, y = suicide_rate, fill = Sex))+
      geom_col()+
      labs(title = "Average suicide rate by Sex", x = "Sex",
           y = "Average suicide rate")+
      scale_y_continuous(limits = c(0,25))
  })
  output$sex_text4 <- renderText({
    paste("Here is the table to summarize 
          the average suicide rate of each sex:")
  })
  
  # filter data for table 2
  avg_sex <- reactive({
    data2%>%
      group_by(Sex)%>%
      summarize(Average_suicide_rate = mean(Numeric))
  })
  
  # table
  output$Table_allsex <- renderTable({
    avg_sex()
  })
  
  # summary 
  output$sex_text5 <- renderText({
    paste("Comparing the table and the histogram, 
    we found that relationship between suicide rate and different sex.")
    paste("Female's suicide rate is lower than Male's suicde rate in every year.
    Female's suicide rate is lower than the average suicide rate of both sexes.
    Male's suicide rate is higher than the average 
          suicide rate of both sexes in every year.")
  })
  
  # Third interactive page about 
  # the mental health service facilities
  # set the plot
  # plot message
  output$message <- renderText({
    paste("You have choose", input$variable, "to show in the plot.")
  })
  # plot panel
  # plot data 
  data <- reactive({
    data1 %>% 
      rename(hospitals = colnames(data1)[3], 
             units = colnames(data1)[4], 
             outpatient = colnames(data1)[5], 
             treatment = colnames(data1)[6], 
             community = colnames(data1)[7])
  })
  # plot
  output$bplot <- renderPlot({
    # set ylab for reducing the repeat code
    ylab <- switch(input$variable,
                   "Mental health unit in hospitals" = 
                     "Mental health units in general hospitals 
                   (per 100,000 population)",
                   "Outpatient" = 
                     "Mental health outpatient facilities 
                   (per 100,000 population)",
                   "Day treatments" = 
                     "Mental health day treatment facilities 
                   (per 100,000 population)",
                   "Community facilities" = 
                     "Community residential facilities 
                   (per 100,000 population)",
                   "Hospitals" = 
                     "Mental Hospitals (per 100,000 population)")
    
    # set data for reducing the repeat code
    plot_data <- switch(input$variable,
                        "Mental health unit in hospitals" = data()$units,
                        "Outpatient" = data()$outpatient,
                        "Day treatments" = data()$treatment,
                        "Community facilities" = data()$community,
                        "Hospitals" = data()$hospitals)
    
    # bar chart
    barplot(plot_data, names.arg = data()$Country, 
            xlab = "Country", ylab = ylab, 
            main = paste0("Global ", input$variable, " status"))
  })
  
  # set the table
  # table message
  output$messages <- renderText({
    paste("You have choose", input$variable, "to show in the table.")
  })
  # table
  output$tables <- renderTable({
    dataset <- data() %>% 
      na.exclude() %>% 
      select(Country, hospitals, units, outpatient, treatment, community) 
    
    # set data for reducing the repeat code
    plot_data <- switch(input$variable,
                        "Mental health unit in hospitals" = dataset$units,
                        "Outpatient" = dataset$outpatient,
                        "Day treatments" = dataset$treatment,
                        "Community facilities" = dataset$community,
                        "Hospitals" = dataset$hospitals)
    
    # print the table
    sorted_data <- dataset[order(plot_data, decreasing = TRUE), ]
    head(sorted_data, 5)
  })
  
  # The conclusion page
  # filter data for avg
  avg_Numeric_data <- reactive({
    data2 %>%
      group_by(Country)%>%
      summarize(avg_suicide_rate = mean(Numeric))
  })
  # data 1
  smallest <- reactive({
    avg_Numeric_data()%>%
      filter(Country %in% c("Tonga","Mexico", "Afghanistan", 
                            "Philippines", "Guatemala"))
  })
  # data 2
  largest <- reactive({
    avg_Numeric_data()%>%
      filter(Country %in% c("Japan", "Ghana", "Estonia",
                            "Estonia", "Greece"))
  })
  # table 1
  output$conclusion_table1 <- renderTable({
    smallest()
  })
  # table 2
  output$conclusion_table2 <- renderTable({
    largest()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)