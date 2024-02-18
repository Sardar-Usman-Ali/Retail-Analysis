####################################
# Muhammad Salman Ali                   
# https://github.com/MSalmanAliKhan
# Data Source: https://www.kaggle.com/datasets/manjeetsingh/retaildataset
####################################

library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(lubridate)
library(readr)
library(dplyr)

# Importing Data sets
stores <- read_csv("E:\\Salman Study Files\\Career Paths\\Data Science\\Open Source Projects\\Finance\\Retail Analytics\\stores data-set.csv")
sales <- read_csv("E:\\Salman Study Files\\Career Paths\\Data Science\\Open Source Projects\\Finance\\Retail Analytics\\sales data-set.csv")
features <- read_csv("E:\\Salman Study Files\\Career Paths\\Data Science\\Open Source Projects\\Finance\\Retail Analytics\\Features data set.csv")

# Combining the files into a single file
Features_Sales<-left_join(
  features,
  sales,
  by = NULL,
  copy = FALSE,
  suffix = c(".features", ".sales"),
  keep = NULL,
  na_matches = c("na", "never"),
  multiple = "all",
  unmatched = "drop",
  relationship = NULL
)
Combined_Table<-left_join(
  Features_Sales,
  stores,
  by = "Store",
  copy = FALSE,
  suffix = c(".features", ".sales"),
  keep = NULL,
  na_matches = c("na", "never"),
  multiple = "all",
  unmatched = "drop",
  relationship = NULL
)

Combined_Table_clean <- Combined_Table %>%
  mutate(Total_Markdown = MarkDown1+MarkDown2+MarkDown3+MarkDown4+MarkDown5) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"))%>%
  filter(!is.na(Weekly_Sales))

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("darkly"),
    navbarPage(theme="darkly", "Dashboard App",
        tabPanel("Time-Series Dashboard",
                  # Sidebar layout ----
                 sidebarLayout(
                   sidebarPanel(
                     # Filter by date
                     dateRangeInput("dateRange", "Date Range:", 
                                    start = "2010-01-01", end = "2012-12-31",
                     format = "dd/mm/yyyy",
                     separator = " - ")
                     
                     # Add more filters if needed
                   ),
                   
                   mainPanel(
                     # KPIs
                     fluidRow(
                       column(6, verbatimTextOutput("kpi1")),
                       column(6, verbatimTextOutput("kpi2"))
                     ),
                     
                     # Line Charts
                     fluidRow(
                       column(6, plotOutput("lineChart1", height = "275px")), # Adjust the height
                       column(6, plotOutput("lineChart2", height = "275px")) # Adjust the height
                     ),
                     
                     # Bar Chart and Pie Chart
                     fluidRow(
                       column(6, plotOutput("barChart", height = "275px")), # Adjust the height
                       column(6, plotlyOutput("pieChart", height = "275px")) # Adjust the height
                     )
                   )
                 )
         ),# Navbar 1, tabPanel
        tabPanel("Store Analysis", # Modified tab panel for the second tab
                 # Sidebar layout ----
                 sidebarLayout(
                   sidebarPanel(
                     # Filter by Store
                     selectInput("storeFilter", "Select Store:",
                                 choices = c(unique(Combined_Table_clean$Store),"All"),
                                 selected = "All")
                   ),
                   
                   mainPanel(
                     # KPIs
                     fluidRow(
                       column(6, verbatimTextOutput("kpi3")),
                       column(6, verbatimTextOutput("kpi4"))
                     ),
                     
                     # Line Charts
                     fluidRow(
                       column(6, plotOutput("lineChart3", height = "275px")), # Adjust the height
                       column(6, plotOutput("lineChart4", height = "275px")) # Adjust the height
                     ),
                     
                     # Bar Chart and Pie Chart
                     fluidRow(
                       column(6, plotOutput("barChart2", height = "275px")), # Adjust the height
                       column(6, plotlyOutput("pieChart2", height = "275px")) # Adjust the height
                     )
                   )
                 )
        ),# Modified tab panel for the second tab
        tabPanel("Holiday Analysis", # Modified tab panel for the third tab
                 # Sidebar layout ----
                 sidebarLayout(
                   sidebarPanel(
                     # Filter by IsHoliday
                     selectInput("holidayFilter", "Select IsHoliday:",
                                 choices = c("TRUE", "FALSE", "Both"),
                                 selected = "Both")
                   ),
                   
                   mainPanel(
                     # KPIs
                     fluidRow(
                       column(6, verbatimTextOutput("kpi5")),
                       column(6, verbatimTextOutput("kpi6"))
                     ),
                     
                     # Line Charts
                     fluidRow(
                       column(6, plotOutput("lineChart5", height = "275px")), # Adjust the height
                       column(6, plotOutput("lineChart6", height = "275px")) # Adjust the height
                     ),
                     
                     # Bar Chart and Pie Chart
                     fluidRow(
                       column(6, plotOutput("barChart3", height = "275px")), # Adjust the height
                       column(6, plotlyOutput("pieChart3", height = "275px")) # Adjust the height
                     )
                   )
                 )
        )# Modified tab panel for the third tab
   ) # navbarPage
)# fluidPage

# Define server logic required to create dashboards ----
server <- function(input, output) {
  
  # Filter data based on date range input
  filtered_data <- reactive({
    Combined_Table_clean %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
  })
  
  # Filter data based on Store input (for the second tab)
  filtered_data_store <- reactive({
    if (input$storeFilter == "All") {
      Combined_Table_clean
    } else {
      Combined_Table_clean %>%
        filter(Store == input$storeFilter)
    }
  })
  
  # Filter data based on IsHoliday input (for the third tab)
  filtered_data_holiday <- reactive({
    if (input$holidayFilter == "Both") {
      Combined_Table_clean
    } else {
      Combined_Table_clean %>%
        filter(IsHoliday == input$holidayFilter)
    }
  })
  
  # KPI 1: Total Weekly Sales
  output$kpi1 <- renderText({
    total_Weekly_Sales <- sum(filtered_data()$Weekly_Sales, na.rm = TRUE)
    paste("Total Weekly Sales:", total_Weekly_Sales)
  })
  
  # KPI 2: Gross Markdown
  output$kpi2 <- renderText({
    gross_markdown <- sum(filtered_data()$Total_Markdown, na.rm = TRUE)
    paste("Gross Markdown:", gross_markdown)
  })

  # Line Chart 1
  output$lineChart1 <- renderPlot({
    data <- filtered_data()
    
    # Check the number of data points in the selected range
    num_data_points <- nrow(data)
    
    # Define the maximum number of data points to display (adjust as needed)
    max_data_points <- 100  # You can change this number
    
    if (num_data_points > max_data_points) {
      # Subsample the data to reduce the number of points
      data <- data %>%
        sample_n(max_data_points)
    }
    
    ggplot(data, aes(x = Date, y = Weekly_Sales, color = "Steel Blue")) +
      geom_line() +
      labs(title = "Weekly Sales over Time", x = "Date", y = "Weekly Sales") +
      scale_color_identity() +  # Set color to "Steel Blue"
      theme_minimal() +  # Apply a minimal theme
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) # Remove the legend for color
  })
  
  # Line Chart 2
  output$lineChart2 <- renderPlot({
    data <- na.omit(filtered_data())
    
    # Check the number of data points in the selected range
    num_data_points <- nrow(data)
    
    # Define the maximum number of data points to display (adjust as needed)
    max_data_points <- 100  # You can change this number
    
    if (num_data_points > max_data_points) {
      # Subsample the data to reduce the number of points
      data <- data %>%
        sample_n(max_data_points)
    }
    
    ggplot(data, aes(x = Date, y = Total_Markdown, color = "Steel Blue")) +
      geom_line() +
      labs(title = "Total Markdown over Time", x = "Date", y = "Total Markdown") +
      scale_color_identity() +  # Set color to "Steel Blue"
      theme_minimal() +  # Apply a minimal theme
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) # Remove the legend for color
  })

  # Bar Chart
  output$barChart <- renderPlot({
    ggplot(na.omit(filtered_data()), aes(x = month(Date), y = Fuel_Price, fill = "Steel Blue")) +
      geom_bar(stat = "identity") +
      labs(title = "Fuel Price by Month", x = "Month", y = "Fuel Price") +
      scale_fill_identity() +  # Set fill color to "Steel Blue"
      theme_minimal() + # Apply a minimal theme
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  })
  
  # Pie Chart
  output$pieChart <- renderPlotly({
    pie_data <- filtered_data()%>%
      group_by(Type)%>%
      summarise(value = sum(Weekly_Sales, na.rm = TRUE))
    plot_ly(pie_data, labels = ~Type, values = ~value, type = "pie") %>%
      layout(title = list(
        text = "Weekly Sales by Department",
        font = list(size = 13)  # Set the font size to 20 (adjust as needed))
        )
      )
  })
  
  # KPI 1: Total Weekly Sales (for the second)
  output$kpi3 <- renderText({
    total_Weekly_Sales <- sum(filtered_data_store()$Weekly_Sales, na.rm = TRUE)
    paste("Total Weekly Sales:", total_Weekly_Sales)
  })
  
  # KPI 2: Gross Markdown (for the second)
  output$kpi4 <- renderText({
    gross_markdown <- sum(filtered_data_store()$Total_Markdown, na.rm = TRUE)
    paste("Gross Markdown:", gross_markdown)
  })
  
  # Line Chart 3 (for the second tab)
  output$lineChart3 <- renderPlot({
    data <- filtered_data_store()
    
    # Check the number of data points in the selected range
    num_data_points <- nrow(data)
    
    # Define the maximum number of data points to display (adjust as needed)
    max_data_points <- 100  # You can change this number
    
    if (num_data_points > max_data_points) {
      # Subsample the data to reduce the number of points
      data <- data %>%
        sample_n(max_data_points)
    }
    
    ggplot(data, aes(x = Date, y = Weekly_Sales, color = "Steel Blue")) +
      geom_line() +
      labs(title = "Weekly Sales over Time  by Store", x = "Date", y = "Weekly Sales") +
      scale_color_identity() +  # Set color to "Steel Blue"
      theme_minimal() +  # Apply a minimal theme 
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) # Remove the legend for color
  })
  
  # Line Chart 4 (for the second tab)
  output$lineChart4 <- renderPlot({
    data <- na.omit(filtered_data_store())
    
    # Check the number of data points in the selected range
    num_data_points <- nrow(data)
    
    # Define the maximum number of data points to display (adjust as needed)
    max_data_points <- 100  # You can change this number
    
    if (num_data_points > max_data_points) {
      # Subsample the data to reduce the number of points
      data <- data %>%
        sample_n(max_data_points)
    }
    
    ggplot(data, aes(x = Date, y = Total_Markdown, color = "Steel Blue")) +
      geom_line() +
      labs(title = "Total Markdown over Time by Store", x = "Date", y = "Total Markdown") +
      scale_color_identity() +  # Set color to "Steel Blue"
      theme_minimal() +  # Apply a minimal theme
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) # Remove the legend for color
  })
  
  # Bar Chart (for the second tab)
  output$barChart2 <- renderPlot({
    data <- filtered_data_store()
    
    ggplot(na.omit(data), aes(x = month(Date), y = Fuel_Price, fill = "Steel Blue")) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(title = "Fuel Price by month by Store", x = "Date", y = "Fuel Price") +
      scale_fill_identity() +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  })
  
  # Pie Chart (for the second tab)
  output$pieChart2 <- renderPlotly({
    pie_data <- filtered_data_store() %>%
      group_by(Type) %>%
      summarise(value = sum(Total_Markdown, na.rm = TRUE))
    
    plot_ly(pie_data, labels = ~Type, values = ~value, type = "pie") %>%
      layout(
        title = list(
          text = "Total Markdown by Type",
          font = list(size = 13)  # Set the font size (adjust as needed)
        )
      )
  })
  
  # KPI 1: Total Weekly Sales (for the third)
  output$kpi5 <- renderText({
    total_Weekly_Sales <- sum(filtered_data_holiday()$Weekly_Sales, na.rm = TRUE)
    paste("Total Weekly Sales:", total_Weekly_Sales)
  })
  
  # KPI 2: Gross Markdown (for the second)
  output$kpi6 <- renderText({
    gross_markdown <- sum(filtered_data_holiday()$Total_Markdown, na.rm = TRUE)
    paste("Gross Markdown:", gross_markdown)
  })
  
  # Line Chart 5 (for the third tab)
  output$lineChart5 <- renderPlot({
    data <- filtered_data_holiday()
    
    # Check the number of data points in the selected range
    num_data_points <- nrow(data)
    
    # Define the maximum number of data points to display (adjust as needed)
    max_data_points <- 100  # You can change this number
    
    if (num_data_points > max_data_points) {
      # Subsample the data to reduce the number of points
      data <- data %>%
        sample_n(max_data_points)
    }
    
    ggplot(data, aes(x = Date, y = Weekly_Sales, color = "Steel Blue")) +
      geom_line() +
      labs(title = "Weekly Sales over Time (Filtered by Holiday)", x = "Date", y = "Weekly Sales") +
      scale_color_identity() +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  })
  
  # Line Chart 6 (for the third tab)
  output$lineChart6 <- renderPlot({
    data <- na.omit(filtered_data_holiday())
    
    # Check the number of data points in the selected range
    num_data_points <- nrow(data)
    
    # Define the maximum number of data points to display (adjust as needed)
    max_data_points <- 100  # You can change this number
    
    if (num_data_points > max_data_points) {
      # Subsample the data to reduce the number of points
      data <- data %>%
        sample_n(max_data_points)
    }
    
    ggplot(data, aes(x = Date, y = Total_Markdown, color = "Steel Blue")) +
      geom_line() +
      labs(title = "Total Markdown over Time (Filtered by Holiday)", x = "Date", y = "Total Markdown") +
      scale_color_identity() +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  })
  
  # Bar Chart (for the third tab)
  output$barChart3 <- renderPlot({
    data <-na.omit (filtered_data_holiday())
    
    ggplot(data, aes(x = month(Date), y = Fuel_Price, fill = "Steel Blue")) +
      geom_bar(stat = "identity") +
      labs(title = "Fuel Price by Month by Holiday", x = "Month", y = "Fuel Price") +
      scale_fill_identity() +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  })
  
  # Pie Chart (for the third tab)
  output$pieChart3 <- renderPlotly({
    pie_data <- filtered_data_holiday() %>%
      group_by(Type) %>%
      summarise(value = sum(Weekly_Sales, na.rm = TRUE))
    
    plot_ly(pie_data, labels = ~Type, values = ~value, type = "pie") %>%
      layout(
        title = list(
          text = "Weekly Sales by Department (Filtered by Holiday)",
          font = list(size = 13)  # Set the font size (adjust as needed)
        )
      )
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
