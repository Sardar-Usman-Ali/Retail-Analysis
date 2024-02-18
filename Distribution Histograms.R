####################################
# Muhammad Salman Ali                   
# https://github.com/MSalmanAliKhan
# Data Source: https://www.kaggle.com/datasets/manjeetsingh/retaildataset 
####################################

library(shiny)
library(shinythemes)
library(readr)
library(dplyr)

# Importing Data sets
stores <- read_csv("E:\\Salman Study Files\\Career Paths\\Data Science\\Open Source Projects\\Finance\\Retail Analytics\\stores data-set.csv")
sales <- read_csv("E:\\Salman Study Files\\Career Paths\\Data Science\\Open Source Projects\\Finance\\Retail Analytics\\sales data-set.csv")
features <- read_csv("E:\\Salman Study Files\\Career Paths\\Data Science\\Open Source Projects\\Finance\\Retail Analytics\\Features data set.csv")

# Total Markdown Column Creation
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
  filter(!is.na(Weekly_Sales) & !is.na(Total_Markdown))

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("darkly"),
    navbarPage(theme="darkly", "Distribution App",
        tabPanel("Weekly Sale Distribution",
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                      # Input: Slider for the number of bins ----
                      sliderInput(inputId = "bins1",
                                  label = "Number of bins:",
                                  min = 1,
                                  max = 50,
                                  value = 30)
                             ), # sidebarPanel
                  # Main panel for displaying outputs ----
                  mainPanel(
                             h1("Histogram of Weekly Sales", style = "text-align: center;"),
                               # Output: Histogram ----
                               plotOutput(outputId = "WeeklySalesPlot")
                             ) # mainPanel
                           ), # Navbar 1, tabPanel
        tabPanel("Fuel Price Distribution",
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                   # Input: Slider for the number of bins ----
                   sliderInput(inputId = "bins2",
                               label = "Number of bins:",
                               min = 1,
                               max = 50,
                               value = 30)
                 ), # sidebarPanel
                 # Main panel for displaying outputs ----
                 mainPanel(
                   h1("Histogram of Fuel Price", style = "text-align: center;"),
                   # Output: Histogram ----
                   plotOutput(outputId = "FuelPricePlot")
                 ) # mainPanel
        ), # Navbar 1, tabPanel
        tabPanel("Total Markdown Distribution",
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                   # Input: Slider for the number of bins ----
                   sliderInput(inputId = "bins3",
                               label = "Number of bins:",
                               min = 1,
                               max = 50,
                               value = 30)
                 ), # sidebarPanel
                 # Main panel for displaying outputs ----
                 mainPanel(
                   h1("Histogram of Total Markdown", style = "text-align: center;"),
                   # Output: Histogram ----
                   plotOutput(outputId = "TotalMarkdownPlot")
                 ) # mainPanel
        ), # Navbar 1, tabPanel
   ) # navbarPage
)# fluidPage

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$WeeklySalesPlot <- renderPlot({
    
    x    <- sales$Weekly_Sales
    x    <- na.omit(x)
    bins1 <- seq(min(x), max(x), length.out = input$bins1 + 1)
    
    hist(x, breaks = bins1, col = "#75AADB", border = "black",
         xlab = "Weekly Sales",
         main = "Histogram of Weekly Sales")
    
  })
  output$FuelPricePlot <- renderPlot({
    
    x    <- features$Fuel_Price
    x    <- na.omit(x)
    bins2 <- seq(min(x), max(x), length.out = input$bins2 + 1)
    
    hist(x, breaks = bins2, col = "#75AADB", border = "black",
         xlab = "Fuel Price",
         main = "Histogram of Fuel Price")
    
  })
  output$TotalMarkdownPlot <- renderPlot({
    
    x    <- Combined_Table_clean$Total_Markdown
    x    <- na.omit(x)
    bins3 <- seq(min(x), max(x), length.out = input$bins3 + 1)
    
    hist(x, breaks = bins3, col = "#75AADB", border = "black",
         xlab = "Total Markdown",
         main = "Histogram of Total Markdown",)
    
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
