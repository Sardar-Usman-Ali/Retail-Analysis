####################################
# Muhammad Salman Ali                   
# https://github.com/MSalmanAliKhan
# Data Source: https://www.kaggle.com/datasets/manjeetsingh/retaildataset
####################################

# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(readr)
library(dplyr)
library(xgboost)

# Importing Data sets
stores <- read_csv("E:\\Salman Study Files\\Career Paths\\Data Science\\Portfolio\\Open Source Projects\\Retail Analytics\\stores data-set.csv")
sales <- read_csv("E:\\Salman Study Files\\Career Paths\\Data Science\\Portfolio\\Open Source Projects\\Retail Analytics\\sales data-set.csv")
features <- read_csv("E:\\Salman Study Files\\Career Paths\\Data Science\\Portfolio\\Open Source Projects\\Retail Analytics\\Features data set.csv")

# Combining the files into a single file
Features_Sales <- left_join(
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
Combined_Table <- left_join(
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
  mutate(Total_Markdown = MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5) %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  filter(!is.na(Weekly_Sales))

# User interface
ui <- fluidPage(theme = shinytheme("darkly"),
                navbarPage(theme = "darkly", "Data Model App",
                           tabPanel("Modeling Fuel Price",  #First Tab
                                    headerPanel('Predict Fuel Price'),
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      sliderInput("CPI_fuel", "CPI:",
                                                  min = 126, max = 229,
                                                  value = 150),
                                      sliderInput("temperature_fuel", "Temperature:",
                                                  min = -7.29, max = 102,
                                                  value = 50),
                                      actionButton("submitbutton_fuel_price", "Submit", class = "btn btn-primary")
                                    ),
                                    mainPanel(
                                      tags$label(h3('Model Result')),
                                      verbatimTextOutput('contents_fuel_price'),
                                      tableOutput('tabledata_fuel_price')
                                    )
                           ),
                           tabPanel("Modeling Weekly Sales",
                                    headerPanel('Predict Weekly Sales'),
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      selectInput("is_holiday", label = "Is it a Holiday:",
                                                  choices = list("Yes" = "TRUE", "No" = "FALSE"),
                                                  selected = "Yes"),
                                      sliderInput("fuel_price", "Fuel Price:",
                                                  min = 2.472, max = 4.468,
                                                  value = 3),
                                      sliderInput("CPI", "CPI:",
                                                  min = 126, max = 229,
                                                  value = 150),
                                      sliderInput("temperature", "Temperature:",
                                                  min = -7.29, max = 102,
                                                  value = 50),
                                      sliderInput("unemployment", "Unemployment:",
                                                  min = 3, max = 15,
                                                  value = 9),
                                      actionButton("submitbutton", "Submit", class = "btn btn-primary")
                                    ),
                                    mainPanel(
                                      tags$label(h3('Model Result')),
                                      verbatimTextOutput('contents'),
                                      tableOutput('tabledata')
                                    )
                           ),
                           tabPanel("Modeling Total Markdown",
                                    headerPanel('Predict Total Markdown'),
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      selectInput("is_holiday_markdown", label = "Is it a Holiday:",
                                                  choices = list("Yes" = "TRUE", "No" = "FALSE"),
                                                  selected = "Yes"),
                                      sliderInput("fuel_price_markdown", "Fuel Price:",
                                                  min = 2.472, max = 4.468,
                                                  value = 3),
                                      sliderInput("CPI_markdown", "CPI:",
                                                  min = 126, max = 229,
                                                  value = 150),
                                      sliderInput("temperature_markdown", "Temperature:",
                                                  min = -7.29, max = 102,
                                                  value = 50),
                                      sliderInput("unemployment_markdown", "Unemployment:",
                                                  min = 3, max = 15,
                                                  value = 9),
                                      actionButton("submitbutton_markdown", "Submit", class = "btn btn-primary")
                                    ),
                                    mainPanel(
                                      tags$label(h3('Model Result')),
                                      verbatimTextOutput('contents_markdown'),
                                      tableOutput('tabledata_markdown')
                                    )
                           )
                )
)

# Server
server <- function(input, output, session) {
  
  # Train the XGBoost model for Fuel Price prediction
  xgb_model_fuel_price <- xgboost(
    data = as.matrix(Combined_Table_clean[, c("CPI", "Temperature")]),
    label = Combined_Table_clean$Fuel_Price,
    nrounds = 100,  # Number of boosting rounds (you can adjust this)
    objective = "reg:squarederror"  # Regression task
  )
  
  # Train the XGBoost model for Weekly Sales
  xgb_model <- xgboost(
    data = as.matrix(Combined_Table_clean[, c("IsHoliday", "Fuel_Price", "CPI", "Temperature", "Unemployment")]),
    label = Combined_Table_clean$Weekly_Sales,
    nrounds = 100,  # Number of boosting rounds (you can adjust this)
    objective = "reg:squarederror"  # Regression task
  )
  
  # Train the XGBoost model for Total Markdown prediction
  xgb_model_markdown <- xgboost(
    data = as.matrix(na.omit(Combined_Table_clean)[, c("IsHoliday", "Fuel_Price", "CPI", "Temperature", "Unemployment")]),
    label = na.omit(Combined_Table_clean$Total_Markdown),
    nrounds = 100,  # Number of boosting rounds (you can adjust this)
    objective = "reg:squarederror"  # Regression task
  )
  
  # Input Data for Fuel Price prediction
  datasetInput_fuel_price <- reactive({
    
    # Prepare input data for prediction
    df <- data.frame(
      CPI = input$CPI_fuel,
      Temperature = input$temperature_fuel
    )
    
    # Predict using the trained XGBoost model for Fuel Price
    prediction <- predict(xgb_model_fuel_price, newdata = as.matrix(df))
    
    Output <- data.frame(Prediction = prediction)
    Output
  })
  
  # Input Data for Weekly Sales
  datasetInput <- reactive({
    
    # Prepare input data for prediction
    df <- data.frame(
      IsHoliday = as.logical(input$is_holiday),
      Fuel_Price = input$fuel_price,
      CPI = input$CPI,
      Temperature = input$temperature,
      Unemployment = input$unemployment
    )
    
    # Predict Weekly Sales using the trained XGBoost model
    prediction <- predict(xgb_model, newdata = as.matrix(df))
    
    Output <- data.frame(Prediction = prediction)
    Output
  })
  
    # Input Data for Total_Markdown
    datasetInput_markdown <- reactive({
      
      # Prepare input data for prediction
      df <- data.frame(
        IsHoliday = as.logical(input$is_holiday_markdown),
        Fuel_Price = input$fuel_price_markdown,
        CPI = input$CPI_markdown,
        Temperature = input$temperature_markdown,
        Unemployment = input$unemployment_markdown
      )
      
    
    # Predict Total Markdown using the trained XGBoost model
    prediction <- predict(xgb_model_markdown, newdata = as.matrix(df))
    
    Output <- data.frame(Prediction = prediction)
    Output
  })
  
    
  # Status/Output Text Box for Fuel Price prediction
  output$contents_fuel_price <- renderPrint({
    if (input$submitbutton_fuel_price > 0) {
      isolate("Calculation complete for Fuel Price.")
    } else {
      return("Server is ready for Fuel Price calculation.")
    }
  })
  
  # Status/Output Text Box for Weekly Sales
  output$contents <- renderPrint({
    if (input$submitbutton > 0) {
      isolate("Calculation complete for Weekly Sales.")
    } else {
      return("Server is ready for Weekly Sales Calculaton.")
    }
  })
  
  # Status/Output Text Box for Total Markdown
  output$contents_markdown <- renderPrint({
    if (input$submitbutton_markdown > 0) {
      isolate("Calculation complete for Total Markdown.")
    } else {
      return("Server is ready for Total Markdown calculation.")
    }
  })
  
  # Prediction results table for Fuel Price
  output$tabledata_fuel_price <- renderTable({
    if (input$submitbutton_fuel_price > 0) {
      isolate(datasetInput_fuel_price())
    }
  })
  
  # Prediction results table of Weekly Sales
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) {
      isolate(datasetInput())
    }
  })
  
  # Prediction results table for Total Markdown
  output$tabledata_markdown <- renderTable({
    if (input$submitbutton_markdown > 0) {
      isolate(datasetInput_markdown())
    }
  })
}

# Create the shiny app
shinyApp(ui = ui, server = server)
