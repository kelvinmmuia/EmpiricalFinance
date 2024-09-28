library(shiny)
library(tidyverse)
library(psych)
library(plotly)
library(bslib)
library(kableExtra)
library(knitr)
library(distributional)

# Source the functions from tools.R
#source("tools.R")
source("main_analysis_functions.R")
options(scipen = 999)

# Define UI for the application
ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  title = "Empirical Stock Selection and Portfolio Optimizer",
  
  #Stock Exploratory Analysis
  tabPanel("Stock Exploratory Analysis",
           sidebarLayout(
             sidebarPanel(
               textInput("ticker1", "Enter Ticker Symbol for Stock 1:", value = "AAPL"),
               dateRangeInput("dateRange", "Select Date Range:",
                              start = Sys.Date() - 365 * 2, end = Sys.Date()),
               selectInput("periodicity", "Select Periodicity:",
                           choices = c("daily", "monthly"),
                           selected = "daily"),
               actionButton("analyze", "Analyze Stock")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Raw Data Preview", 
                          uiOutput("rawData1")),
                 tabPanel("Summary Statistics", 
                          uiOutput("summaryStats1")),
                 tabPanel("Correlation Analysis", 
                          uiOutput("correlationMatrix1")),
                 tabPanel("Performance Time Series", 
                          plotlyOutput("performanceTS1")),
                 tabPanel("Returns",
                          uiOutput("returns"),
                          plotOutput("returnsHist1"),
                          plotlyOutput("returnsTS1"))
               )
             )
           )
  ),
  
  #Stock Selection Model Using Regression Analysis
  tabPanel("Best Stock Price Prediction Model Using Regression Analysis",
           sidebarLayout(
             sidebarPanel(
               textInput("ticker_input", "Enter Stock Ticker(s):", value = "AAPL"),
               selectInput("dependent_var", "Select Dependent Variable:", choices = NULL),
               selectInput("independent_vars", "Select Independent Variables:", 
                           choices = NULL, multiple = TRUE),
               actionButton("runForwardSearch", "Run Forward Search")
             ),
             mainPanel(
               verbatimTextOutput("forwardSearchResult")
             )
           )
  ),
  
  #Stock Selection Model Using Negative BIC
  tabPanel("Best Stock Price Prediction Model Using Negative BIC",
           sidebarLayout(
             sidebarPanel(
               textInput("ticker_input_neg_bic", "Enter Stock Ticker(s):", value = "AAPL"),
               selectInput("dependent_var_neg_bic", "Select Dependent Variable:", choices = NULL),
               selectInput("independent_vars_neg_bic", "Select Independent Variables:", 
                           choices = NULL, multiple = TRUE),
               actionButton("run_neg_bic", "Run Negative BIC")
             ),
             mainPanel(
               verbatimTextOutput("neg_bic_results")
             )
           )
  ),
  #Time Series Analysis and Prediction
  tabPanel("Time Series Analysis and Prediction",
           sidebarLayout(
             sidebarPanel(
               textInput("ticker_ts", "Enter Ticker Symbol for Stock:", value = "AAPL"),
               dateRangeInput("dateRange_ts", "Select Date Range:",
                              start = Sys.Date() - 365 * 2, end = Sys.Date()),
               selectInput("periodicity_ts", "Select Periodicity:",
                           choices = c("daily", "monthly"),
                           selected = "daily"),
               numericInput("forecast_horizon", "Enter Forecast Horizon (months):", value = 12),
               actionButton("analyze_ts", "Analyze and Predict")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Adjusted Price Visualization", 
                          plotlyOutput("adjustedPricePlot")),
                 tabPanel("Model Comparisons", 
                          uiOutput("modelComparisons")),
                 tabPanel("Training Data Forecast", 
                          plotlyOutput("trainForecastPlot")),
                 tabPanel("Test Data Forecast", 
                          plotlyOutput("testForecastPlot"),
                          uiOutput("testForecastTable"))
               )
             )
           )
  )
  
)


# Server logic
server <- function(input, output, session) {
  
  # Reactive expression to get stock data for stock 1
  stock_data1 <- reactive({
    req(input$analyze)
    stock_data_list <- getfinpricedata(symnames = input$ticker1, 
                                       from = as.character(input$dateRange[1]), 
                                       to = as.character(input$dateRange[2]), 
                                       src = "yahoo", 
                                       periodicity = input$periodicity) 
    stock_data <- stock_data_list[[1]]
    return(stock_data)
  })
  
  # Output a preview of the raw data in a table for chosen stock
  output$rawData1 <- renderUI({
    req(stock_data1())
    
    # Get the first 10 observations
    raw_data <- head(stock_data1(), 10)  
    
    # Convert to a data frame for better handling
    raw_data_df <- as.data.frame(raw_data)
    
    # Dynamically get the stock ticker from input
    ticker <- input$ticker1
    
    # Calculate the date range for the raw data
    start_date <- as.character(min(index(stock_data1())))
    end_date <- as.character(max(index(stock_data1())))
    
    # Create the table using kable
    raw_data_table <- kable(raw_data_df, "html", 
                            caption = paste("Raw Data for", ticker, "(", start_date, " to ", end_date, ")")) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
    
    # Render the table
    HTML(raw_data_table)
  })
  
  # Summary statistics for the stock 
  output$summaryStats1 <- renderUI({
    req(stock_data1())
    
    # Dynamically get the stock ticker from input
    ticker <- input$ticker1
    
    # Get the summary statistics
    stats <- psych::describe(stock_data1()) %>% round(2)
    
    # Convert to a data frame for better handling
    stats_df <- as.data.frame(stats)
    
    # Calculate the date range for the summary statistics
    start_date <- as.character(min(index(stock_data1())))
    end_date <- as.character(max(index(stock_data1())))  # Fixed closing parenthesis
    
    # Create the table using kable
    summary_table1 <- kable(stats_df, "html", 
                            caption = paste("Summary Statistics for", ticker, "(", start_date, " to ", end_date, ")")) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
    
    # Render the table
    HTML(summary_table1)
  })
  
  # Output correlation matrix for stock 1
  output$correlationMatrix1 <- renderUI({
    req(stock_data1())
    
    # Calculate correlation matrix
    cor_matrix <- cor(stock_data1(), use = "complete.obs")
    
    # Convert correlation matrix to a data frame
    cor_matrix_df <- as.data.frame(cor_matrix)
    
    # Dynamically get the stock ticker from input
    ticker <- input$ticker1
    
    # Calculate the date range for the correlation matrix
    start_date <- as.character(min(index(stock_data1())))
    end_date <- as.character(max(index(stock_data1())))
    
    # Create the correlation table using kable
    cor_table <- kable(cor_matrix_df, "html", 
                       caption = paste("Correlation Matrix for", ticker, "(", start_date, " to ", end_date, ")")) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
    
    # Render the table
    HTML(cor_table)
  })
  
  # Output time series plot for stock 1
  output$performanceTS1 <- renderPlotly({
    req(stock_data1())
    dat <- stock_data1()
    
    # Create the ggplot
    p <- ggplot(dat, aes(x = index(dat), y = dat[, 1])) +
      geom_line(color = "blue") +
      labs(title = paste("Performance Time Series for", input$ticker1),
           x = "Date",
           y = "Adjusted Prices") +
      theme_minimal()
    
    # Convert to plotly
    ggplotly(p)
  })
  
  # Calculate returns for stock
  output$returns <- renderUI({  # Use renderUI to allow for HTML rendering
    req(stock_data1())  # Ensure stock data is available
    dat <- stock_data1()
    
    # Calculate adjusted prices
    adj_prices <- dat[, grep(".Adjusted", colnames(dat))]
    
    # Check if there's enough data to calculate returns
    if (nrow(adj_prices) < 2) {
      return(HTML("Not enough data to calculate returns."))
    }
    
    # Calculate stock returns
    stock_returns <- diff(adj_prices) / lag.xts(adj_prices, k = 1)
    colnames(stock_returns) <- sub(".Adjusted$", ".Ret", colnames(adj_prices))
    stock_returns <- cbind(dat[-1, ], stock_returns)  # Combine with original data
    
    # Dynamically get the stock ticker from input
    ticker <- input$ticker1
    
    # Check if the return column exists
    ret_col <- paste0(ticker, ".Ret")
    if (!ret_col %in% colnames(stock_returns)) {
      return(HTML(paste("No returns data available for ticker:", ticker)))
    }
    
    # Convert stock_returns to a data frame for dplyr
    stock_returns_df <- as.data.frame(stock_returns)
    
    # Use dplyr to get the 5 highest and lowest returns
    highest_returns <- stock_returns_df %>%
      arrange(desc(get(ret_col))) %>%  # Arrange by descending returns
      head(5)                            # Get the top 5
    
    lowest_returns <- stock_returns_df %>%
      arrange(get(ret_col)) %>%         # Arrange by ascending returns
      head(5)                            # Get the bottom 5
    
    # Use kable to format tables
    highest_table <- kable(highest_returns, "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
    
    lowest_table <- kable(lowest_returns, "html") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
    
    # Calculate the date range for the returns
    start_date <- index(stock_returns)[1]
    end_date <- index(stock_returns)[nrow(stock_returns)]
    
    # Render the tables in the UI with dynamic date range in titles
    HTML(paste(
      "<h3>Top Five Highest Returns for ", ticker, " (", start_date, " to ", end_date, ")</h3>", highest_table,
      "<h3>Top Five Lowest Returns for ", ticker, " (", start_date, " to ", end_date, ")</h3>", lowest_table
    ))
  })
  
  
  # Output histogram for stock returns
  output$returnsHist1 <- renderPlot({
    req(stock_data1())
    dat <- stock_data1()
    adj_prices <- dat[, grep(".Adjusted", colnames(dat))]
    stock_returns <- diff(adj_prices) / lag.xts(adj_prices, k = 1)
    stock_returns <- na.omit(stock_returns)  # Remove NA values for histogram
    
    # Convert stock returns to a data frame for ggplot2
    returns_df <- data.frame(Returns = as.numeric(stock_returns))
    
    ggplot(returns_df, aes(x = Returns)) +
      geom_histogram(fill = "lightblue", color = "black") +
      labs(title = paste("Returns Distribution for", input$ticker1),
           x = "Returns", y = "Frequency") +
      theme_minimal()
  })
  
  # Output time series plot for stock returns
  output$returnsTS1 <- renderPlotly({
    req(stock_data1())
    dat <- stock_data1()
    adj_prices <- dat[, grep(".Adjusted", colnames(dat))]
    stock_returns <- diff(adj_prices) / lag.xts(adj_prices, k = 1)
    stock_returns <- na.omit(stock_returns)  # Remove NA values for time series
    
    # Check if stock_returns has data
    if (length(stock_returns) == 0) {
      return(NULL)  # Return NULL if no data to plot
    }
    
    # Create a time series plot using plotly
    p_ts <- plot_ly(x = index(stock_returns), y = coredata(stock_returns),
                    type = 'scatter', mode = 'lines',
                    name = 'Returns', line = list(color = 'blue')) %>%
      layout(title = paste("Time Series of Returns for", input$ticker1),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Returns"))
    
    p_ts  # Return the plot
  })
  
  #helper function to update variable choices
  update_variable_choices <- function(ticker_input, dependent_var_input,
                                      dependent_var_id, independent_var_id) {
    tickers <- strsplit(ticker_input, ",\\s*")[[1]]
    
    # Construct dynamic variable names based on the ticker(s)
    dependent_choices <- paste0(tickers, c(".Adjusted", ".Volume"))
    updateSelectInput(session, dependent_var_id, choices = dependent_choices)
    
    # Construct independent choices
    independent_choices <- c(paste0(tickers, ".Adjusted"),
                             paste0(tickers, ".Volume"),
                             paste0(tickers, ".Open"),
                             paste0(tickers, ".High"),
                             paste0(tickers, ".Low"),
                             paste0(tickers, ".Close"))
    
    # Exclude the selected dependent variable from independent choices
    selected_dependent <- input[[dependent_var_input]]
    independent_choices <- setdiff(independent_choices, selected_dependent)
    
    # Update the select input choices for independent variables
    updateSelectInput(session, independent_var_id, choices = independent_choices)
  }
  # Server logic for Stock Characteristic Regression Analysis
  observeEvent(input$ticker_input, {
    update_variable_choices(input$ticker_input, "dependent_var",
                            "dependent_var", "independent_vars")
  })
  
  observeEvent(input$dependent_var, {
    update_variable_choices(input$ticker_input, "dependent_var", 
                            "dependent_var", "independent_vars")
  })
  
  # Observe the button click to run forward search
  observeEvent(input$runForwardSearch, {
    req(stock_data1())  # Ensure stock data is available
    
    output$forwardSearchResult <- renderPrint({
      yname <- input$dependent_var
      xnames <- input$independent_vars
      req(length(xnames) > 0, "Please select at least one independent variable.")
      
      # Call forwardsearchgr function with reactive inputs
      best_model <- forwardsearchgr(yname = yname, 
                                    xnames2 = xnames, 
                                    data = stock_data1())
      print(best_model)
    })
  })
  
  # Server logic for Stock Selection Model Using Negative BIC
  observeEvent(input$ticker_input_neg_bic, {
    update_variable_choices(input$ticker_input_neg_bic,
                            "dependent_var_neg_bic", 
                            "dependent_var_neg_bic", 
                            "independent_vars_neg_bic")
  })
  
  observeEvent(input$dependent_var_neg_bic, {
    update_variable_choices(input$ticker_input_neg_bic, 
                            "dependent_var_neg_bic",
                            "dependent_var_neg_bic",
                            "independent_vars_neg_bic")
  })
  
  # Observe the button click to run negative BIC analysis
  observeEvent(input$run_neg_bic, {
    req(stock_data1())  # Ensure stock data is available
    
    yname <- input$dependent_var_neg_bic
    xnames <- input$independent_vars_neg_bic
    req(length(xnames) > 0, "Please select at least one independent variable.")
    
    # Call modelscan function with reactive inputs
    best_model_neg_bic <- modelscan(allfrmls = lapply(xnames, function(x) as.formula(paste(yname, "~", x))), 
                                    data = stock_data1())
    
    output$neg_bic_results <- renderPrint({
      print(best_model_neg_bic)
    })
  })
  
  # Reactive function to fetch and process stock data for time series
  stock_data_ts <- reactive({
    req(input$ticker_ts, input$dateRange_ts, input$periodicity_ts)
    
    ticker <- input$ticker_ts
    start_date <- input$dateRange_ts[1]
    end_date <- input$dateRange_ts[2]
    periodicity <- input$periodicity_ts
    
    # Call the function to process the data
    processed_data <- process_stock_data(ticker, start_date, end_date, periodicity)
    
    return(list(train = processed_data$train_tsibble,
                test = processed_data$test_tsibble,
                adjusted_column_name = processed_data$adjusted_column_name))
  })
  
  # Function to fit and compare models (ETS, TSLM, ARIMA, etc.)
  ensemble_models_ts <- reactive({
    req(stock_data_ts())
    
    # Fetch the training data and adjusted column name
    train_tsibble <- stock_data_ts()$train
    test_tsibble <- stock_data_ts()$test  # Ensure this is being accessed
    adjusted_column_name <- stock_data_ts()$adjusted_column_name
    
    # Fit the ensemble models using the dynamically extracted adjusted column name
    fitted_models <- fit_ensemble_models(train_tsibble, adjusted_column_name)
    
    # Return a list with the fitted models and the test data
    return(list(models = fitted_models$models, 
                best_model_name = fitted_models$best_model_name,
                test_tsibble = test_tsibble,  # Make sure to include this
                comparison_table = fitted_models$comparison_table)) 
  })
  
  # Create a reactive value to store the forecast horizon
  forecast_horizon_val <- reactiveVal(0)
  
  # Update the forecast horizon when the button is clicked
  observeEvent(input$analyze_predict_button, {
    forecast_horizon_val(input$forecast_horizon)
  })
  
  # Adjusted Price Visualization
  output$adjustedPricePlot <- renderPlotly({
    req(stock_data_ts())
    
    train_tsibble <- stock_data_ts()$train
    plot <- autoplot(train_tsibble, value = stock_data_ts()$adjusted_column_name) +
      labs(title = "Adjusted Price over Time", x = "Date", y = "Price")
    
    ggplotly(plot)
  })
  
  # Model Comparisons
  output$modelComparisons <- renderUI({
    req(input$ticker_ts, input$dateRange_ts, input$periodicity_ts)
    ticker <- input$ticker_ts
    start_date <- input$dateRange_ts[1]
    end_date <- input$dateRange_ts[2]
    req(ensemble_models_ts())
    
    # Fetch the comparison table from the model fitting function
    comparison_table <- ensemble_models_ts()$comparison_table
    
    # Check for NULL and handle it gracefully
    if (is.null(comparison_table)) {
      return(data.frame(Message = "No comparison data available."))
    }
    
    # Sort by RMSE or any other metric and display
    comparison_table <- comparison_table %>%
      arrange(RMSE)  # Adjust the metric to sort by RMSE, MAPE, or MAE
    
    comparison_table_small <- kable(comparison_table, "html", 
                       caption = paste("Comparison table for models fitted for", ticker, "(", start_date, " to ", end_date, ")")) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
    
    # Render the table
    HTML(comparison_table_small)
  })
  
  # Training Data Forecast
  output$trainForecastPlot <- renderPlotly({
    req(ensemble_models_ts())
    
    best_model_name <- ensemble_models_ts()$best_model_name
    
    # Ensure the best model exists
    req(best_model_name)
    
    best_model <- ensemble_models_ts()$models[[best_model_name]]  # Reference best model
    
    print(best_model_name)  # Debugging information
    print(best_model)       # Debugging information
    
    # Check if the best_model is valid before forecasting
    req(!is.null(best_model))
    
    # Adjust the forecast call based on the model type
    if (grepl("TSLM", best_model_name)) {
      forecasted_train <- best_model %>%
        forecast(new_data = stock_data_ts()$train)
    } else {
      forecasted_train <- best_model %>%
        forecast(h = input$forecast_horizon)
    }
    
    # Visualization of the forecast vs original training data
    plot <- autoplot(forecasted_train, stock_data_ts()$train) +
      labs(title = "Training Data Forecast vs Actual", x = "Date", y = "Price") +
      theme(legend.position = "top")
    
    ggplotly(plot)
  })
  
  
  # Test Data Forecast Plot
  output$testForecastPlot <- renderPlotly({
    req(ensemble_models_ts())
    
    best_model <- ensemble_models_ts()$models[[ensemble_models_ts()$best_model_name]]
    forecasted_test <- best_model %>%
      forecast(stock_data_ts()$test)
    
    # Visualization of the forecast vs actual test data
    plot <- autoplot(forecasted_test, stock_data_ts()$test) +
      labs(title = "Test Data Forecast vs Actual", x = "Date", y = "Price") +
      theme(legend.position = "top")
    
    ggplotly(plot)
  })
  
  # Test Data Forecast Table
  output$testForecastTable <- renderUI({
    req(ensemble_models_ts())
    
    best_model_name <- ensemble_models_ts()$best_model_name
    best_model <- ensemble_models_ts()$models[[best_model_name]]
    
    req(!is.null(best_model))
    
    # Get the adjusted column name from the stock data
    adjusted_column_name <- stock_data_ts()$adjusted_column_name
    
    # Prepare the new data for forecasting
    new_data <- stock_data_ts()$test %>%
      mutate(!!sym(adjusted_column_name) := lag(!!sym(adjusted_column_name), 1),
             lag2 = lag(!!sym(adjusted_column_name), 2)) %>%
      drop_na()  # remove NAs from the data
    
    # Forecast for the test dataset
    test_forecast <- best_model %>%
      forecast(new_data = new_data, h = nrow(stock_data_ts()$test))
    
    forecast_df <- as.data.frame(test_forecast)
    
    # Combine the forecasted values with actual adjusted prices
    actual_prices <- stock_data_ts()$test %>%
      select(Date, !!sym(adjusted_column_name)) %>%
      rename(Actual = !!sym(adjusted_column_name))  # Rename the column for clarity
    
    # Merge forecast results with actual prices
    forecast_df_combined <- forecast_df %>%
      left_join(actual_prices, by = "Date") %>%
      select(.model, Date, Actual, .mean) %>%
      mutate(Best.Model = .model, Date=Date, Adjusted.Price=Actual, Point.Forecast=.mean)%>%
      select(-.model, -Actual, -.mean)%>%
      tail(10)  # Keep only the last 10 rows
    
    # Render the forecast table using kable
    forecast_df_html <- kable(forecast_df_combined, "html", 
                              caption = paste("Test Forecast Results for", best_model_name)) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
    
    # Render the table
    HTML(forecast_df_html)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
