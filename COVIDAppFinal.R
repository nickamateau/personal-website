library(shiny)
library(ggplot2)
library(dplyr)
library(forecast)
library(readr)
library(lubridate)
library(plotly)

# Load filtered data
filtered_data <- read_csv("filtered_covid_data.csv")

# UI
ui <- fluidPage(
  titlePanel("COVID-19 Time Series Analysis: Selected Countries"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country", 
                  choices = c("United States", "Canada", "United Kingdom")),
      dateRangeInput("dates", "Select Date Range", 
                     start = min(filtered_data$date), 
                     end = max(filtered_data$date)),
      radioButtons("chart", "Choose Secondary Chart", 
                   choices = c("Seasonality", "Autocorrelation")),
      actionButton("calculate", "Calculate Total Cases"),
      br(), br(),
      actionButton("how_it_works", "How the App Works", icon = icon("question-circle")),
      br(), br(),
      selectInput("model_type", "Select Forecast Model",
                  choices = c("Naive", "ETS", "ARIMA", "TSLM"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Full Time Series", 
                 plotOutput("full_series"),
                 h3("Interpretation of the Full Time Series Plot"),
                 uiOutput("country_description")
        ),
        tabPanel("Secondary Chart", 
                 plotOutput("secondary_chart"),
                 conditionalPanel(
                   condition = "input.chart == 'Seasonality'",
                   h3("Seasonality Plot Overview"),
                   p("The Seasonality Plot shows recurring patterns in the data over time. Below is a summary of patterns observed for the selected country:"),
                   uiOutput("seasonality_description")
                 ),
                 conditionalPanel(
                   condition = "input.chart == 'Autocorrelation'",
                   h3("Autocorrelation Function (ACF) Overview"),
                   p("The ACF plot shows the correlation between the time series and its lags. Below is a custom summary for the selected country:"),
                   uiOutput("acf_description")
                 )
        ),
        tabPanel("Decomposition", 
                 plotOutput("decomposition"),
                 h3("Time Series Decomposition Overview"),
                 p("This breaks down the time series into trend, seasonality, and residuals using STL decomposition."),
                 uiOutput("decomp_description")
        ),
        tabPanel("Forecast",
                 plotlyOutput("forecast_plot"),
                 h3("Forecast Interpretation"),
                 textOutput("forecast_interpretation")
        ),
        tabPanel("Recommendation",
                 h3("Recommended Forecast Model"),
                 p("Based on visual inspection and forecast accuracy (e.g., RMSE, AIC), the ETS model appears most suitable for this data due to its ability to handle seasonality and trend effectively. It performs consistently well across various countries and tends to generate smoother, more interpretable forecasts.")
        ),
        tabPanel("Case Calculator", 
                 textOutput("total_cases"),
                 uiOutput("total_cases_ui")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data_country <- reactive({
    filtered_data %>%
      filter(location == input$country, date >= input$dates[1], date <= input$dates[2])
  })
  
  output$full_series <- renderPlot({
    ggplot(filtered_data_country(), aes(x = date, y = new_cases)) +
      geom_line() +
      labs(title = paste("Daily New COVID-19 Cases in", input$country), x = "Date", y = "New Cases")
  })
  
  output$country_description <- renderUI({
    switch(input$country,
           "United States" = tagList(
             p("The time series for the United States highlights several distinct waves of COVID-19."),
             tags$ul(
               tags$li("Multiple large spikes, including the Omicron wave in early 2022."),
               tags$li("Clear downward trend after widespread vaccination."),
               tags$li("Waves likely tied to variant emergence and policy shifts.")
             )
           ),
           "Canada" = tagList(
             p("Canada experienced distinct winter waves and consistent weekly cycles."),
             tags$ul(
               tags$li("Lower case counts early on due to stricter public health measures."),
               tags$li("Significant waves during winter seasons."),
               tags$li("Decline post-Omicron.")
             )
           ),
           "United Kingdom" = tagList(
             p("The UK time series reflects the impact of variant-driven waves."),
             tags$ul(
               tags$li("Clear peaks during Alpha and Omicron."),
               tags$li("Relatively consistent post-2022 decline."),
               tags$li("Highly structured case data.")
             )
           )
    )
  })
  
  output$seasonality_description <- renderUI({
    switch(input$country,
           "United States" = tagList(
             h4("United States Seasonality Insight"),
             tags$ul(
               tags$li("January spikes from winter surges."),
               tags$li("Slight summer/fall upticks."),
               tags$li("Mild but present seasonality.")
             )
           ),
           "Canada" = tagList(
             h4("Canada Seasonality Insight"),
             tags$ul(
               tags$li("Sharp increases in winter months."),
               tags$li("Flattened spring and summer."),
               tags$li("Repetition suggests cold-weather sensitivity.")
             )
           ),
           "United Kingdom" = tagList(
             h4("United Kingdom Seasonality Insight"),
             tags$ul(
               tags$li("Peak in January 2022 aligns with Omicron."),
               tags$li("Regularity across winters."),
               tags$li("Minimal cases in summer.")
             )
           )
    )
  })
  
  output$acf_description <- renderUI({
    switch(input$country,
           "United States" = tagList(
             h4("United States ACF Insight"),
             tags$ul(
               tags$li("Weekly autocorrelation at lag 7 and 14."),
               tags$li("Gradual decay shows typical memory effects."),
               tags$li("Highly structured pattern.")
             )
           ),
           "Canada" = tagList(
             h4("Canada ACF Insight"),
             tags$ul(
               tags$li("Lag 7 consistently strong."),
               tags$li("Pattern fades by lag 21."),
               tags$li("Indicates weekly rhythm.")
             )
           ),
           "United Kingdom" = tagList(
             h4("United Kingdom ACF Insight"),
             tags$ul(
               tags$li("Sharp spikes at 7, 14, 21."),
               tags$li("Highly regular structure."),
               tags$li("Very clean autocorrelation pattern.")
             )
           )
    )
  })
  
  output$decomposition <- renderPlot({
    clean_data <- filtered_data_country() %>% filter(!is.na(new_cases))
    ts_data <- ts(clean_data$new_cases, frequency = 7)
    stl_data <- stl(ts_data, s.window = "periodic")
    plot(stl_data, main = paste("Time Series Decomposition for", input$country))
  })
  
  output$decomp_description <- renderUI({
    switch(input$country,
           "United States" = tagList(
             h4("United States Decomposition Insight"),
             tags$ul(
               tags$li("Strong seasonality and trend peaks."),
               tags$li("Omicron causes large residuals."),
               tags$li("Trend stabilizes in late periods.")
             )
           ),
           "Canada" = tagList(
             h4("Canada Decomposition Insight"),
             tags$ul(
               tags$li("Winter waves dominate trend."),
               tags$li("Seasonality more balanced than US."),
               tags$li("Less volatility in residuals.")
             )
           ),
           "United Kingdom" = tagList(
             h4("United Kingdom Decomposition Insight"),
             tags$ul(
               tags$li("Seasonality is very steady."),
               tags$li("Trend reflects Alpha & Omicron."),
               tags$li("Predictable residual patterns.")
             )
           )
    )
  })
  
  output$secondary_chart <- renderPlot({
    if (input$chart == "Seasonality") {
      seasonal_data <- filtered_data_country() %>%
        mutate(year = year(date), month = month(date, label = TRUE)) %>%
        group_by(year, month) %>%
        summarize(avg_cases = mean(new_cases, na.rm = TRUE), .groups = "drop")
      
      ggplot(seasonal_data, aes(x = month, y = avg_cases, color = factor(year))) +
        geom_line(aes(group = year)) +
        geom_point() +
        labs(title = paste("Monthly Seasonality in", input$country),
             x = "Month", y = "Average New Cases",
             color = "Year") +
        theme_minimal()
    } else {
      ts_data <- ts(filtered_data_country()$new_cases, frequency = 7)
      Acf(ts_data, main = paste("Autocorrelation Plot for", input$country))
    }
  })
  
  output$forecast_plot <- renderPlotly({
    req(input$model_type)
    clean_data <- filtered_data_country() %>% filter(!is.na(new_cases))
    ts_data <- ts(clean_data$new_cases, frequency = 7)
    
    forecast_obj <- switch(input$model_type,
                           "Naive" = naive(ts_data, h = 14),
                           "ETS" = forecast(ets(ts_data), h = 14),
                           "ARIMA" = forecast(auto.arima(ts_data), h = 14),
                           "TSLM" = {
                             time_index <- seq_along(ts_data)
                             df <- data.frame(y = ts_data, t = time_index)
                             fit <- tslm(y ~ t, data = df)
                             forecast(fit, newdata = data.frame(t = max(time_index) + 1:14))
                           }
    )
    
    last_date <- max(filtered_data_country()$date)
    future_dates <- seq.Date(last_date + 1, by = "day", length.out = 14)
    forecast_df <- data.frame(
      date = future_dates,
      forecast = as.numeric(forecast_obj$mean),
      lower = as.numeric(forecast_obj$lower[,2]),
      upper = as.numeric(forecast_obj$upper[,2])
    )
    
    p <- ggplot(forecast_df, aes(x = date, y = forecast)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.5) +
      geom_line(color = "darkblue") +
      geom_point(color = "darkblue") +
      labs(title = paste("14-Day Forecast using", input$model_type),
           x = "Date", y = "Forecasted New Cases") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$forecast_interpretation <- renderText({
    req(input$model_type)
    clean_data <- filtered_data_country() %>% filter(!is.na(new_cases))
    ts_data <- ts(clean_data$new_cases, frequency = 7)
    
    forecast_obj <- switch(input$model_type,
                           "Naive" = naive(ts_data, h = 14),
                           "ETS" = forecast(ets(ts_data), h = 14),
                           "ARIMA" = forecast(auto.arima(ts_data), h = 14),
                           "TSLM" = {
                             time_index <- seq_along(ts_data)
                             df <- data.frame(y = ts_data, t = time_index)
                             fit <- tslm(y ~ t, data = df)
                             forecast(fit, newdata = data.frame(t = max(time_index) + 1:14))
                           }
    )
    
    final_raw <- tail(forecast_obj$mean, 1)
    final_value <- round(final_raw)
    final_date <- as.character(max(filtered_data_country()$date) + 14)
    
    base_explanation <- switch(input$model_type,
                               "Naive" = "The Naive model assumes that the most recent value will continue unchanged.",
                               "ETS" = "The ETS model applies exponential smoothing to adjust for trend and seasonality.",
                               "ARIMA" = "ARIMA considers past values and residual patterns to project the future.",
                               "TSLM" = "TSLM fits a linear trend based on time and projects that forward."
    )
    
    country_note <- switch(input$country,
                           "United States" = switch(input$model_type,
                                                    "Naive" = "Because case counts recently stabilized post-Omicron, the model predicts steady levels.",
                                                    "ETS" = "The model captures the downward trend and strong seasonality after large winter spikes.",
                                                    "ARIMA" = "It incorporates recent drops and prior spikes, balancing short-term trends.",
                                                    "TSLM" = "The steady downward trend is extended linearly, reflecting post-peak decline."
                           ),
                           "Canada" = switch(input$model_type,
                                             "Naive" = "Cases have been flat in recent weeks, leading to a stable short-term forecast.",
                                             "ETS" = "The model reflects winter seasonality, with projected moderation of cases.",
                                             "ARIMA" = "It blends strong weekly autocorrelation with recent drops in case counts.",
                                             "TSLM" = "Linear downward trend leads to a moderate decline in predicted cases."
                           ),
                           "United Kingdom" = switch(input$model_type,
                                                     "Naive" = "The last observed value is used due to relatively steady recent numbers.",
                                                     "ETS" = "ETS captures weekly structure and seasonal lows expected in upcoming weeks.",
                                                     "ARIMA" = "The model detects consistent patterns and predicts stabilization.",
                                                     "TSLM" = "A subtle downward linear trend is projected, matching post-surge behavior."
                           )
    )
    
    warning_note <- if (final_raw < 0) {
      " However, this prediction is negative, which does not make sense for case counts. This result likely indicates the model is over-projecting a downward trend. The value should be interpreted as approximately zero cases."
    } else {
      ""
    }
    
    paste0("On ", final_date, ", the forecasted number of new cases in ", input$country, 
           " is approximately ", final_value, ". ",
           base_explanation, " ", country_note, warning_note)
  })
  
  
  
  total_cases_value <- reactiveVal(NULL)
  observeEvent(input$calculate, {
    total <- sum(filtered_data_country()$new_cases, na.rm = TRUE)
    total_cases_value(total)
  })
  
  output$total_cases_ui <- renderUI({
    if (!is.null(total_cases_value())) {
      h4(paste("Total cases in", input$country, "from", input$dates[1], "to", input$dates[2], ":", total_cases_value()))
    }
  })
  
  observeEvent(input$how_it_works, {
    showModal(modalDialog(
      title = "How the App Works",
      h4("About the Data"),
      p("This app uses COVID-19 case data from the", 
        tags$a(href = "https://ourworldindata.org/covid-cases", "Our World in Data COVID-19 dataset"), "."),
      tags$ul(
        tags$li("United States"),
        tags$li("Canada"),
        tags$li("United Kingdom")
      ),
      h4("How to Use the App"),
      tags$ol(
        tags$li("Select a country."),
        tags$li("Choose a date range."),
        tags$li("Explore charts and forecasts."),
        tags$li("Use calculator for total cases."),
        tags$li("Read model recommendation.")
      ),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

# Run the app
shinyApp(ui, server)
