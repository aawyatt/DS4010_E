library(readr)
library(dplyr)
library(ggplot2)

#-------------------------------
# Load & Prepare Meal Plan Data
#-------------------------------
load_meal_data <- function() {
  meal_prices <- read_csv("../../data_folder/transformed_data/Clean_Meal_Plan_Prices.csv")
  combined_data <- read_csv("../../data_folder/clean/CurrentDiningData.csv")
  regents <- read.csv("../../data_folder/clean/CleanRegents.csv")

  
  fall_2024_meal_plans <- unique(combined_data$Meal.Plan.Description)
  
  meal_prices %>%
    filter(Meal.Plan.Description %in% fall_2024_meal_plans,
           grepl("Fall", Term.Session.Description),
           !is.na(Price.Year)) %>%
    mutate(Year = as.numeric(gsub("Fall ", "", Term.Session.Description))) %>%
    dplyr::select(Meal.Plan.Description, Year, Price.Year)
}

run_price_model <- function(selected_plans = NULL, years_ahead = 4) {
  
  data <- load_meal_data()
  # Get the last available year in the data:
  current_max <- max(data$Year, na.rm = TRUE)
  # Create a sequence of future years (for example, if current_max is 2023 and years_ahead is 4, then future_years: 2024,2025,2026,2027)
  future_years <- seq(current_max + 1, current_max + years_ahead)
  
  # For simplicity, use a fixed inflation rate for each future year (this can be adjusted if desired)
  inflation_rate <- rep(0.03, length(future_years))
  
  results <- predict_and_plot_prices(data, future_years, inflation_rate)
  
  # If the user selected specific meal plans, filter the results to only include those:
  if (!is.null(selected_plans)) {
    results$plots <- results$plots[selected_plans]
    results$predictions <- results$predictions[results$predictions$Meal.Plan.Description %in% selected_plans, ]
  }
  
  return(results)
}

predict_and_plot_prices <- function(data, future_years, inflation_rate = 0) {
  future_df <- data.frame(Year = future_years)
  predictions <- data.frame()
  plot_list <- list()
  
  for (plan in unique(data$Meal.Plan.Description)) {
    plan_data <- filter(data, Meal.Plan.Description == plan)
    
    if (nrow(plan_data) > 1) {
      model <- lm(Price.Year ~ Year, data = plan_data)
      raw_preds <- predict(model, newdata = future_df)
      
      last_year <- max(plan_data$Year)
      n_years <- future_years - last_year
      
      # Apply inflation adjustment (if any)
      adjusted_preds <- raw_preds * (1 + inflation_rate)^n_years
      
      r2 <- summary(model)$r.squared
      
      pred_df <- data.frame(
        Meal.Plan.Description = plan,
        Year = future_years,
        Predicted.Price = raw_preds,
        Inflation.Rate = inflation_rate,
        Adjusted.Price = adjusted_preds,
        R2 = r2
      )
      
      predictions <- bind_rows(predictions, pred_df)
      
      # Combine for plotting
      combined_plot_data <- bind_rows(
        mutate(plan_data, Type = "Historical"),
        mutate(pred_df, Price.Year = Adjusted.Price, Type = "Predicted") %>%
          dplyr::select(Meal.Plan.Description, Year, Price.Year, Type)
      )
      
      # Plot
      p <- ggplot() +
        geom_point(data = filter(combined_plot_data, Type == "Historical"),
                   aes(x = Year, y = Price.Year), color = "blue", size = 3) +
        geom_line(data = filter(combined_plot_data, Type == "Historical"),
                  aes(x = Year, y = Price.Year), color = "blue") +
        geom_point(data = filter(combined_plot_data, Type == "Predicted"),
                   aes(x = Year, y = Price.Year), color = "red", size = 3, shape = 17) +
        labs(title = paste("Meal Plan:", plan),
             subtitle = paste("R² =", round(r2, 3), "| Inflation =", paste0(inflation_rate * 100, "%")),
             y = "Yearly Price", x = "Year") +
        theme_minimal()
      
      plot_list[[plan]] <- p
    }
  }
  
  return(list(predictions = predictions, plots = plot_list))
}

#-------------------------------
# Run Prediction with Inflation
#-------------------------------
# Load data
#data <- load_meal_data()

# Set parameters
#future_years <- c(2024,2025,2026,2027)
#inflation_rate <- c(0.02,0.03,0.03,0.01)  # 3% annual inflation

# Get results
#result <- predict_and_plot_prices(data, future_years, inflation_rate)

# View predictions
#print(result$predictions)

# View one plot
#print(result$plots[["Cardinal"]])




