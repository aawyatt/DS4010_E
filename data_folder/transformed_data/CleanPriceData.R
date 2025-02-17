
# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)

# Read the Excel file
file_path<-"./data_folder/raw/Prices_Data/Meal Plan Prices.xlsx"
meal_plan_prices <- read_excel(file_path)

# Function to expand years into Fall and Spring semesters
expand_years <- function(df) {
  df %>%
    mutate(
      Fall_Term = paste0("Fall ", substr(`Year`, 1, 4)),
      Spring_Term = paste0("Spring ", substr(`Year`, 6, 9))
    ) %>%
    select(-`Year`) %>%  # Remove original Year column
    pivot_longer(cols = c(Fall_Term, Spring_Term), names_to = "Semester", values_to = "Term") %>%
    select(`Meal Plan Description`, Term, `Price (Year)`, `Price (Semester)`)
}

# Apply function to expand years
cleaned_meal_plan_prices <- expand_years(meal_plan_prices)

# Ensure proper column formatting
cleaned_meal_plan_prices <- cleaned_meal_plan_prices %>%
  mutate(
    `Price (Year)` = as.integer(`Price (Year)`),
    `Price (Semester)` = as.integer(`Price (Semester)`)
  )
# Define output directory and filename
output_directory <- "./data_folder/transformed_data/"
output_filename <- paste0(output_directory, "Clean_Meal_Plan_Prices.csv")


# Save the cleaned dataset
write.csv(cleaned_meal_plan_prices, output_filename, row.names = FALSE)

# Display first few rows of the cleaned dataset
head(cleaned_meal_plan_prices)

