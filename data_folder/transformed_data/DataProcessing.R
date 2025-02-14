# Load necessary libraries
library(readxl)
library(dplyr)
library(writexl)

# Define file path (update this with your actual file path)
file_path <- "./data_folder/raw/Dining_Data/Meal Plan Data for Student Project.xlsx"

# List of sheet names
sheet_names <- c("Fall 2021", "Spring 2022", "Fall 2022", "Spring 2023", 
                 "Fall 2023", "Spring 2024", "Fall 24", "Spring 25")

output_folder <- "./data_folder/transformed_data/"

terms_to_remove <- c(" Fall 2022"," - Fall 2022"," Fall 2023","Spring 2024"," Fall 2024")
term_pattern <- paste(terms_to_remove, collapse = "|")

for (sheet in sheet_names) {
  # Read the data
  data <- read_excel(file_path, sheet = sheet)
  
  # Rename "Web Description" to "Meal Plan Description" only for "Spring 2023"
  if (sheet == "Spring 2023" && "Web Description" %in% colnames(data)) {
    colnames(data)[colnames(data) == "Web Description"] <- "Meal Plan Description"
  }
  

    # Replace terms only if column exists
    data$`Meal Plan Description` <- gsub(term_pattern, "", data$`Meal Plan Description`)
    
    # Remove extra spaces after term removal
    data$`Meal Plan Description` <- trimws(data$`Meal Plan Description`)

  
  
  # Define output file path
  output_file <- paste0(output_folder, gsub(" ", "_", sheet), ".csv")  # Replacing spaces with underscores
  
  # Save as CSV
  write.csv(data, output_file, row.names = FALSE)
  
  # Print confirmation message
  print(paste("Saved:", output_file))
}

# Load the semester wise csv files

fall_24<-read.csv("./data_folder/transformed_data/Fall_24.csv")
fall_23<-read.csv("./data_folder/transformed_data/Fall_2023.csv")
fall_22<-read.csv("./data_folder/transformed_data/Fall_2022.csv")
fall_21<-read.csv("./data_folder/transformed_data/Fall_2021.csv")

spring_25<-read.csv("./data_folder/transformed_data/Spring_25.csv")
spring_24<-read.csv("./data_folder/transformed_data/Spring_2024.csv")
spring_23<-read.csv("./data_folder/transformed_data/Spring_2023.csv")
spring_22<-read.csv("./data_folder/transformed_data/Spring_2022.csv")


combined_data <- bind_rows(fall_24, fall_23, fall_22, fall_21,
                           spring_25, spring_24, spring_23, spring_22)

# Save as a single CSV file
write.csv(combined_data, "./data_folder/transformed_data/Combined_Data.csv", row.names = FALSE)


