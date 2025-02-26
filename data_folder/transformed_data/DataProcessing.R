# Load necessary libraries
library(readxl)
library(dplyr)
library(writexl)
# Semester wise Data creation-----------------------------------------------------------------------------------
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
# Combined Transformed data-----------------------------------------------------------------------------------
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
# Standardize Room Location Names
combined_data <- combined_data %>%
  mutate(Room.Location.Description = case_when(
    grepl("^Frederiksen Court", Room.Location.Description) ~ "Frederiksen Court",
    grepl("^University Village", Room.Location.Description) ~ "University Village",
    grepl("^Schilletter Village", Room.Location.Description) ~ "Schilletter Village",
    TRUE ~ Room.Location.Description
  ))


# Remove rows with specific meal plans
combined_data <- combined_data %>%
  filter(!Meal.Plan.Description %in% c("CA Plan", "RA Cardinal", "RA Plan"))


# Save as a single CSV file
write.csv(combined_data, "./data_folder/transformed_data/Combined_Data.csv", row.names = FALSE)
# Final clean data for modelling-----------------------------------------------------------------------------------
# Rename meal plans in combined_data to match clean_price_data
combined_data <- combined_data %>%
  mutate(Meal.Plan.Description = recode(Meal.Plan.Description,
                                        "105 Flex Block" = "105 Meal Blocks",
                                        "25 Flex Block"  = "25 Meal Blocks",
                                        "50 Flex Block"  = "50 Meal Blocks",
                                        "CyFlex"         = "Flex",
                                        "85 Flex Block"  = "85 Meal Blocks"))

# Join the Price.Year column from clean_price_data to combined_data
combined_data <- combined_data %>%
  left_join(clean_price_data %>% select(Meal.Plan.Description, Term.Session.Description, Price.Year),
            by = c("Meal.Plan.Description", "Term.Session.Description"))

# Convert Term.Session.Description to an ordered factor
term_order <- c("Fall 2021", "Spring 2022", "Spring Only 2022", "Fall 2022", 
                "Spring 2023", "Spring Only 2023", "Fall 2023", "Spring 2024", 
                "Spring Only 2024", "Fall 2024", "Spring 2025", "Spring Only 2025", "Fall 2025")

combined_data$Term.Session.Description <- factor(combined_data$Term.Session.Description, 
                                                 levels = term_order, 
                                                 ordered = TRUE)

# Convert Meal.Plan.Description and Room.Location.Description to nominal factors
combined_data$Meal.Plan.Description <- as.factor(combined_data$Meal.Plan.Description)
combined_data$Room.Location.Description <- as.factor(combined_data$Room.Location.Description)

# Remove Entry.Status.Description column
clean_data <- combined_data %>%
  select(-Entry.Status.Description)

# Create a filtered dataset with only meal plans present in Fall 2024
fall_2024_meal_plans <- unique(combined_data$Meal.Plan.Description[combined_data$Term.Session.Description == "Fall 2024"])

current_data <- combined_data %>%
  select(-Entry.Status.Description)%>%
  filter(Meal.Plan.Description %in% fall_2024_meal_plans)
#current_data has NAs for price.year as NAs for spring only semesters


head(combined_data)



write.csv(clean_data, "./data_folder/transformed_data/clean_data.csv", row.names = FALSE)

write.csv(current_data, "./data_folder/transformed_data/current_data.csv", row.names = FALSE)
