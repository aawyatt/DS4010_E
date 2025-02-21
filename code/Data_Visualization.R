library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(viridis)

#Loading All the datasets
fall_24<-read.csv("./data_folder/transformed_data/Fall_24.csv")
fall_23<-read.csv("./data_folder/transformed_data/Fall_2023.csv")
fall_22<-read.csv("./data_folder/transformed_data/Fall_2022.csv")
fall_21<-read.csv("./data_folder/transformed_data/Fall_2021.csv")

spring_25<-read.csv("./data_folder/transformed_data/Spring_25.csv")
spring_24<-read.csv("./data_folder/transformed_data/Spring_2024.csv")
spring_23<-read.csv("./data_folder/transformed_data/Spring_2023.csv")
spring_22<-read.csv("./data_folder/transformed_data/Spring_2022.csv")

combined_data<-read.csv("./data_folder/transformed_data/Combined_Data.csv")

# Define the correct term order
term_order <- c("Fall 2021", "Spring 2022","Fall 2022", "Spring 2023", "Fall 2023", "Spring 2024", "Fall 2024", "Spring 2025", "Fall 2025")

# Combine all meal plans with "block" or "blocks" into "Block Meal Plan"
combined_data <- combined_data %>%
  mutate(Meal.Plan.Description = case_when(
    grepl("block", Meal.Plan.Description, ignore.case = TRUE) ~ "Block Meal Plan",
    TRUE ~ Meal.Plan.Description
  ))

# Compute the correct percentage by normalizing within each term
meal_plan_trends <- combined_data %>%
  filter(Term.Session.Description %in% term_order) %>%
  group_by(Term.Session.Description) %>%
  mutate(Total_Students = n()) %>%
  group_by(Term.Session.Description, Meal.Plan.Description) %>%
  summarise(Count = n(), Total_Students = first(Total_Students), .groups = 'drop') %>%
  mutate(Percentage = (Count / Total_Students) * 100) %>%
  mutate(Term.Session.Description = factor(Term.Session.Description, levels = term_order))

# Interactive Line Chart - Meal Plan Popularity Trends Over Time
fig1 <- ggplot(meal_plan_trends, aes(x = Term.Session.Description, y = Percentage, color = Meal.Plan.Description, group = Meal.Plan.Description)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Meal Plan Popularity Trends Over Time", x = "Term", y = "Percentage of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(fig1)

# Compute overall meal plan popularity
overall_meal_plan_popularity <- combined_data %>%
  count(Meal.Plan.Description) %>%
  mutate(Percentage = (n / sum(n)) * 100)

# Interactive Bar Chart - Overall Meal Plan Popularity 
fig2 <- ggplot(overall_meal_plan_popularity, aes(x = reorder(Meal.Plan.Description, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(title = "Overall Meal Plan Popularity", x = "Meal Plan", y = "Percentage of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(fig2)


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)  # For proper ordering within facets

# Compute top 5 meal plans per term (Remove Spring Only terms)
top_meal_plans_by_year <- combined_data %>%
  filter(!grepl("Spring Only", Term.Session.Description)) %>%  # Remove Spring Only terms
  group_by(Term.Session.Description, Meal.Plan.Description) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(Term.Session.Description, desc(Count)) %>%
  group_by(Term.Session.Description) %>%
  slice_max(order_by = Count, n = 5) %>%
  ungroup()

# Correctly order Meal Plan Description within each facet
top_meal_plans_by_year <- top_meal_plans_by_year %>%
  mutate(Meal.Plan.Description = fct_reorder2(Meal.Plan.Description, Term.Session.Description, Count))

# Faceted Bar Chart - Top 5 Meal Plans by Year (Ordered within each facet)
fig3 <- ggplot(top_meal_plans_by_year, aes(x = Meal.Plan.Description, y = Count, fill = Meal.Plan.Description)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(~Term.Session.Description, scales = "free_x") +  # Create facets by term
  labs(title = "Top 5 Meal Plans by Year", x = "Meal Plan", y = "Number of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggplotly(fig3)



# Compute student distribution by housing
housing_distribution <- combined_data %>%
  count(Room.Location.Description) %>%
  arrange(desc(n))

# Interactive Bar Chart - Student Distribution by Room Location
fig4 <- ggplot(housing_distribution, aes(x = reorder(Room.Location.Description, -n), y = n)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Student Distribution by Room Location", x = "Room Location", y = "Number of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggplotly(fig4)


## Merging the prices data to the meal plan data:
clean_price_data<-read.csv("./data_folder/transformed_data/Clean_Meal_Plan_Prices.csv")


# Perform Left Join to attach price data
price_data <- combined_data %>%
  left_join(clean_price_data, by = c("Meal.Plan.Description", "Term.Session.Description"))


# Line chart: Price Trends Over Time using clean_price_data
fig3 <- ggplot(clean_price_data, aes(x = Term.Session.Description, y = Price.Year, 
                                     color = Meal.Plan.Description, group = Meal.Plan.Description)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_viridis_d() +
  labs(
    title = "Meal Plan Price Trends Over Time",
    x = "Term",
    y = "Price per Semester ($)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(fig3)

# Count students per meal plan per term
popularity_trends <- price_data %>%
  group_by(Term.Session.Description, Meal.Plan.Description) %>%
  summarise(Total_Students = n(), .groups = 'drop')

# Stacked Area Chart: Meal Plan Popularity
fig_popularity <- ggplot(popularity_trends, aes(x = Term.Session.Description, y = Total_Students, 
                                                fill = Meal.Plan.Description, group = Meal.Plan.Description)) +
  geom_area(alpha = 0.8) +
  scale_fill_viridis_d() +
  labs(
    title = "Most Popular Meal Plans Over Time",
    x = "Term",
    y = "Number of Students"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(fig_popularity)


price_by_plan <- price_data %>%
  filter(!is.na(Price.Year)) %>%
  group_by(Meal.Plan.Description) %>%
  summarise(
    avg_yearly_price = mean(Price.Year, na.rm = TRUE),
    avg_semester_price = mean(Price.Semester, na.rm = TRUE),
    n_students = n()
  ) %>%
  arrange(desc(avg_yearly_price))


library(scales)

# Visualization of price by meal plan
ggplot(price_by_plan, 
       aes(x = reorder(Meal.Plan.Description, avg_yearly_price), 
           y = avg_yearly_price)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = dollar(avg_yearly_price)), hjust = -0.1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Average Yearly Price by Meal Plan",
       x = "Meal Plan",
       y = "Average Yearly Price ($)") +
  scale_y_continuous(labels = dollar_format())


popularity_vs_price <- price_data %>%
  filter(!is.na(Price.Year)) %>%
  group_by(Meal.Plan.Description) %>%
  summarise(
    avg_price = mean(Price.Year, na.rm = TRUE),
    number_of_students = n()
  )

ggplot(popularity_vs_price, 
       aes(x = avg_price, y = number_of_students)) +
  geom_point(aes(size = number_of_students), alpha = 0.6) +
  geom_text(aes(label = Meal.Plan.Description), 
            hjust = -0.1, 
            size = 3) +
  theme_minimal() +
  labs(title = "Meal Plan Popularity vs Price",
       x = "Average Yearly Price ($)",
       y = "Number of Students") +
  scale_x_continuous(labels = dollar_format())

price_summary <- price_data %>%
  filter(!is.na(Price.Year)) %>%
  group_by(Meal.Plan.Description) %>%
  summarise(
    min_price = min(Price.Year),
    max_price = max(Price.Year),
    avg_price = mean(Price.Year),
    median_price = median(Price.Year),
    std_dev = sd(Price.Year),
    n_students = n()
  ) %>%
  arrange(desc(n_students))

# Print summary statistics
print("Price Summary Statistics by Meal Plan:")
print(price_summary, n = nrow(price_summary))

