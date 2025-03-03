# Load the libraries
library(dplyr)
library(ggplot2)
library(plotly)
library(forcats)
library(viridis)
#Read the required data:
clean_data<-read.csv("./data_folder/transformed_data/clean_data.csv")
current_data<-read.csv("./data_folder/transformed_data/current_data.csv")

term_order <- c("Fall 2021", "Spring 2022", "Fall 2022", "Spring 2023", 
                "Fall 2023", "Spring 2024", "Fall 2024", "Spring 2025", "Fall 2025")

# Meal plan trends visualization 
meal_plan_trends <- current_data %>%
  filter(Term.Session.Description %in% term_order) %>%
  group_by(Term.Session.Description) %>%
  mutate(Total_Students = n()) %>%
  group_by(Term.Session.Description, Meal.Plan.Description) %>%
  summarise(Count = n(), Total_Students = first(Total_Students), .groups = 'drop') %>%
  mutate(Percentage = (Count / Total_Students) * 100) %>%
  mutate(Term.Session.Description = factor(Term.Session.Description, levels = term_order))

#  Meal Plan Popularity Trends Over Time (current_data)
fig1 <- ggplot(meal_plan_trends, aes(x = Term.Session.Description, y = Percentage, color = Meal.Plan.Description, group = Meal.Plan.Description)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Meal Plan Popularity Trends Over Time", x = "Term", y = "Percentage of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(fig1)

# Updated Overall Meal Plan Popularity (clean_data)
overall_meal_plan_popularity <- current_data %>%
  count(Meal.Plan.Description) %>%
  mutate(Percentage = (n / sum(n)) * 100)

fig2 <- ggplot(overall_meal_plan_popularity, aes(x = reorder(Meal.Plan.Description, -Percentage), y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  labs(title = "Overall Meal Plan Popularity", x = "Meal Plan", y = "Percentage of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(fig2)

# Update Top 5 Meal Plans per Term using clean_data
top_meal_plans_by_year <- current_data %>%
  filter(!grepl("Spring Only", Term.Session.Description)) %>%
  group_by(Term.Session.Description, Meal.Plan.Description) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(Term.Session.Description, desc(Count)) %>%
  group_by(Term.Session.Description) %>%
  slice_max(order_by = Count, n = 5) %>%
  ungroup() %>%
  mutate(Meal.Plan.Description = fct_reorder2(Meal.Plan.Description, Term.Session.Description, Count))

fig3 <- ggplot(top_meal_plans_by_year, aes(x = Meal.Plan.Description, y = Count, fill = Meal.Plan.Description)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(~Term.Session.Description, scales = "free_x") +
  labs(title = "Top 5 Meal Plans by Year", x = "Meal Plan", y = "Number of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

ggplotly(fig3)

# Updated Housing Distribution by Meal Plan using clean_data
housing_distribution <- current_data %>%
  group_by(Room.Location.Description, Meal.Plan.Description) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  ungroup()

# Compute total count per Room.Location.Description for ordering
room_order <- housing_distribution %>%
  group_by(Room.Location.Description) %>%
  summarise(Total_Count = sum(Count)) %>%
  arrange(desc(Total_Count)) %>%
  pull(Room.Location.Description)

# Convert Room.Location.Description into an ordered factor for proper plotting
housing_distribution$Room.Location.Description <- factor(housing_distribution$Room.Location.Description, levels = room_order)

# Create Interactive Bar Chart - Student Distribution by Room Location, filled by Meal Plan
fig4 <- ggplot(housing_distribution, aes(x = Room.Location.Description, y = Count, fill = Meal.Plan.Description)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
  scale_fill_viridis_d() +  # Use Viridis color palette for better distinction
  labs(
    title = "Student Distribution by Room Location and Meal Plan",
    x = "Room Location",
    y = "Number of Students",
    fill = "Meal Plan"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert to interactive chart
ggplotly(fig4)

# Update Meal Plan Price Trends Over Time using clean_price_data
# Filter out "Spring Only" semesters from current_data
filtered_price_data <- current_data %>%
  filter(!grepl("Spring Only", Term.Session.Description))%>%
  mutate(Term.Session.Description = factor(Term.Session.Description, levels = term_order, ordered = TRUE))


# Update Meal Plan Price Trends Over Time using the filtered data
fig5 <- ggplot(filtered_price_data, aes(x = Term.Session.Description, y = Price.Year, 
                                        color = Meal.Plan.Description, group = Meal.Plan.Description)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_viridis_d() +
  labs(title = "Meal Plan Price Trends Over Time", x = "Term", y = "Price per Semester ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert to interactive plot
ggplotly(fig5)

# Updated Popularity Trends using clean_data
popularity_trends <- filtered_price_data %>%
  group_by(Term.Session.Description, Meal.Plan.Description) %>%
  summarise(Total_Students = n(), .groups = 'drop')
fig_popularity <- ggplot(popularity_trends, aes(x = Term.Session.Description, y = Total_Students, 
                                                fill = Meal.Plan.Description, group = Meal.Plan.Description)) +
  geom_area(alpha = 0.8) +
  scale_fill_viridis_d() +
  labs(title = "Most Popular Meal Plans Over Time", x = "Term", y = "Number of Students") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(fig_popularity)

# Update price summary using clean_data
price_summary <- clean_data %>%
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

print("Price Summary Statistics by Meal Plan:")
print(price_summary, n = nrow(price_summary))
