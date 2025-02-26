#########
# This R script contains code to find IDs of students that 
# purchased a meal plan more than one school year.
########

## load libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)

## read in data
data <- read.csv("./data_folder/transformed_data/Combined_Data.csv")

## 'Spring Only XXXX' changed to 'Spring XXXX'
data <- data %>%
  mutate(Term.Session.Description = str_replace(Term.Session.Description, "Spring Only", "Spring")) %>%
  mutate(Term.Session.Description = factor(Term.Session.Description, 
                                           levels = c("Fall 2021", "Spring 2022", "Fall 2022", "Spring 2023", 
                                                      "Fall 2023", "Spring 2024", "Fall 2024", "Spring 2025")))

## 1 row for each ID, meal plans (if applicable) for each semester
reshaped_data <- data %>%
  select(ID, Term.Session.Description, Meal.Plan.Description) %>%
  arrange(ID, Term.Session.Description) %>%
  pivot_wider(names_from = Term.Session.Description, values_from = Meal.Plan.Description,
              values_fn = function(x) paste(unique(x), collapse = ", "), names_sort=TRUE)

## View the transformed dataset
head(reshaped_data)

## Write tibble to dataframe then to csv
df <- as.data.frame(reshaped_data)
write.csv(df, "./data_folder/clean/MealPlanBySemester.csv", row.names = FALSE)

## 1 row for each ID, room location (if applicable) for each semester
byResidenceHall <- data %>%
  select(ID, Term.Session.Description, Room.Location.Description) %>%
  arrange(ID, Term.Session.Description) %>%
  pivot_wider(names_from = Term.Session.Description, values_from = Room.Location.Description,
              values_fn = function(x) paste(unique(x), collapse = ", "), names_sort=TRUE)

## Write tibble to dataframe then to csv
df2 <- as.data.frame(byResidenceHall)
write.csv(df2, "./data_folder/clean/RoomLocationBySemester.csv", row.names = FALSE)

## group by ID
grouped_data <- data %>% group_by(ID) %>%
  summarize(count = n_distinct(Term.Session.Description))

## IDs that are accounted for a meal plan in ONE semester
one_semesterID <- grouped_data %>% filter(count == 1) %>%
                  pull(ID)

one_semester <- data %>%
                filter(ID %in% one_semesterID) %>%
                select(-Entry.Status.Description) %>%
                arrange(Term.Session.Description)

## IDs that are accounted for a meal plan in TWO semesters
two_semesterID <- grouped_data %>% filter(count == 2) %>%
                  pull(ID)

two_semesters <- data %>%
                  filter(ID %in% two_semesterID) %>%
                  select(-Entry.Status.Description) %>%
                  arrange(ID, Term.Session.Description)

## IDs that are accounted for a meal plan in greater than two semesters
morethan2ID <- grouped_data %>% filter(count > 2) %>%
              pull(ID)

morethan2 <- data %>%
            filter(ID %in% morethan2ID) %>%
            select(-Entry.Status.Description) %>%
            arrange(ID, Term.Session.Description)

counts <- data.frame(
  NumSemesters = c('One', 'Two', 'More than Two'),
  NumRows = c(nrow(one_semester), nrow(two_semesters), nrow(morethan2))
)

ggplot(data=two_semesters, aes(x=Meal.Plan.Description)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data=one_semester, aes(x=Meal.Plan.Description)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))

ggplot(data=morethan2, aes(x=Meal.Plan.Description)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))
