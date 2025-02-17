#########
# This R script contains code to find IDs of students that 
# purchased a meal plan more than one school year.
########

## load libraries
library(dplyr)
library(stringr)

## read in data
data <- read.csv("data_folder/transformed_data/Combined_Data.csv")

## 'Spring Only XXXX' changed to 'Spring XXXX'
data <- data %>%
  mutate(Term.Session.Description = str_replace(Term.Session.Description, "Spring Only", "Spring")) %>%
  mutate(Term.Session.Description = factor(Term.Session.Description, 
                                           levels = c("Fall 2021", "Spring 2022", "Fall 2022", "Spring 2023", 
                                                      "Fall 2023", "Spring 2024", "Fall 2024", "Spring 2025")))


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
