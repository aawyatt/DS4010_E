library(dplyr)

dining <- read.csv("./data_folder/clean/CurrentDiningData.csv")

dining$Meal.Plan.Description <- factor(dining$Meal.Plan.Description)
dining$Term.Session.Description <- factor(dining$Term.Session.Description, 
                                             levels = c("Fall 2021", "Spring 2022", "Fall 2022", "Spring 2023", 
                                                        "Fall 2023", "Spring 2024", "Fall 2024", "Spring 2025"))
dining$Room.Location.Description <- factor(dining$Room.Location.Description)

freq <- table(dining$Meal.Plan.Description, dining$Term.Session.Description)

counts <- as.data.frame(freq)
colnames(counts) <- c("Meal.Plan.Description", "Term.Session.Description", "Frequency")

## there is some overdispersion because mean and variance are not equal, so a negative binomial model
## which is an extension of a poisson model will be used
m <- mean(counts$Frequency)
v <- var(counts$Frequency)
v/m

library(MASS)

pois <- glm.nb(Frequency ~ Meal.Plan.Description + Term.Session.Description, data=counts)
summary(pois)

# Load the pscl package
library(pscl)

# Fit the Zero-Inflated Negative Binomial (ZINB) model
zinb_model <- zeroinfl(Frequency ~ Meal.Plan.Description + Term.Session.Description | Meal.Plan.Description + Term.Session.Description, 
                       data = counts, 
                       dist = "negbin")

# Summary of the model
summary(zinb_model)
