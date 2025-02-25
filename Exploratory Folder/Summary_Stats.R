library(dplyr)
library(ggplot2)
library(tidyr)
library(plyr)

#Pulling in the data
joined_Data <- read.csv("./data_folder/transformed_data/Combined_Data.csv")
meal_plan_price <- read.csv("./data_folder/transformed_data/Clean_Meal_Plan_Prices.csv")
merged <- read.csv("./data_folder/raw/Reddit_Webscraped/MergedDataset.csv")

#Plots the count of each meal plan and splits by term
ggplot(joined_Data, aes(x = Meal.Plan.Description, fill = Term.Session.Description)) + geom_bar()


ggplot(joined_Data, aes(x = Meal.Plan.Description, fill = Room.Location.Description)) + geom_bar()
#Plots yearly meal plan price over time the last 3 years
ggplot(meal_plan_price[, 1:3] %>% filter(grepl('Fall', Term.Session.Description)) %>% drop_na(), 
       aes(x = Term.Session.Description, y = Price.Year, group = Meal.Plan.Description)) + 
  geom_line(aes(colour = Meal.Plan.Description)) + geom_point()

#Formatting data set types
meal_plan_price$Term.Session.Description <- as.factor(meal_plan_price$Term.Session.Description)

#This is price per year as price each semester has been the same for multiple years
tapply(meal_plan_price$Price.Year, meal_plan_price$Term.Session.Description, summary)

convos<- read.csv("./data_folder/raw/Dining_Hall_Webscraped/Convos.csv")
friley<- read.csv("./data_folder/raw/Dining_Hall_Webscraped/Windows.csv")
UDCC<- read.csv("./data_folder/raw/Dining_Hall_Webscraped/Udcc.csv")
seasons<- read.csv("./data_folder/raw/Dining_Hall_Webscraped/SeasonsMarketplace.csv")

survey_data <- rbind(convos, seasons, UDCC, friley)

write.csv(survey_data, "./data_folder/transformed_data/survey_data.csv", row.names = FALSE)