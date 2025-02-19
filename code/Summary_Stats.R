library(dplyr)
library(ggplot2)

joined_Data <- read.csv("./data_folder/transformed_data/Combined_Data.csv")

ggplot(joined_Data, aes(x = Meal.Plan.Description, fill = Term.Session.Description)) + geom_bar()
       