##Name: Jonah Landas
##Date: 2/24/2025
##Purpose: STAT 486 HW 4

library(tidyverse)
data = diamonds

#1.1
q1.1 = diamonds |> rename(Price = price)


#1.2
q1.2 = diamonds
names(q1.2)[1] = "Carat"
names(q1.2)[2] = "Cut"
names(q1.2)[3] = "Color"
names(q1.2)[4] = "Clarity"
names(q1.2)[7] = "Price"

#1.3
q1.3=diamonds
q1.3["Dollar per carat"] = q1.3$price/q1.3$carat

#1.4
q1.4 = diamonds %>% filter(cut == "Ideal")

#1.5
q1.5 = diamonds[,c("x","y","z")]

#1.6
q1.6 = dplyr::select(diamonds,-depth,-table)

#1.7
q1.7 = dplyr::filter(diamonds, 
                    carat > 1.5, 
                    cut %in% c("Good", "Premium", "Ideal"), 
                    color != "D", 
                    price > 10000, 
                    price < 15000)
#1.8
q1.8 = diamonds
q1.8$x_cm = q1.8$x * 0.1  
q1.8$y = q1.8$y * 0.1  
q1.8 = dplyr::filter(q1.8, cut == "Premium", y >= 5)  
q1.8 = q1.8[, c("price", "carat", "x_cm")]  


#1.9
q1.9 = diamonds %>%
  summarize(
    n= n(),
    mean = mean(price),
    sd = sd(price)
  )

#1.10
q1.10 = diamonds %>%
  filter(color =='J', clarity =='IF')%>%
  summarize(
    n = n(),
    mean = mean(carat),
    sd = sd(carat)
  )

#1.11
q1.11 = diamonds  %>%
  group_by(cut, color, clarity) %>%
  summarize(
    n = n(),                     
    mean = mean(price),
    sd = sd(price),
    .groups = "drop"
  )

#1.12
clarity_explanation <- tribble(
  ~clarity, ~explanation,
  "I1", "Included",
  "SI1", "Slightly Included 1",
  "SI2", "Slightly Included 2",
  "VS1", "Very Slightly Included 1",
  "VS2", "Very Slightly Included 2",
  "VVS1", "Very, Very Slightly Included 1",
  "VVS2", "Very, Very Slightly Included 2",
  "IF", "Internally Flawless"
)
q1.12 = diamonds %>%
  inner_join(clarity_explanation)

#1.13
combos <- tribble(
  ~color, ~clarity,
  "E", "SI1",
  "E", "VS2",
  "F", "VVS2",
  "F", "IF",
  "H", "I1"
)

q1.13 = diamonds %>%
  semi_join(combos, by = c("color", "clarity"))

#1.14
q1.14 = diamonds %>%
  anti_join(combos, by = c("color", "clarity"))

#1.15
q1.15 = diamonds %>%
  semi_join(combos, by = c("color", "clarity")) %>%
  group_by(color, clarity) %>%
  summarize(
    n = n(),
    mean = mean(price),
    sd = sd(price),
    .groups = "drop"
  )
