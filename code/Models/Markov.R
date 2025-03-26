library(markovchain)
library(seqinr)
library(dplyr)

#Calculating transition probabilities between states
plans2 <- read.csv("./data_folder/clean/MealPlanBySemester.csv")
plans2$Spring.2022 <- as.factor(plans2$Spring.2022)
plans2$Fall.2022 <- as.factor(plans2$Fall.2022)

#Data set where double NA isn't removed 
ctoc22 <- plans2 %>%  count(Spring.2022, Fall.2022)
ctoc23 <- plans2 %>%  count(Spring.2023, Fall.2023)
ctoc24 <- plans2 %>% count(Spring.2024, Fall.2024)

ctoc22_nona <- plans2 %>% filter(!(is.na(Spring.2022) & is.na(Fall.2022))) %>% count(Spring.2022, Fall.2022)
ctoc23_nona <- plans2 %>% filter(!(is.na(Spring.2023) & is.na(Fall.2023))) %>% count(Spring.2023, Fall.2023)
ctoc24_nona <- plans2 %>% filter(!(is.na(Spring.2024) & is.na(Fall.2024))) %>% count(Spring.2024, Fall.2024) 

ctoc22$Proportions <- ctoc22$n/nrow(plans2)
ctoc23$Proportions <- ctoc23$n/nrow(plans2)
ctoc24$Proportions <- ctoc24$n/nrow(plans2)

ctoc22_nona$Proportions <- ctoc22_nona$n/sum(ctoc22_nona$n)
ctoc23_nona$Proportions <- ctoc23_nona$n/sum(ctoc23_nona$n)
ctoc24_nona$Proportions <- ctoc24_nona$n/sum(ctoc24_nona$n)

#Transition matrix of probabilities between states
states <- c("100 Meal Blocks", "25 Meal Blocks", "50 Meal Blocks", "Campanile", "Cardinal", "Gold", "NA")
transitionMatrix <- matrix(c())


#Creating the markov chain
planChain <- new("markovchain", states = states, transitionMatrix = transitionMatrix)

#Simulate Markov Chain
#Setting seed for reproducability
set.seed(2025)
modelStates <- rmarkovchain(n = 5, object = planChain, t0 = )












