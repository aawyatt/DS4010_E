library(markovchain)
library(seqinr)
library(dplyr)

#Calculating transition probabilities between states
plans2 <- read.csv("./data_folder/clean/MealPlanBySemester.csv")
plans2$Spring.2022 <- as.factor(plans2$Spring.2022)
plans2$Fall.2022 <- as.factor(plans2$Fall.2022)
ctoc22 <- plans2 %>% count(Spring.2022, Fall.2022)
ctoc23 <- plans2 %>% count(Spring.2022, Fall.2022)
ctoc24 <- plans2 %>% count(Spring.2022, Fall.2022)


#Transition matrix of probabilities between states
states <- c("Cardinal", "Gold", "Campanile", "25MealBlocks", "50Mealblocks", "100MealBlocks", "NA")
transitionMatrix <- matrix()


#Creating the markov chain
planChain <- new("markovchain", states = states, transitionMatrix = transitionMatrix)

#Simulate Markov Chain
#Setting seed for reproducability
set.seed(2025)
modelStates <- rmarkovchain(n = 5, object = planChain, t0 = )












