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
transitionMatrix <- matrix(c(0.0032206119, 0.0002800532, 0.0010501995, 0, 0, 0.0000700133, 0.0033606385, 
                             0.0004200798, 0.0035706784, 0.0013302527, 0, 0, 0, 0.0101519289,
                             0.0014702794, 0.0014702794, 0.0064412238, 0, 0, 0, 0.0118322481,
                             0.0044808514, 0.0018203459, 0.0046908913, 0.0251347756, 0.0014702794, 0.0009801862, 0.0562206819,
                             0.0098718757, 0.0051809844, 0.0108520619, 0.0336763985, 0.0627319191, 0.0109920885, 0.1801442274,
                             0.0022404257, 0.0030105720, 0.0039907582, 0.0128124344, 0.0033606385, 0.0087516628, 0.0779248057,
                             0.0100119023, 0.0113421550, 0.0125323812, 0.0200938178, 0.0473990058, 0.3310228943, 0), nrow = 7)



#Creating the markov chain
planChain <- new("markovchain", states = states, transitionMatrix = transitionMatrix)

#Steady State
states <- steadyStates(object = planChain)
states

#Simulate Markov Chain
#Setting seed for reproducibility
set.seed(2025)
modelStates <- rmarkovchain(n = 4, object = planChain, what = "list")












