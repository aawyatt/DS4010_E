library(markovchain)
library(seqinr)
library(dplyr)

#Calculating transition probabilities between states
plans2 <- read.csv("./data_folder/clean/MealPlanBySemester.csv")
plans2$Spring.2022 <- as.factor(plans2$Spring.2022)
plans2$Fall.2022 <- as.factor(plans2$Fall.2022)

#Data set where double NA isn't removed 
ctoc22 <- plans2[!grepl(", ", plans2$Fall.2022) & !grepl(", ", plans2$Spring.2022),] %>%  count(Spring.2022, Fall.2022)
ctoc23 <- plans2[!grepl(", ", plans2$Fall.2023) & !grepl(", ", plans2$Spring.2023),] %>%  count(Spring.2023, Fall.2023)
ctoc24 <- plans2[!grepl(", ", plans2$Fall.2024) & !grepl(", ", plans2$Spring.2024),] %>% count(Spring.2024, Fall.2024)

#Data set where NA is removed
ctoc22_nona <- plans2 %>% filter(!(is.na(Spring.2022) & is.na(Fall.2022))) %>% count(Spring.2022, Fall.2022)
ctoc23_nona <- plans2 %>% filter(!(is.na(Spring.2023) & is.na(Fall.2023))) %>% count(Spring.2023, Fall.2023)
ctoc24_nona <- plans2 %>% filter(!(is.na(Spring.2024) & is.na(Fall.2024))) %>% count(plans2$Spring.2024, plans2$Fall.2024) 

##Getting proportions for each state

hundred <- ctoc24 %>% filter(Spring.2024 == "100 Meal Blocks")
hundred$Proportions <- hundred$n/sum(hundred$n)

#Only needed if combination doesn't exists in data
hundred <- rbind(hundred, c("100 Meal Blocks", "Cardinal", 0, 0))
hundred <- rbind(hundred, c("100 Meal Blocks", "Campanile", 0, 0))
hundred <- hundred %>% arrange(hundred$Fall.2024)


twenty_five <- ctoc24 %>% filter(Spring.2024 == "25 Meal Blocks")
twenty_five$Proportions <- twenty_five$n/sum(twenty_five$n)

#Only needed if combination doesn't exists in data
twenty_five <- rbind(twenty_five, c("25 Meal Blocks", "Cardinal", 0, 0))
twenty_five <- rbind(twenty_five, c("25 Meal Blocks", "Campanile", 0, 0))
twenty_five <- rbind(twenty_five, c("25 Meal Blocks", "Gold", 0, 0))
twenty_five <- twenty_five %>% arrange(twenty_five$Fall.2024)

fifty <- ctoc24 %>% filter(Spring.2024 == "50 Meal Blocks")
fifty$Proportions <- fifty$n/sum(fifty$n)
#Only needed if combination doesn't exists in data
fifty <- rbind(fifty, c("50 Meal Blocks", "Gold", 0, 0))
fifty <- fifty %>% arrange(fifty$Fall.2024)

camp <- ctoc24 %>% filter(Spring.2024 == "Campanile")
camp$Proportions <- camp$n/sum(camp$n)
camp <- camp %>% arrange(camp$Fall.2024)

card <- ctoc24 %>% filter(Spring.2024 == "Cardinal")
card$Proportions <- card$n/sum(card$n)
card <- card %>% arrange(card$Fall.2024)

gold <- ctoc24 %>% filter(Spring.2024 == "Gold")
gold$Proportions <- gold$n/sum(gold$n)
gold <- gold %>% arrange(gold$Fall.2024)

N_A <- ctoc24 %>% filter(is.na(Spring.2024))
N_A$Proportions <- N_A$n/sum(N_A$n)
N_A <- N_A %>% arrange(N_A$Fall.2024)

used_proportions <- rbind(hundred, twenty_five, fifty, camp, card, gold, N_A)
write.csv(used_proportions, "./data_folder/clean/TransitionMatrix.csv", row.names = FALSE)
 

ctoc22$Proportions <- ctoc22$n/nrow(is.na(plans2$Spring.2022))
ctoc23$Proportions <- ctoc23$n/nrow(plans2)
ctoc24$Proportions <- ctoc24$n/sum(ctoc24$n)

# Proportions after removing NAs (Do not use)
ctoc22_nona$Proportions <- ctoc22_nona$n/sum(ctoc22_nona$n)
ctoc23_nona$Proportions <- ctoc23_nona$n/sum(ctoc23_nona$n)
ctoc24_nona$Proportions <- ctoc24_nona$n/sum(ctoc24_nona$n)

#Transition matrix of probabilities between states
states <- c("100 Meal Blocks", "25 Meal Blocks", "50 Meal Blocks", "Campanile", "Cardinal", "Gold", "NA")
matrix2 <- matrix(as.numeric(used_proportions$Proportions), nrow = 7, byrow = TRUE)


#Creating the markov chain
planChain <- new("markovchain", states = states, transitionMatrix = matrix2)


#Steady State
states <- steadyStates(object = planChain)
states

#Simulate Markov Chain
#Setting seed for reproducibility
#t0 would be where user input would come into play
modelStates <- rmarkovchain(n = 4, object = planChain, t0 = "NA", include.t0 = TRUE)
modelStates[1:4]



#Plot of Simulation
library(ggplot2)


simulated_data <- data.frame(Time = 1:4, State = modelStates[1:4], 4)
                            

ggplot(data = simulated_data, aes(x = Time, y = State, color = State)) +
  geom_point() +
  geom_line() + 
  labs(title = "State Evolution Over Time", x = "Time", y = "State") +
  theme_minimal()







