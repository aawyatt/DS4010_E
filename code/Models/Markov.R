library(markovchain)
library(seqinr)


#Calculating transition probabilities between states

#Transition matrix of probabilities between states
states <- c("Cardinal", "Gold", "Campanile", "25MealBlocks", "50Mealblocks", "100MealBlocks", "NA")
transitionMatrix <- matrix()


#Creating the markov chain
planChain <- new("markovchain", states = states, transitionMatrix = transitionMatrix)

#Simulate Markov Chain
#Setting seed for reproducability
set.seed(2025)
modelStates <- rmarkovchain(n = 5, object = planChain, t0 = )












