library(dplyr)
library(ggplot2)
library("ggResidpanel")
library(MASS)
library(ISLR2)
library(glmnet)
library(car)

# read in the data
dining <- read.csv("./data_folder/clean/CurrentDiningData.csv")
regents <- read.csv("./data_folder/clean/CleanRegents.csv")

# make `MealPlan` and `Term` factors, and `Term` numerical from 1-8 for their corresponding factor levels
dining$MealPlan <- factor(dining$Meal.Plan.Description)
dining$Term <- as.numeric(factor(dining$Term.Session.Description, 
                                 levels = c("Fall 2021", "Spring 2022", "Fall 2022", "Spring 2023", 
                                            "Fall 2023", "Spring 2024", "Fall 2024", "Spring 2025")))

# calculate price for each semester (price.year/2)
dining$Price <- dining$Price.Year/2

# make frequency table for MealPlan and Term
freq <- table(dining$MealPlan, dining$Term)
# any 0 counts should be NA
freq[freq==0] <- NA
freq

# turn the frequency table into a data frame
counts <- as.data.frame(freq)
colnames(counts) <- c("MealPlan", "Term", "Frequency")
counts$Term <- as.numeric(counts$Term)
counts

# add price (by semester) to the counts data frame
data <- counts %>%
  left_join(dining, by = c("MealPlan", "Term")) %>%
  dplyr::select(MealPlan, Term, Price, Frequency) 

undergradCounts <- regents %>%
  filter(Student.Classification=="Undergraduate") %>%
  dplyr::select(Year, count) %>%
  slice(rep(1:n(), each=2)) %>%
  mutate(Term = c(1:8))

data.final <- counts %>%
  left_join(dining %>% dplyr::select(MealPlan, Term, Price) %>% unique(), by = c("MealPlan", "Term")) %>%
  dplyr::select(MealPlan, Term, Price, Frequency) %>%
  left_join(undergradCounts, by="Term")

#-------------------------------------------------------------------------------
# Visualize the Data
#-------------------------------------------------------------------------------
g <- ggplot(data.final,
            aes(x     = Term,
                y     = Frequency,
                color = MealPlan)) +
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.1, jitter.height = 0,
      dodge.width  = 0.1)) + 
  geom_smooth()
g + scale_y_log10()

#-------------------------------------------------------------------------------
# Model 1: all variables
#-------------------------------------------------------------------------------
m1 <- lm(
  Frequency ~ Term + MealPlan + Price + count,
  data=data.final
)
summary(m1)

## m1.1 does not contain `count`
m1.1 <- lm(
  Frequency ~ Term + MealPlan + Price,
  data=data.final
)
summary(m1.1)
anova(m1, m1.1) # `count` is not an important predictor

resid_panel(m1,
            plots    = c("resid", "index", "qq", "cookd"),
            qqbands  = TRUE,
            smoother = TRUE)

#-------------------------------------------------------------------------------
# Model 2: no count, test interaction term 
#-------------------------------------------------------------------------------
m2 <- lm(
  Frequency ~ Term + MealPlan + Price + Term*MealPlan,
  data=data.final
)
summary(m2)

m2.2 <- lm(
  Frequency ~ Term + MealPlan + Price,
  data=data.final
)
summary(m2.2)

anova(m2, m2.2) # `Term`*`MealPlan` is significant in the model


resid_panel(m2,
            plots    = c("resid", "index", "qq", "cookd"),
            qqbands  = TRUE,
            smoother = TRUE)
## curvature in residual plot, Q-Q plot is pretty straight (normality)

#-------------------------------------------------------------------------------
# Model 3: log(Frequency) and interaction term
#-------------------------------------------------------------------------------
m3 <- lm(
  log(Frequency) ~ Term + MealPlan + Price + Term*MealPlan,
  data=data.final
)
summary(m3)

resid_panel(m3,
            plots    = c("resid", "index", "qq", "cookd"),
            qqbands  = TRUE,
            smoother = TRUE)
## Q-Q plot is pretty straight, less curvature in residual plot 
## but it does show some groups (error variances aren't equal?)
## variance is dependent on meal plan (generalized least squares?)

resid_xpanel(m3,
            smoother = TRUE)


data.clean <- na.omit(data.final)
predicted <- predict(m3, newdata = data.clean)
all <- data.clean %>%
  mutate(predictValues = predicted,
         expPredict = exp(predicted))

all %>%
  filter(predicted > 7) # predicted values > 7 are Cardinal, Gold & Campanile

all %>%
  filter(predicted > 8) # predicted values > 8 is Cardinal

all %>%
  filter(predicted < 7) # predicted values < 7 are the meal blocks (25, 50, 100)

#-------------------------------------------------------------------------------
# Model 4: Random Forest
#-------------------------------------------------------------------------------

library(randomForest)
library(ISLR2)


rf.dining <- randomForest(Frequency ~ .,
                          data=data.clean, importance=TRUE, ntree=400)
importance(rf.dining)
varImpPlot(rf.dining)
