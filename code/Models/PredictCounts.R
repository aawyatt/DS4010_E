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
# calculate price for each semester (price.year/2)
dining <- dining %>%
  mutate(MealPlan = factor(dining$Meal.Plan.Description),
         Term = as.numeric(factor(dining$Term.Session.Description, 
                                  levels = c("Fall 2021", "Spring 2022", "Fall 2022", "Spring 2023", 
                                             "Fall 2023", "Spring 2024", "Fall 2024", "Spring 2025"))),
         Price = Price.Year/2
         )

# make frequency table for MealPlan and Term

counts <- as.data.frame(table(dining$MealPlan, dining$Term)) %>%
  rename(MealPlan = Var1, Term = Var2, MealPlanCount = Freq) %>%
  mutate(Term = as.numeric(Term), 
         MealPlanCount = ifelse(MealPlanCount==0, NA, MealPlanCount))

undergradCounts <- regents %>%
  filter(Student.Classification=="Undergraduate") %>%
  dplyr::select(Year, count) %>%
  slice(rep(1:n(), each=2)) %>%
  mutate(Term = c(1:8),
         UndergradCount=count) %>%
  dplyr::select(-count)

data.final <- counts %>%
  left_join(dining %>% dplyr::select(MealPlan, Term, Price) %>% distinct(), by = c("MealPlan", "Term")) %>%
  dplyr::select(MealPlan, Term, Price, MealPlanCount) %>%
  left_join(undergradCounts, by="Term") %>%
  mutate(Semester = ifelse(Term %% 2 == 0, "Spring", "Fall")) %>%
  dplyr::select(MealPlan, Term, Semester, Year, MealPlanCount, UndergradCount)
  
  
#-------------------------------------------------------------------------------
# Visualize the Data
#-------------------------------------------------------------------------------
g <- ggplot(data.final,
            aes(x = Year,
                y = MealPlanCount,
                color = MealPlan,
                shape = Semester,
                group = interaction(MealPlan, Semester))) + 
  geom_point(position = position_jitterdodge(
    jitter.width = 0.1, jitter.height = 0,
    dodge.width = 0.1)) +
  geom_line() + 
  scale_y_log10()

g

#-------------------------------------------------------------------------------
# Model 1: all variables
#-------------------------------------------------------------------------------
m1 <- lm(
  MealPlanCount ~ .,
  data=data.final
)
summary(m1)

resid_panel(m1,
            plots    = c("resid", "index", "qq", "cookd"),
            qqbands  = TRUE,
            smoother = TRUE)

## diagnostic plots do not look good, take log of response variable

#-------------------------------------------------------------------------------
# Model 2 and 3: log(MealPlanCount) and interaction term
#-------------------------------------------------------------------------------
m2 <- lm(
  log(MealPlanCount) ~ .,
  data=data.final
)
summary(m2)

resid_panel(m2,
            plots    = c("resid", "index", "qq", "cookd"),
            qqbands  = TRUE,
            smoother = TRUE)
# diagnostics look somewhat better

m3 <- lm(
  log(MealPlanCount) ~ . + Term*MealPlan,
  data=data.final
)
summary(m3)

## test significance of interaction term
anova(m2, m3) ## high p-value, interaction term is not significant

resid_xpanel(m2,
            smoother = TRUE)

confint(m2)

data.clean <- na.omit(data.final)
predicted <- predict(m2, newdata = data.clean)
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
# Model 2: Model fit and model selection
#-------------------------------------------------------------------------------

# MSE and RMSE
mse1 <- mean((log(data.clean$MealPlanCount) - predict(m2, data.clean))^2) ## MSE
sqrt(mse1) ## RMSE = average difference between actual and predicted in LOG UNITS
## on average, the model's predictions are off by 0.3 units on the log scale of MealPlanCounts

exp(sqrt(mse1)) 
## 1.35, the model predictions are off by about 35%

m2_step <- stepAIC(m2, direction = "both")
summary(m2_step) ## the model chose MealPlan, Term, Semester, and Year increase the AIC
## when they're removed from the model, meaning they have a strong influence on the counts

lm_final <- lm(log(MealPlanCount) ~ MealPlan + Semester + UndergradCount, data=data.final)
summary(lm_final)

resid_panel(lm_final)
resid_xpanel(lm_final)

# MSE and RMSE
mse2 <- mean((log(data.clean$MealPlanCount) - predict(lm_final, data.clean))^2) ## MSE
sqrt(mse2) ## RMSE = average difference between actual and predicted in LOG UNITS
## on average, the model's predictions are off by 0.31 units on the log scale of MealPlanCounts

exp(sqrt(mse2)) ## 1.35, the model predictions are off by about 35% 

#-------------------------------------------------------------------------------
# Model 2: Cross Validation (k-fold)
#-------------------------------------------------------------------------------

## Using cross validation to train the model and test on data it hasn't seen

library(caret)

k <- 10 # number of folds/times to rerun the code

folds <- sample(1:k,nrow(data.final),replace=TRUE)
names(folds)

for(i in 1:k){
  test_index = folds[[i]]
  
  test = data.final[test_index,]
  train = data.final[-test_index,]
  
  M1 <- lm(log(MealPlanCount) ~ MealPlan + Semester + UndergradCount, data=data.final)
  M1_count <- predict(M1,newdata=test[,-1])
  
}

#-------------------------------------------------------------------------------
# Model 4: Poisson
#-------------------------------------------------------------------------------

m.pois1 <- glm(MealPlanCount ~ MealPlan + Term + Semester + Year + offset(log(UndergradCount)),
               data=data.final,
               family=poisson(link="log"))
m.pois2 <- glm(MealPlanCount ~ MealPlan + Term + Semester + Year + log(UndergradCount),
               data=data.final,
               family=poisson(link="log"))
summary(m.pois1)
summary(m.pois2)
confint(m.pois1)
confint(m.pois2) ## higher # of students means lower counts of meal plans?

summary <- data.final %>%
  group_by(Year) %>%
  summarize(freqSum = sum(Frequency, na.rm=TRUE),
            totalCount = first(count))

ggplot(summary, aes(x = Year)) +
  # Line plot for Frequency
  geom_line(aes(y = freqSum), color = "skyblue", size = 1.5) +
  geom_point(aes(y = freqSum), color = "skyblue", size = 3) +
  # Line plot for Count
  geom_line(aes(y = totalCount), color = "orange", size = 1.5, linetype = "dashed") +
  geom_point(aes(y = totalCount), color = "orange", size = 3) +
  labs(title = "Total Frequency and Count by Year",
       x = "Year",
       caption = "Orange dashed line represents total undergrad students, 
       Blue solid line represents total meal plans purchased") +
  theme_minimal()


1 - pchisq(deviance(m.pois1), df = m.pois1$df.residual)
## p-value is 0
## goodness of fit test is statistically significant, data do not fit the model well
## residual deviance > degrees of freedom --> over dispersion
## do I need to do quasipoisson?

## Pearson GOF - 'the model is appropriate' vs 'not appropriate'
Pearson <- sum((na.omit(data.final$Frequency) - m.pois1$fitted.values)^2 
               / m.pois1$fitted.values)
1 - pchisq(Pearson, df = m.pois1$df.residual)
## p-value is 1, overwhelming evidence that model is not appropriate



## do model selection

## 