library(dplyr)
library(ggplot2)
library("ggResidpanel")
library(MASS)
library(ISLR2)
library(glmnet)
library(car)

# read in the data
dining <- read.csv("../../data_folder/clean/CurrentDiningData.csv")
regents <- read.csv("../../data_folder/clean/CleanRegents.csv")
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


numeric_data <- data.final %>%
  select_if(is.numeric)

# Calculate the correlation matrix
cor(numeric_data)
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

lm_final <- lm(log(MealPlanCount) ~ MealPlan + Semester + Year + UndergradCount, data=data.final)
summary(lm_final)

resid_panel(lm_final)
resid_xpanel(lm_final)

# MSE and RMSE
mse2 <- mean((log(data.clean$MealPlanCount) - predict(lm_final, data.clean))^2) ## MSE
sqrt(mse2) ## RMSE = average difference between actual and predicted in LOG UNITS
## on average, the model's predictions are off by 0.31 units on the log scale of MealPlanCounts

exp(sqrt(mse2)) ## 1.35, the model predictions are off by about 35% 


#-------------------------------------------------------------------------------
# Poisson Models
#-------------------------------------------------------------------------------

m.pois.full <- glm(MealPlanCount ~ .,
                   data=data.final,
                   family=poisson(link="log"))

summary(m.pois.full)
#vif(m.pois.full) # doesn't work because two variables are highly correlated (Term and Year)

m.pois.2 <- glm(MealPlanCount ~ MealPlan + Semester + Year + UndergradCount,
                data=data.final,
                family=poisson(link="log"))
summary(m.pois.2)
vif(m.pois.2) # ideal VIF values meaning no multicollinearity

m.pois.2log <- glm(MealPlanCount ~ MealPlan + Semester + Year + log(UndergradCount),
                   data=data.final,
                   family=poisson(link="log"))

summary(m.pois.2log)

## Compare the two models

### AIC ###
AIC(m.pois.2)
AIC(m.pois.2log)
## models have the same number of parameters, so AIC is very similar

### deviance ###
deviance(m.pois.2)
deviance(m.pois.2log)

### Chi-Square test ###
anova(m.pois.2, m.pois.2log, test = "Chisq")
## there is no added improvement in the model taking a log of UndergradCount
## so we will use m.pois.2

plot(residuals(m.pois.2)) 
abline(h = 0, col = "red") # residuals have a good scatter



predictions <- predict(m.pois.2, type = "response") # on the scale of MealPlanCount
actuals <- data.clean$MealPlanCount

# plot Predicted vs Actual
plot(predictions, actuals, main = "Predicted vs Actual", xlab = "Predicted", ylab = "Actual")
abline(0, 1, col = "red") 

# RMSE
rmse <- sqrt(mean((predictions - actuals)^2))
rmse


summary <- data.final %>%
  group_by(Year) %>%
  summarize(freqSum = sum(MealPlanCount, na.rm=TRUE),
            totalCount = first(UndergradCount))

#-------------------------------------------------------------------------------
# Cross Validation (k-fold)
#-------------------------------------------------------------------------------

## Using cross validation to train the model and test on data it hasn't seen
## and compare the linear model and the Poisson model

library(caret)

k <- 10 # number of folds/times to rerun the code

folds <- sample(1:k,nrow(data.clean),replace=TRUE)
rmse_log_linear <- c()
rmse_poisson <- c()

for(i in 1:k){
  test_index = folds[[i]]
  
  test <- data.clean[test_index,]
  train <- data.clean[-test_index,]
  
  M1 <- lm(log(MealPlanCount) ~ MealPlan + Semester + UndergradCount, data=train)
  M1_count <- exp(predict(M1,newdata=test[,-5]))
  M2 <- glm(MealPlanCount ~ MealPlan + Semester + Year + UndergradCount,
            data=train,
            family=poisson(link="log"))
  M2_count <- predict(M2, newdata=test[,-5], type="response")
  cat("Log-Linear model:", M1_count, "\n Poisson model:", M2_count, "\n True value:",
      test[,5], "\n")
  
}

## use GOF and pseudo Rsquare to test

library(caret)

k <- 10 # number of folds/times to rerun the code
folds <- sample(1:k, nrow(data.clean), replace=TRUE)

# Initialize vectors to store evaluation metrics for each fold
rmse_log_linear <- c()
rmse_poisson <- c()
mae_log_linear <- c()
mae_poisson <- c()
r2_log_linear <- c()  # Only for the log-linear model
deviance_poisson <- c()  # Only for the Poisson model

for(i in 1:k) {
  test_index <- which(folds == i)
  
  test <- data.clean[test_index,]
  train <- data.clean[-test_index,]
  
  # Log-Linear Model (M1)
  M1 <- lm(log(MealPlanCount) ~ MealPlan + Semester + UndergradCount, data=train)
  M1_count <- exp(predict(M1, newdata=test[,-5]))
  
  # Poisson Model (M2)
  M2 <- glm(MealPlanCount ~ MealPlan + Semester + Year + UndergradCount,
            data=train, family=poisson(link="log"))
  M2_count <- predict(M2, newdata=test[,-5], type="response")
  
  # True values
  true_values <- test[, 5]
  
  # Log-Linear Model Evaluation
  rmse_log_linear[i] <- sqrt(mean((M1_count - true_values)^2))
  mae_log_linear[i] <- mean(abs(M1_count - true_values))
  r2_log_linear[i] <- summary(M1)$r.squared  # R-squared for the log-linear model
  
  # Poisson Model Evaluation
  rmse_poisson[i] <- sqrt(mean((M2_count - true_values)^2))
  mae_poisson[i] <- mean(abs(M2_count - true_values))
  deviance_poisson[i] <- deviance(M2)  # Deviance for the Poisson model
  
  cat("Fold:", i, "\n")
  cat("Log-Linear model: RMSE =", rmse_log_linear[i], " MAE =", mae_log_linear[i], " R-squared =", r2_log_linear[i], "\n")
  cat("Poisson model: RMSE =", rmse_poisson[i], " MAE =", mae_poisson[i], " Deviance =", deviance_poisson[i], "\n")
}

# Calculate and print average metrics across all folds
cat("\nAverage Metrics:\n")
cat("Log-Linear model: RMSE =", mean(rmse_log_linear), " MAE =", mean(mae_log_linear), " R-squared =", mean(r2_log_linear), "\n")
cat("Poisson model: RMSE =", mean(rmse_poisson), " MAE =", mean(mae_poisson), " Deviance =", mean(deviance_poisson), "\n")

#-------------------------------------------------------------------------------
# Final Model, Diagnostics, and Visualizations
#-------------------------------------------------------------------------------

finalModel <- glm(MealPlanCount ~ MealPlan + Semester + Year + UndergradCount,
                  data=data.clean, family=poisson(link="log"))

summary(finalModel)

predictions <- predict(finalModel, type="response", se.fit=TRUE)
lower.ci <- predictions$fit - 1.96*predictions$se.fit
upper.ci <- predictions$fit + 1.96*predictions$se.fit

data.predict <- data.clean %>%
  mutate(prediction = predictions$fit,
         lower_ci = lower.ci,
         upper_ci = upper.ci)

plot(data.predict$MealPlanCount, data.predict$prediction)

RMSE <- sqrt(mean((data.predict$prediction-data.predict$MealPlanCount)^2))

plot(finalModel$residuals ~ fitted(finalModel))

# plot residuals
#ggplot(aes(x=predictions$fit, y=finalModel$residuals)) +
#  geom_point(color="black", alpha=1) +
#  geom_abline(slope=0, intercept = 0, color="blue", linetype="solid") +
#  labs(
#    title="Residuals vs Predicted",
#    x = "Residuals",
#    y = "Predicted Values"
#  ) +
#  theme_minimal()


plot(predictions$fit, finalModel$residuals)
abline(h=0, lty=2)

qqnorm(res)
qqline(res)

## Plot actual vs predicted with confidence interavals
ggplot(data.predict, aes(x = MealPlanCount, y = prediction)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "red") +
  geom_abline(slope = 1, intercept = 0, color = "green", linetype = "dashed") + 
  labs(
    title = "Actual vs. Predicted Meal Plan Count",
    x = "Actual Meal Plan Count",
    y = "Predicted Meal Plan Count"
  ) +
  theme_minimal()

