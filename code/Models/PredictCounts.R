library(dplyr)
library(ggplot2)
library("ggResidpanel")
library(MASS)

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
  select(Year, count) %>%
  slice(rep(1:n(), each=2)) %>%
  mutate(Term = c(1:8))

data.final <- counts %>%
  left_join(dining %>% dplyr::select(MealPlan, Term, Price) %>% unique(), by = c("MealPlan", "Term")) %>%
  dplyr::select(MealPlan, Term, Price, Frequency) %>%
  left_join(undergradCounts, by="Term")

g <- ggplot(data.final,
            aes(x     = MealPlan,
                y     = Price,
                color = Term)) +
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.1, jitter.height = 0,
      dodge.width  = 0.1))
g + scale_y_log10()



nd <- data.final %>%
  dplyr::select(Term, MealPlan) %>%
  unique()

p <- bind_cols(
  nd,
  predict(m3,
          newdata = nd,
          se.fit = TRUE)|>
    as.data.frame() |>
    
    # Manually construct confidence intervals
    mutate(
      lwr = fit - qnorm(0.975) * se.fit,
      upr = fit + qnorm(0.975) * se.fit,
      
      # Exponentiate to get to response scale
      freq = exp(fit),
      lwr    = exp(lwr),
      upr    = exp(upr)
    ) 
)

m1 <- lm(
  Frequency ~ Term + MealPlan + Price + count,
  data=data.final
)
summary(m1)

m1.1 <- lm(
  Frequency ~ Term + MealPlan + Price,
  data=data.final
)
summary(m1.1)
anova(m1, m1.1) # count is not an important predictor

resid_panel(m1,
            plots    = c("resid", "index", "qq", "cookd"),
            qqbands  = TRUE,
            smoother = TRUE)

m2 <- lm(
  Frequency ~ Term + MealPlan + Price + Term*MealPlan,
  data=data.final
)
summary(m2)


resid_panel(m2,
            plots    = c("resid", "index", "qq", "cookd"),
            qqbands  = TRUE,
            smoother = TRUE)

m3 <- lm(
  log(Frequency) ~ Term + MealPlan + Price + Term*MealPlan,
  data=data.final
)
summary(m3)


resid_panel(m3,
            plots    = c("resid", "index", "qq", "cookd"),
            qqbands  = TRUE,
            smoother = TRUE)


m4 <- lm(
  sqrt(Frequency) ~ Term + MealPlan + Price + count + Term*MealPlan,
  data=data.final
)
summary(m4)

resid_panel(m4,
            plots    = c("resid", "index", "qq", "cookd"),
            qqbands  = TRUE,
            smoother = TRUE)

