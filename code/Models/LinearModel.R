
library(dplyr)
  
fit_linear_model <- function() {
    # Read data
    dining <- read.csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/code/Shiny/CurrentDiningData.csv")
    regents <- read.csv("C:/Users/landa/Documents/DS 401 Project/DS4010_E/code/Shiny/CleanRegents.csv")
    
    # Process dining data
    dining <- dining %>%
      mutate(
        MealPlan = factor(Meal.Plan.Description),
        Term = as.numeric(factor(Term.Session.Description, 
                                 levels = c("Fall 2021", "Spring 2022", "Fall 2022", "Spring 2023", 
                                            "Fall 2023", "Spring 2024", "Fall 2024", "Spring 2025"))),
        Price = Price.Year / 2
      )
    
    # Create frequency table for MealPlan and Term
    counts <- as.data.frame(table(dining$MealPlan, dining$Term)) %>%
      rename(MealPlan = Var1, Term = Var2, MealPlanCount = Freq) %>%
      mutate(Term = as.numeric(Term), 
             MealPlanCount = ifelse(MealPlanCount == 0, NA, MealPlanCount))
    
    # Process regents data for undergraduate counts
    undergradCounts <- regents %>%
      filter(Student.Classification == "Undergraduate") %>%
      dplyr::select(Year, count) %>%
      slice(rep(1:n(), each = 2)) %>%
      mutate(Term = c(1:8),
             UndergradCount = count) %>%
      dplyr::select(-count)
    
    # Combine data into final dataset
    data.final <- counts %>%
      left_join(dining %>% dplyr::select(MealPlan, Term, Price) %>% distinct(), by = c("MealPlan", "Term")) %>%
      dplyr::select(MealPlan, Term, Price, MealPlanCount) %>%
      left_join(undergradCounts, by = "Term") %>%
      mutate(Semester = ifelse(Term %% 2 == 0, "Spring", "Fall")) %>%
      dplyr::select(MealPlan, Term, Semester, Year, MealPlanCount, UndergradCount, Price)
    
    # Fit linear regression model
    m1 <- lm(
      MealPlanCount ~ .,
      data=data.final
    )
    
    # Return the model and processed data
    return(list(model = m1, data = data.final))
}