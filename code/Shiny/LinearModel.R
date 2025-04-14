library(dplyr)

fit_linear_model <- function() {
  # Read data
  dining <- read.csv("CurrentDiningData.csv")
  regents <- read.csv("CleanRegents.csv")
  
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
  
  # Fit Poisson regression model
  m1 <- glm(
    MealPlanCount ~ MealPlan + Semester + Year + UndergradCount,
    data = data.final,
    family = poisson(link = "log")
  )
  
  return(list(model = m1, data = data.final))
}
