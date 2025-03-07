library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(dplyr)
library(markovchain)
library(forcats)
library(viridis)

# Load datasets
clean_data <- read.csv("./data_folder/clean/CleanDiningData.csv")
current_data <- read.csv("./data_folder/clean/CurrentDiningData.csv")

term_order <- c("Fall 2021", "Spring 2022", "Fall 2022", "Spring 2023", 
                "Fall 2023", "Spring 2024", "Fall 2024", "Spring 2025", "Fall 2025")

# Define UI
dashboard_ui <- dashboardPage(
  dashboardHeader(title = "Dining Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Explanatory Analysis", icon = icon("chart-bar"),
               menuSubItem("Housing", tabName = "housing"),
               menuSubItem("Meal Plans", tabName = "mealplans")
      ),
      menuItem("Predictions/Models", icon = icon("cogs"),
               menuSubItem("Price Prediction (Poisson)", tabName = "price_prediction"),
               menuSubItem("Churn Rate (Markov)", tabName = "churn_rate")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview", 
              fluidRow(
                box(title = "Data Summary", width = 12, status = "primary", solidHeader = TRUE, 
                    dataTableOutput("data_summary"))
              )
      ),
      tabItem(tabName = "housing", 
              fluidRow(
                box(title = "Student Distribution by Room Location", width = 12, status = "info", solidHeader = TRUE, 
                    plotlyOutput("housing_plot"))
              )
      ),
      tabItem(tabName = "mealplans", 
              fluidRow(
                box(title = "Meal Plan Popularity Trends Over Time", width = 12, status = "info", solidHeader = TRUE, 
                    plotlyOutput("mealplan_trend_plot")),
                box(title = "Overall Meal Plan Popularity", width = 6, status = "info", solidHeader = TRUE, 
                    plotlyOutput("mealplan_popularity_plot")),
                box(title = "Top 5 Meal Plans by Year", width = 6, status = "info", solidHeader = TRUE, 
                    plotlyOutput("top_mealplans_plot"))
              )
      ),
      tabItem(tabName = "price_prediction", 
              box(title = "Price Prediction using Poisson Model", width = 12, status = "warning", solidHeader = TRUE, 
                  verbatimTextOutput("poisson_output"))
      ),
      tabItem(tabName = "churn_rate", 
              box(title = "Churn Rate using Markov Model", width = 12, status = "warning", solidHeader = TRUE, 
                  verbatimTextOutput("markov_output"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  output$data_summary <- renderDataTable({
    head(clean_data)
  })
  
  output$housing_plot <- renderPlotly({
    housing_distribution <- current_data %>%
      group_by(Room.Location.Description, Meal.Plan.Description) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      ungroup()
    
    room_order <- housing_distribution %>%
      group_by(Room.Location.Description) %>%
      summarise(Total_Count = sum(Count)) %>%
      arrange(desc(Total_Count)) %>%
      pull(Room.Location.Description)
    
    housing_distribution$Room.Location.Description <- factor(housing_distribution$Room.Location.Description, levels = room_order)
    
    ggplotly(
      ggplot(housing_distribution, aes(x = Room.Location.Description, y = Count, fill = Meal.Plan.Description)) +
        geom_bar(stat = "identity", position = "stack") +  
        scale_fill_viridis_d() +  
        labs(title = "Student Distribution by Room Location and Meal Plan", x = "Room Location", y = "Number of Students", fill = "Meal Plan") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  output$mealplan_trend_plot <- renderPlotly({
    meal_plan_trends <- current_data %>%
      filter(Term.Session.Description %in% term_order) %>%
      group_by(Term.Session.Description) %>%
      mutate(Total_Students = n()) %>%
      group_by(Term.Session.Description, Meal.Plan.Description) %>%
      summarise(Count = n(), Total_Students = first(Total_Students), .groups = 'drop') %>%
      mutate(Percentage = (Count / Total_Students) * 100) %>%
      mutate(Term.Session.Description = factor(Term.Session.Description, levels = term_order))
    
    ggplotly(
      ggplot(meal_plan_trends, aes(x = Term.Session.Description, y = Percentage, color = Meal.Plan.Description, group = Meal.Plan.Description)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        scale_color_viridis(discrete = TRUE) +
        labs(title = "Meal Plan Popularity Trends Over Time", x = "Term", y = "Percentage of Students") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
}

# Run the app
shinyApp(ui = dashboard_ui, server = server)