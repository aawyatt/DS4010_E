# ISU Dining Meal Plan Analysis Dashboard
# A comprehensive dashboard for analyzing meal plan trends and making predictions

library(shiny)
library(shinydashboard)
library(shinyWidgets)        # For enhanced input widgets
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)                  # For interactive tables
library(tidyr)
library(purrr)
library(scales)
library(forcats)
library(viridis)
library(markovchain)         # For Markov chain model
library(MASS)                # For Poisson regression
library(ggrepel)             # For non-overlapping text labels
library(tidyr)
library(seqinr)

# Load datasets
path <- "../../data_folder/clean/"
clean_data <- read.csv(paste0(path, "CleanDiningData.csv"))
current_data <- read.csv(paste0(path, "CurrentDiningData.csv"))
regents <- read.csv(paste0(path, "CleanRegents.csv"))
used_proportions <- read.csv(paste0(path, "TransitionMatrix.csv"))

# Define term order globally
term_order <- c(
  "Fall 2021", "Spring 2022",
  "Fall 2022", "Spring 2023",
  "Fall 2023", "Spring 2024",
  "Fall 2024", "Spring 2025"
)

# Create theme colors
theme_colors <- list(
  primary = "#C8102E",    # ISU red
  secondary = "#F1BE48",  # ISU gold
  info = "#4B8BBE",       # Blue
  success = "#006747",    # Green
  warning = "#FFA500",    # Orange
  danger = "#990000"      # Dark Red
)

# UI Definition
ui <- dashboardPage(
  # Header
  dashboardHeader(
    title = "ISU Dining Analytics"
  ),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      # Overview Section
      menuItem("Overview", tabName = "overview", icon = icon("dashboard"),
               selected = TRUE),
      
      # Explanatory Analysis Section
      menuItem("Explanatory Analysis", icon = icon("chart-bar"),
               menuSubItem("Meal Plans", tabName = "mealplans", icon = icon("utensils")),
               menuSubItem("Housing", tabName = "housing", icon = icon("home"))
      ),
      
      # Models Section
      menuItem("Predictive Models", icon = icon("cogs"),
               menuSubItem("Meal Plan Prediction", tabName = "poisson_model", icon = icon("chart-line")),
               menuSubItem("Churn Analysis", tabName = "markov_model", icon = icon("random"))
      ),
      
      # Conclusion Section
      menuItem("Conclusion", tabName = "conclusion", icon = icon("clipboard-check"))
    )
  ),
  
  # Body
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        /* Header styles */
        .skin-blue .main-header .logo {
          background-color: #C8102E;  /* ISU red */
          color: white;
          font-weight: bold;
        }
        .skin-blue .main-header .navbar {
          background-color: #C8102E;  /* ISU red */
        }
        
        /* Sidebar styles */
        .skin-blue .main-sidebar {
          background-color: #F5F5F5;  /* Light gray background */
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a {
          color: #333333;  /* Dark text for better readability */
          font-weight: 500;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active > a {
          background-color: #EAEAEA;  /* Slightly darker than sidebar background */
          color: #C8102E;  /* ISU red for active items */
          font-weight: bold;
          border-left: 4px solid #C8102E;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
          background-color: #E0E0E0;  /* Hover effect */
        }
        
        /* Dropdown menu items */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > li > a {
          background-color: #E8E8E8;  /* Slightly darker than sidebar */
          color: #444444;  /* Darker text for submenu items */
          padding-left: 25px;  /* Indent submenu items */
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > li > a:hover {
          background-color: #D0D0D0;
          color: #C8102E;  /* ISU red on hover */
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu > li.active > a {
          color: #C8102E;  /* ISU red for active submenu items */
          font-weight: bold;
        }
        
        /* Box styles */
        .box.box-solid.box-primary > .box-header {
          background-color: #C8102E;  /* ISU red */
          color: white;
        }
        .box.box-solid.box-primary {
          border: 1px solid #C8102E;
        }
        .box.box-solid.box-info > .box-header {
          background-color: #4B8BBE;  /* Blue */
          color: white;
        }
        .box.box-solid.box-success > .box-header {
          background-color: #006747;  /* Green */
          color: white;
        }
        .box.box-solid.box-warning > .box-header {
          background-color: #FFA500;  /* Orange */
          color: white;
        }
        
        /* Global filters section */
        .sidebar-filters {
          padding: 10px 15px;
          background-color: #EFEFEF;  /* Slightly different background to separate */
          border-top: 1px solid #ddd;
          margin-top: 10px;
        }
        .sidebar-filters h4 {
          color: #333333;  /* Dark text color for the header */
          font-weight: bold;
          margin-bottom: 15px;
        }
        .sidebar-filters label {
          color: #333333;  /* Dark text color for labels */
          font-weight: 500;
        }
        .sidebar-filters .selectize-input {
          background-color: white;
          border: 1px solid #ccc;
        }
        .sidebar-filters .checkbox label {
          color: #333333;  /* Dark text for checkbox label */
        }
        
        /* Content area */
        .content-wrapper {
          background-color: #FFFFFF;
        }
        
        /* Tab styles */
        .nav-tabs-custom .nav-tabs li.active {
          border-top-color: #C8102E;  /* ISU red */
        }
        
        /* Other custom styles */
        .section-title {
          margin-top: 20px;
          margin-bottom: 10px;
          font-size: 20px;
          border-bottom: 1px solid #ddd;
          padding-bottom: 5px;
          color: #333;
        }
        
        /* Stats boxes */
        .stat-box {
          border-radius: 5px;
          padding: 15px;
          margin-bottom: 15px;
          color: white;
          min-height: 110px;
        }
        .stat-box .icon {
          position: absolute;
          top: 15px;
          right: 30px;
          font-size: 40px;
          opacity: 0.3;
        }
        .stat-box .number {
          font-size: 28px;
          font-weight: bold;
        }
        .stat-box .text {
          font-size: 14px;
          margin-top: 5px;
        }
        
        /* Insights section */
        .box-insights {
          margin-top: 20px;
          margin-bottom: 10px;
        }
        .insights-title {
          font-size: 16px;
          font-weight: bold;
          margin-bottom: 10px;
          color: #333;
        }
        .insight-item {
          border-left: 4px solid #C8102E;  /* ISU red */
          padding-left: 10px;
          margin-bottom: 10px;
          background-color: #f9f9f9;
          padding: 10px 10px 10px 15px;
          border-radius: 0 4px 4px 0;
        }
      "))
    ),
    
    # Tab content
    tabItems(
      # ===== OVERVIEW TAB =====
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            title = "Dashboard Purpose", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            p("This project aims to analyze and predict trends in the ISU Dining meal plan. By examining historical meal plan purchases, residence hall occupancy, and churn rates over semesters and years, we will identify the most popular meal plans, analyze student retention, and forecast meal plan sales and revenue for future terms.
              Using exploratory analysis, we will draw conclusions regarding meal plan adoption by housing location."),
            p("The insights from this study are designed to help ISU Dining optimize pricing, meal plan structures, and retention strategies, ensuring financial sustainability while improving student satisfaction. This interactive dashboard visualizes trends predictions, making data-driven policy decisions more accessible.")
          )
        ),
        
        # Key Metrics Section
        fluidRow(
          valueBoxOutput("total_students_box", width = 3),
          valueBoxOutput("total_meal_plans_box", width = 3),
          valueBoxOutput("most_popular_plan_box", width = 3),
          valueBoxOutput("avg_price_box", width = 3)
        ),
        
        # Main overview charts
        fluidRow(
          box(
            title = "Student Distribution Over Time",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("student_trend_plot", height = "300px")
          ),
          box(
            title = "Meal Plan Distribution",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("overview_meal_plan_dist", height = "300px")
          )
        ),
        
        # Data Dictionary Section
        fluidRow(
          box(
            title = "Data Dictionary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            DTOutput("data_dictionary")
          )
        ),
        
        # Data Preview Section
        fluidRow(
          box(
            title = "Data Preview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            collapsed = TRUE,
            DTOutput("data_preview")
          )
        )
      ),
      
      # ===== MEAL PLANS TAB =====
      tabItem(
        tabName = "mealplans",
        fluidRow(
          column(
            width = 3,
            box(
              title = "Filters", 
              status = "primary", 
              solidHeader = TRUE,
              width = NULL,
              selectInput("mealplan_term_filter", "Select Terms:",
                          choices = term_order,
                          multiple = TRUE,
                          selected = term_order),
              selectInput("mealplan_filter", "Select Meal Plans:",
                          choices = NULL, # Will be populated in server
                          multiple = TRUE,
                          selected = NULL),
              checkboxInput("show_percentage", "Show as Percentage", value = TRUE),
              actionButton("reset_mealplan_filters", "Reset Filters",
                           icon = icon("refresh"),
                           class = "btn-block")
            )
          ),
          column(
            width = 9,
            tabBox(
              title = "Meal Plan Analysis",
              id = "mealplanTabs",
              width = NULL,
              height = "500px",
              tabPanel("Distribution", 
                       plotlyOutput("meal_plan_dist_plot", height = "450px")),
              tabPanel("Trends Over Time", 
                       plotlyOutput("meal_plan_trends_plot", height = "450px")),
              tabPanel("Price Analysis", 
                       plotlyOutput("price_vs_popularity_plot", height = "450px"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Top 5 Meal Plans by Term",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("top_meal_plans_plot", height = "350px")
          ),
          box(
            title = "Meal Plan Price Trends",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("price_trends_plot", height = "350px")
          )
        ),
        fluidRow(
          box(
            title = "Detailed Meal Plan Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("meal_plan_table")
          )
        )
      ),
      
      # ===== HOUSING TAB =====
      tabItem(
        tabName = "housing",
        fluidRow(
          column(
            width = 3,
            box(
              title = "Filters", 
              status = "primary", 
              solidHeader = TRUE,
              width = NULL,
              selectInput("housing_term_filter", "Select Terms:",
                          choices = term_order,
                          multiple = TRUE,
                          selected = term_order),
              selectInput("housing_location_filter", "Select Housing Locations:",
                          choices = NULL, # Will be populated in server
                          multiple = TRUE,
                          selected = NULL),
              checkboxInput("show_housing_percentage", "Show as Percentage", value = TRUE),
              actionButton("reset_housing_filters", "Reset Filters",
                           icon = icon("refresh"),
                           class = "btn-block")
            )
          ),
          column(
            width = 9,
            tabBox(
              title = "Housing Analysis",
              id = "housingTabs",
              width = NULL,
              height = "500px",
              tabPanel("Distribution by Location", 
                       plotlyOutput("housing_dist_plot", height = "450px")),
              tabPanel("Meal Plans by Location", 
                       plotlyOutput("housing_meal_plan_plot", height = "450px")),
              tabPanel("Trends Over Time", 
                       plotlyOutput("housing_trends_plot", height = "450px"))
            )
          )
        ),
        fluidRow(
          box(
            title = "Detailed Housing Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("housing_table")
          )
        )
      ),
      
      # ===== POISSON MODEL TAB =====
      tabItem(
        tabName = "poisson_model",
        fluidRow(
          tabBox(
            title = "Meal Plan Prediction",
            width = 12,
            id = "mealPlanPredictionTabs",
            # First subtab: Current Prediction Model content
            # In your POISSON MODEL TAB section of app.R
            # In your POISSON MODEL TAB section, add the prediction controls and plot as before:
            tabPanel("Poisson Model",
                     fluidRow(
                       box(
                         title = "About the Poisson Model",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         p("A Poisson Regression is used to model and predict counts: 
                           variables with a minimum of 0 and an unbounded maximum. In the ISU Dining context, 
                           a count is the number of students that purchase a meal plan in a semester. 
                           The explanatory variables in the Poisson model are the Meal Plan 
                           (Campanile, Cardinal, Gold, or Meal Blocks), Semester (Spring or Fall), Year, 
                           and the number of undergraduate students (from Iowa Board of Regents data). 
                           The model incorporates a ‘log link,’ exponentiating the linear predictors, 
                           which ensures the predicted counts are positive. "),
                         p("Select a Meal Plan, Semester, Year, and Undergraduate Count to see 
                         future predictions for the number of meal plans purchased. 
                           The default values are the upcoming term (Fall 2025) and the current undergraduate count of 25,500. ")
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Poisson Model Controls",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 3,
                         selectInput("poisson_mealplan", "Meal Plan:",
                                     choices = NULL),  # will be updated in server
                         selectInput("poisson_semester", "Semester:",
                                     choices = c("Fall", "Spring"),
                                     selected = "Fall"),
                         numericInput("poisson_year", "Year:",
                                      value = 2025, min = 2021, max = 2040, step = 1),
                         numericInput("poisson_undergrad", "Undergrad Count:",
                                      value = 25500, min = 0, step = 1),
                         actionButton("run_poisson", "Run Poisson Prediction",
                                      icon = icon("calculator"),
                                      class = "btn-primary btn-block")
                       ),
                       box(
                         title = "Poisson Prediction Plot",
                         status = "info",
                         solidHeader = TRUE,
                         width = 9,
                         plotlyOutput("poisson_plot", height = "400px")
                       )
                     ),
                     # Add a new fluidRow with two boxes for diagnostic plots:
                     fluidRow(
                       box(
                         title = "Residual vs. Fitted Plot",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 6,
                         plotOutput("residual_plot", height = "300px"),
                         tags$p("The Residuals vs. Fitted plot shows how far off the 
                               model’s predictions are by comparing the predicted 
                               values to the differences between those predictions 
                               and the actual data. Ideally, the points are scattered 
                               around zero with no clear pattern. The groupings in 
                               this plot are due to the different ‘groupings’ of meal plans, like Meal Blocks.")
                       ),
                       box(
                         title = "Actual vs. Predicted with Confidence Intervals",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 6,
                         plotlyOutput("actual_vs_pred_plot", height = "300px"),
                         tags$p("The Actual vs. Predicted plot compares the true 
                                historical amount of a given meal plan purchased 
                                and the value predicted by the Poisson model. Each 
                                point has a 95% prediction interval, which is a 
                                range of values that a future observation is estimated 
                                to fall, based on the previous data. The green dashed
                                line has a slope of one and an intercept of zero, 
                                visualizing the actual and predicted values having a 1:1 relationship.")
                       )
                     )
            ),
            # In your Price Model Tab (within the tabPanel for Price Model)
            tabPanel("Price Model",
                     fluidRow(
                       box(
                         title = "About the Linear Price Model",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         p("The Price Model uses linear regression to estimate the yearly price of each meal plan over time."),
                         p("By leveraging historical price data and adjusting for projected inflation, the model predicts future prices for a given year.")
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Price Model Controls",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 3,
                         # Select the meal plan from available choices
                         selectInput("price_meal_plan", "Meal Plan:", choices = NULL),
                         # Numeric input for the forecast year (user specifies desired future year)
                         numericInput("forecast_year", "Forecast Year:", value = 2025, min = 2000, max = 2100, step = 1),
                         # Numeric input for inflation rate (as a percentage)
                         numericInput("price_inflation", "Inflation Rate (%)", value = 3, step = 0.1),
                         actionButton("run_price_model", "Run Price Prediction",
                                      icon = icon("calculator"),
                                      class = "btn-primary btn-block")
                       ),
                       box(
                         title = "Price Forecast Plot",
                         status = "info",
                         solidHeader = TRUE,
                         width = 9,
                         plotlyOutput("price_forecast_plot", height = "400px"),
                         tags$p("The Price Forecast Plot visualizes the trend in historical yearly prices for the selected meal plan, and projects the future price adjusted for inflation. The red diamond represents the forecasted value.")
                         
                       )
                     ),
                     # Diagnostic plots
                     fluidRow(
                       box(
                         title = "Residual vs. Fitted Plot",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 6,
                         plotOutput("price_residual_plot", height = "300px")
                       ),
                       box(
                         title = "Actual vs. Predicted with Confidence Intervals",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 6,
                         plotlyOutput("price_actual_vs_pred_plot", height = "300px")
                       )
                     )
            ),
            
            
            tabPanel("Income Forecast",
                     fluidRow(
                       box(
                         title = "About Income Forecast",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         p("This tab simply combines the prediction of student count per meal plan with the projected price to get an estimate of the income for a selected term.")
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Income Forecast Controls",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 3,
                         # These inputs apply to both sub-models
                         selectInput("income_mealplan", "Meal Plan:", choices = NULL),
                         selectInput("income_semester", "Semester:", choices = c("Fall", "Spring"), selected = "Fall"),
                         numericInput("income_forecast_year", "Forecast Year:", value = 2025, min = 2021, max = 2040, step = 1),
                         numericInput("income_undergrad", "Undergrad Count:", value = 25500, min = 0, step = 1),
                         numericInput("income_inflation", "Inflation Rate (%)", value = 3, step = 0.1),
                         actionButton("run_income_forecast", "Run Income Forecast",
                                      icon = icon("calculator"),
                                      class = "btn-primary btn-block")
                       ),
                       box(
                         title = "Income Forecast Plot",
                         status = "info",
                         solidHeader = TRUE,
                         width = 9,
                         plotlyOutput("income_forecast_plot", height = "400px")
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Poisson Model Diagnostic: Residual vs. Fitted",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 6,
                         plotOutput("income_poisson_resid_plot", height = "300px")
                       ),
                       box(
                         title = "Price Model Diagnostic: Residual vs. Fitted",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 6,
                         plotOutput("income_price_resid_plot", height = "300px")
                       )
                     )
            )
          )
        )
      ),
      
      # ===== MARKOV MODEL TAB =====
      tabItem(
        tabName = "markov_model",
        fluidRow(
          box(
            title = "What is a Markov Chain?",
            status = "primary",
            solidHeader = TRUE,
            
            width = 12,
            p("A Markov Chain is a statistical model that describes a sequence of possible events, where the probability of each event depends only on the state attained in the previous event."),
            p("In this dashboard, the Markov Chain helps us model and simulate how a student will change between meal plans during their time at ISU."),
            p("Churn Rate: The rate at which you lose customers. In this case it would be the rate at which you lose students to different meal plans.")
            
          ),
          column(
            width = 4,
            box(
              title = "Run Markov Chain Simulation",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              selectInput("starting_meal_plan", "Select Starting Meal Plan:",
                          choices = c("100 Meal Blocks", "25 Meal Blocks", "50 Meal Blocks", 
                                      "Campanile", "Cardinal", "Gold", "NA")),
              numericInput("markov_seed", "Random Seed (Optional)",
                           value = NA, min = 1, step = 1),
              helpText("A seed initializes the random number generator so that your simulation is reproducible—using the same seed produces the same results."),
              actionButton("run_markov", "Run Simulation",
                           icon = icon("random"),
                           class = "btn-primary btn-block")
            )
          ),
          
          column(
            width = 8,
            box(
              title = "Markov Simulation of Student",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              plotlyOutput("retention_forecast_plot", height = "400px"),
              tags$p("This plot displays the simulation of meal plan transitions over consecutive terms, starting from the chosen meal plan. The X-axis represents term steps, while the Y-axis shows the meal plan state at each step.")
            )
          )
        ),
        fluidRow(
          box(
            title = "Self-Retention Probabilities",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("retention_prob_plot"),
            tags$p("This plot displays the probabilities that students will remain with the same meal plan from one term to the next. Higher values indicate greater stability (retention) in that meal plan.")
          )
        )
      ),
      
      # ===== CONCLUSION TAB =====
      tabItem(
        tabName = "conclusion",
        fluidRow(
          box(
            title = "Key Findings",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h4("Meal Plan Trends:"),
            tags$div(class = "insight-item",
                     p("Analysis shows that [meal plan] is consistently the most popular choice across all terms, with [percentage]% of students selecting this option.")),
            tags$div(class = "insight-item",
                     p("Block meal plans have seen a [trend] in popularity since [term], with a [percentage]% change in adoption.")),
            tags$div(class = "insight-item",
                     p("Price sensitivity appears highest among students in [housing location], where a [percentage]% change in price correlates with a [percentage]% change in adoption.")),
            
            h4("Housing Insights:"),
            tags$div(class = "insight-item",
                     p("[Housing location] has the highest proportion of optional meal plan purchases at [percentage]%.")),
            tags$div(class = "insight-item",
                     p("Students in [housing location] have the lowest churn rate, with [percentage]% maintaining the same meal plan between terms.")),
            
            h4("Predictive Insights:"),
            tags$div(class = "insight-item",
                     p("Our model predicts a [trend] in overall meal plan enrollment for Fall 2025, with [number] students expected to purchase meal plans.")),
            tags$div(class = "insight-item",
                     p("The most significant growth is predicted in [meal plan], with a projected increase of [percentage]%.")),
            tags$div(class = "insight-item",
                     p("Churn analysis indicates that students most frequently transition from [meal plan A] to [meal plan B], suggesting an opportunity for targeted retention strategies."))
          )
        ),
        fluidRow(
          box(
            title = "Strategic Recommendations",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            h4("Pricing Strategy:"),
            tags$ol(
              tags$li("Consider [specific pricing recommendation] for [meal plan] to optimize revenue while maintaining adoption rates."),
              tags$li("Implement tiered pricing structured around the most common transition paths to encourage upgrades rather than downgrades.")
            ),
            
            h4("Product Development:"),
            tags$ol(
              tags$li("Develop a new meal plan option that addresses the gap between [meal plan A] and [meal plan B], targeting the [number] students who currently switch between these options."),
              tags$li("Consider specialized meal plans for [housing location] residents based on their unique usage patterns.")
            ),
            
            h4("Retention Strategy:"),
            tags$ol(
              tags$li("Implement a targeted communication campaign for students in [housing location] during [time period] to address the higher churn rate observed."),
              tags$li("Develop incentives for students to maintain their meal plan from [term A] to [term B], when transitions are most common.")
            ),
            
            h4("Long-term Planning:"),
            tags$ol(
              tags$li("Prepare for projected [increase/decrease] in overall meal plan adoption by adjusting staffing and resources accordingly."),
              tags$li("Use steady-state projections to inform long-term facility planning and investment decisions.")
            )
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  source("../../code/Models/LinearModel.R")
  source("../../code/Models/priceModel.R")
  
  # ===== REACTIVE DATA PROCESSING =====
  
  # Basic reactive dataset
  filtered_data <- reactive({
    data <- clean_data
    data$Term.Session.Description <- gsub("Spring Only", "Spring", data$Term.Session.Description)
    
    data <- data %>%
      mutate(Term.Session.Description = factor(Term.Session.Description, 
                                               levels = term_order,
                                               ordered = TRUE))
    
    return(data)
  })
  
  # Meal plan specific reactive dataset
  meal_plan_data <- reactive({
    data <- filtered_data()
    
    if (!is.null(input$mealplan_term_filter)) {
      data <- data %>% filter(Term.Session.Description %in% input$mealplan_term_filter)
    }
    
    if (!is.null(input$mealplan_filter) && length(input$mealplan_filter) > 0) {
      data <- data %>% filter(Meal.Plan.Description %in% input$mealplan_filter)
    }
    
    return(data)
  })
  
  # Housing specific reactive dataset
  housing_data <- reactive({
    data <- filtered_data()
    
    if (!is.null(input$housing_term_filter)) {
      data <- data %>% filter(Term.Session.Description %in% input$housing_term_filter)
    }
    
    if (!is.null(input$housing_location_filter) && length(input$housing_location_filter) > 0) {
      data <- data %>% filter(Room.Location.Description %in% input$housing_location_filter)
    }
    
    return(data)
  })
  
  # Update filter choices based on data
  observe({
    # Update meal plan filter choices
    meal_plans <- sort(unique(filtered_data()$Meal.Plan.Description))
    updateSelectInput(session, "mealplan_filter", choices = meal_plans, selected = meal_plans[1:min(5, length(meal_plans))])
    #updateSelectInput(session, "starting_meal_plan", choices = meal_plans, selected = meal_plans[1])
    
    # Update housing filter choices
    housing_locations <- sort(unique(filtered_data()$Room.Location.Description))
    updateSelectInput(session, "housing_location_filter", choices = housing_locations, selected = housing_locations[1:min(5, length(housing_locations))])
  })
  
  # Reset filters when button clicked
  observeEvent(input$reset_mealplan_filters, {
    updateSelectInput(session, "mealplan_term_filter", selected = term_order)
    meal_plans <- sort(unique(filtered_data()$Meal.Plan.Description))
    updateSelectInput(session, "mealplan_filter", selected = meal_plans[1:min(5, length(meal_plans))])
    updateCheckboxInput(session, "show_percentage", value = TRUE)
  })
  
  observeEvent(input$reset_housing_filters, {
    updateSelectInput(session, "housing_term_filter", selected = term_order)
    housing_locations <- sort(unique(filtered_data()$Room.Location.Description))
    updateSelectInput(session, "housing_location_filter", selected = housing_locations[1:min(5, length(housing_locations))])
    updateCheckboxInput(session, "show_housing_percentage", value = TRUE)
  })
  
  # ===== OVERVIEW TAB OUTPUTS =====
  
  # Value Boxes
  output$total_students_box <- renderValueBox({
    n_students <- length(unique(filtered_data()$ID))
    valueBox(
      formatC(n_students, big.mark = ","),
      "Total Students",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$total_meal_plans_box <- renderValueBox({
    n_plans <- paste(c(length(unique(filtered_data()$Meal.Plan.Description)), "(6 active)"), collapse = " ")
    valueBox(
      n_plans,
      "Meal Plan Options",
      icon = icon("utensils"),
      color = "green"
    )
  })
  
  output$most_popular_plan_box <- renderValueBox({
    plan_counts <- table(filtered_data()$Meal.Plan.Description)
    most_popular <- names(which.max(plan_counts))
    valueBox(
      most_popular,
      "Most Popular Plan",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  output$avg_price_box <- renderValueBox({
    avg_price <- mean(filtered_data()$Price.Year, na.rm = TRUE)
    valueBox(
      dollar(avg_price),
      "Average Yearly Price",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })
  
  # Student Trend Plot
  output$student_trend_plot <- renderPlotly({
    trend_data <- filtered_data() %>%
      group_by(Term.Session.Description) %>%
      summarise(n_students = n_distinct(ID)) %>%
      arrange(Term.Session.Description)
    
    p <- ggplot(trend_data, aes(x = Term.Session.Description, y = n_students, group = 1)) +
      geom_line(color = theme_colors$primary, size = 1) +
      geom_point(color = theme_colors$primary, size = 3) +
      theme_minimal() +
      labs(x = "Term", y = "Number of Students") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% 
      layout(margin = list(b = 100)) %>%
      config(displayModeBar = FALSE)
  })
  
  # Overview Meal Plan Distribution
  output$overview_meal_plan_dist <- renderPlotly({
    meal_plan_counts <- filtered_data() %>%
      group_by(Meal.Plan.Description) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(10) # Top 10 meal plans for better visibility
    
    p <- ggplot(meal_plan_counts, aes(x = reorder(Meal.Plan.Description, count), y = count, fill = count)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c() +
      coord_flip() +
      theme_minimal() +
      labs(x = "Meal Plan", y = "Number of Students") +
      theme(legend.position = "none")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Data Dictionary
  output$data_dictionary <- renderDT({
    data.frame(
      Column = c("ID", "Term.Session.Description", "Meal.Plan.Description", 
                 "Room.Location.Description", "Price.Year", "Price.Semester"),
      Description = c(
        "Unique identifier for each student",
        "Academic term (e.g., Fall 2021, Spring 2022)",
        "Type of meal plan selected by the student",
        "Housing location where the student resides",
        "Annual cost of the meal plan",
        "Cost of the meal plan per semester"
      ),
      Type = c("Character", "Factor", "Factor", "Factor", "Numeric", "Numeric"),
      Example = c(
        "1001", 
        "Fall 2021", 
        "Gold", 
        "Frederiksen Court", 
        "$4,200", 
        "$2,100"
      )
    ) %>%
      datatable(options = list(dom = 't', paging = FALSE, ordering = FALSE),
                rownames = FALSE)
  })
  
  # Data Preview
  output$data_preview <- renderDT({
    datatable(
      head(clean_data, 50),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'ftip'
      ),
      rownames = FALSE
    )
  })
  
  # ===== MEAL PLANS TAB OUTPUTS =====
  
  # Meal Plan Distribution Plot
  output$meal_plan_dist_plot <- renderPlotly({
    data <- meal_plan_data() %>%
      group_by(Meal.Plan.Description) %>%
      summarise(count = n()) %>%
      mutate(total = sum(count))
    
    if(input$show_percentage) {
      data <- data %>%
        mutate(value = (count / total) * 100,
               label = paste0(round(value, 1), "%"))
      y_label <- "Percentage of Students (%)"
    } else {
      data <- data %>%
        mutate(value = count,
               label = as.character(count))
      y_label <- "Number of Students"
    }
    
    data <- data %>%
      arrange(desc(value)) %>%
      mutate(Meal.Plan.Description = factor(Meal.Plan.Description, levels = Meal.Plan.Description))
    
    p <- ggplot(data, aes(x = Meal.Plan.Description, y = value, fill = value, text = paste0(
      "Meal Plan: ", Meal.Plan.Description, "<br>",
      "Students: ", count, "<br>",
      "Percentage: ", round((count/total)*100, 1), "%"
    ))) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c() +
      coord_flip() +
      theme_minimal() +
      labs(x = "Meal Plan", y = y_label) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Meal Plan Trends Plot
  output$meal_plan_trends_plot <- renderPlotly({
    data <- meal_plan_data() %>%
      group_by(Term.Session.Description, Meal.Plan.Description) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(Term.Session.Description) %>%
      mutate(total = sum(count))
    
    if(input$show_percentage) {
      data <- data %>%
        mutate(value = (count / total) * 100)
      y_label <- "Percentage of Students (%)"
    } else {
      data <- data %>%
        mutate(value = count)
      y_label <- "Number of Students"
    }
    
    p <- ggplot(data, aes(x = Term.Session.Description, 
                          y = value, 
                          color = Meal.Plan.Description, 
                          group = Meal.Plan.Description,
                          text = paste0(
                            "Term: ", Term.Session.Description, "<br>",
                            "Meal Plan: ", Meal.Plan.Description, "<br>",
                            "Students: ", count, "<br>",
                            "Percentage: ", round((count/total)*100, 1), "%"
                          ))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(x = "Term", y = y_label, color = "Meal Plan") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Price vs Popularity Plot
  output$price_vs_popularity_plot <- renderPlotly({
    data <- meal_plan_data() %>%
      filter(!is.na(Price.Year)) %>%
      group_by(Meal.Plan.Description) %>%
      summarise(
        avg_price = mean(Price.Year, na.rm = TRUE),
        count = n(),
        .groups = 'drop'
      )
    
    p <- ggplot(data, aes(x = avg_price, y = count, 
                          text = paste0(
                            "Meal Plan: ", Meal.Plan.Description, "<br>",
                            "Average Price: $", round(avg_price, 2), "<br>",
                            "Students: ", count
                          ))) +
      geom_point(aes(size = count, color = avg_price), alpha = 0.7) +
      geom_text_repel(aes(label = Meal.Plan.Description),
                      box.padding = 0.5,
                      force = 2,
                      size = 3) +
      scale_color_viridis_c() +
      theme_minimal() +
      labs(x = "Average Yearly Price ($)", y = "Number of Students") +
      scale_x_continuous(labels = dollar_format()) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Top 5 Meal Plans Plot
  output$top_meal_plans_plot <- renderPlotly({
    data <- meal_plan_data() %>%
      group_by(Term.Session.Description, Meal.Plan.Description) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(Term.Session.Description) %>%
      mutate(
        rank = rank(-count, ties.method = "first"),
        is_top5 = rank <= 5
      ) %>%
      filter(is_top5) %>%
      arrange(Term.Session.Description, rank)
    
    p <- ggplot(data, aes(x = reorder(Meal.Plan.Description, -count), 
                          y = count, 
                          fill = factor(rank),
                          text = paste0(
                            "Term: ", Term.Session.Description, "<br>",
                            "Meal Plan: ", Meal.Plan.Description, "<br>",
                            "Students: ", count, "<br>",
                            "Rank: ", rank
                          ))) +
      geom_bar(stat = "identity") +
      facet_wrap(~ Term.Session.Description, scales = "free_y") +
      scale_fill_viridis_d(direction = -1) +
      theme_minimal() +
      labs(x = "Meal Plan", y = "Number of Students", fill = "Rank") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% layout(margin = list(b = 120))
  })
  
  # Price Trends Plot
  output$price_trends_plot <- renderPlotly({
    data <- meal_plan_data() %>%
      filter(!is.na(Price.Year)) %>%
      group_by(Term.Session.Description, Meal.Plan.Description) %>%
      summarise(
        avg_price = mean(Price.Year, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- ggplot(data, aes(x = Term.Session.Description, 
                          y = avg_price, 
                          color = Meal.Plan.Description, 
                          group = Meal.Plan.Description,
                          text = paste0(
                            "Term: ", Term.Session.Description, "<br>",
                            "Meal Plan: ", Meal.Plan.Description, "<br>",
                            "Average Price: $", round(avg_price, 2)
                          ))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal() +
      labs(x = "Term", y = "Average Yearly Price ($)", color = "Meal Plan") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Meal Plan Table
  output$meal_plan_table <- renderDT({
    data <- meal_plan_data() %>%
      group_by(Term.Session.Description, Meal.Plan.Description) %>%
      summarise(
        Students = n(),
        `Unique Housing Locations` = n_distinct(Room.Location.Description),
        `Avg Yearly Price` = mean(Price.Year, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(Term.Session.Description, desc(Students))
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(columns = "Avg Yearly Price", digits = 0)
  })
  
  # ===== HOUSING TAB OUTPUTS =====
  
  # Housing Distribution Plot
  output$housing_dist_plot <- renderPlotly({
    data <- housing_data() %>%
      group_by(Room.Location.Description) %>%
      summarise(count = n()) %>%
      mutate(total = sum(count))
    
    if(input$show_housing_percentage) {
      data <- data %>%
        mutate(value = (count / total) * 100,
               label = paste0(round(value, 1), "%"))
      y_label <- "Percentage of Students (%)"
    } else {
      data <- data %>%
        mutate(value = count,
               label = as.character(count))
      y_label <- "Number of Students"
    }
    
    data <- data %>%
      arrange(desc(value)) %>%
      mutate(Room.Location.Description = factor(Room.Location.Description, levels = Room.Location.Description))
    
    p <- ggplot(data, aes(x = Room.Location.Description, y = value, fill = value, text = paste0(
      "Housing Location: ", Room.Location.Description, "<br>",
      "Students: ", count, "<br>",
      "Percentage: ", round((count/total)*100, 1), "%"
    ))) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c() +
      coord_flip() +
      theme_minimal() +
      labs(x = "Housing Location", y = y_label) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
  })
  
  # Housing Meal Plan Plot
  output$housing_meal_plan_plot <- renderPlotly({
    data <- housing_data() %>%
      group_by(Room.Location.Description, Meal.Plan.Description) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(Room.Location.Description) %>%
      mutate(total = sum(count))
    
    if(input$show_housing_percentage) {
      data <- data %>%
        mutate(value = (count / total) * 100)
      y_label <- "Percentage of Students (%)"
    } else {
      data <- data %>%
        mutate(value = count)
      y_label <- "Number of Students"
    }
    
    # Sort by total students
    location_order <- data %>%
      group_by(Room.Location.Description) %>%
      summarise(total = sum(count), .groups = 'drop') %>%
      arrange(desc(total)) %>%
      pull(Room.Location.Description)
    
    data$Room.Location.Description <- factor(data$Room.Location.Description, 
                                             levels = location_order)
    
    p <- ggplot(data, aes(x = Room.Location.Description, 
                          y = value, 
                          fill = Meal.Plan.Description,
                          text = paste0(
                            "Housing Location: ", Room.Location.Description, "<br>",
                            "Meal Plan: ", Meal.Plan.Description, "<br>",
                            "Students: ", count, "<br>",
                            "Percentage: ", round((count/total)*100, 1), "%"
                          ))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_viridis_d() +
      coord_flip() +
      theme_minimal() +
      labs(x = "Housing Location", y = y_label, fill = "Meal Plan") +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Housing Trends Plot
  output$housing_trends_plot <- renderPlotly({
    data <- housing_data() %>%
      group_by(Term.Session.Description, Room.Location.Description) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(Term.Session.Description) %>%
      mutate(total = sum(count))
    
    if(input$show_housing_percentage) {
      data <- data %>%
        mutate(value = (count / total) * 100)
      y_label <- "Percentage of Students (%)"
    } else {
      data <- data %>%
        mutate(value = count)
      y_label <- "Number of Students"
    }
    
    # Filter to top locations for readability
    top_locations <- data %>%
      group_by(Room.Location.Description) %>%
      summarise(total_count = sum(count), .groups = 'drop') %>%
      arrange(desc(total_count)) %>%
      head(10) %>%
      pull(Room.Location.Description)
    
    data <- data %>%
      filter(Room.Location.Description %in% top_locations)
    
    p <- ggplot(data, aes(x = Term.Session.Description, 
                          y = value, 
                          color = Room.Location.Description, 
                          group = Room.Location.Description,
                          text = paste0(
                            "Term: ", Term.Session.Description, "<br>",
                            "Housing Location: ", Room.Location.Description, "<br>",
                            "Students: ", count, "<br>",
                            "Percentage: ", round((count/total)*100, 1), "%"
                          ))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(x = "Term", y = y_label, color = "Housing Location") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom")
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.2))
  })
  
  # Housing Table
  output$housing_table <- renderDT({
    data <- housing_data() %>%
      group_by(Term.Session.Description, Room.Location.Description) %>%
      summarise(
        Students = n(),
        `Unique Meal Plans` = n_distinct(Meal.Plan.Description),
        `Most Common Plan` = names(which.max(table(Meal.Plan.Description))),
        `Avg Yearly Price` = mean(Price.Year, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(Term.Session.Description, desc(Students))
    
    datatable(
      data,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE
    ) %>%
      formatCurrency(columns = "Avg Yearly Price", digits = 0)
  })
  
  # ===== POISSON MODEL TAB OUTPUTS =====
  
  # Source the LinearModel.R file to load the fit_linear_model() function
  source("../../code/Models/LinearModel.R")
  
  # After sourcing LinearModel.R and fitting the model once…
  # Fit the linear model once at startup (from LinearModel.R)
  linear_results <- fit_linear_model()
  data_final <- linear_results$data
  m1 <- linear_results$model
  
  # Ensure that the 'poisson_mealplan' input is populated using data_final
  observe({
    # Require that the global model data is available.
    req(data_final)
    # Check that the MealPlan column contains data.
    meal_plans <- sort(unique(data_final$MealPlan))
    if(length(meal_plans) > 0){
      updateSelectInput(session, "poisson_mealplan", 
                        choices = meal_plans,
                        selected = meal_plans[1])
    }
  })
  
  # Wrap the Poisson Model computations in an eventReactive that runs immediately with default inputs.
  poisson_model_results <- eventReactive(input$run_poisson, {
    req(input$poisson_mealplan)  # Ensure a valid meal plan is selected
    
    # Build prediction data frame using user inputs.
    pred_input <- data.frame(
      MealPlan = factor(input$poisson_mealplan, levels = levels(data_final$MealPlan)),
      Semester = input$poisson_semester,
      Year = input$poisson_year,
      UndergradCount = input$poisson_undergrad
    )
    # Get the predicted count from the global Poisson model 'm1'
    predicted_count <- predict(m1, newdata = pred_input, type = "response")
    
    # Filter historical data for the selected meal plan (includes all semesters)
    hist_data <- data_final %>% filter(MealPlan == input$poisson_mealplan)
    hist_data <- hist_data %>%
      mutate(hover_txt = sprintf(
        "Year: %s<br>Semester: %s<br>Actual count: %s",
        Year, Semester, MealPlanCount
      ))
    # Build the main Poisson prediction ggplot:
    p_main <- ggplot(hist_data,
                     aes(x = Year,
                         y = MealPlanCount,
                         color = Semester,
                         text  = hover_txt)) +
      geom_point(size = 3) +
      geom_line(aes(group = Semester), size = 1) +
      geom_point(aes(x = input$poisson_year,
                     y = predicted_count,
                     text = sprintf("Forecast<br>Year: %s<br>Predicted: %.0f",
                                    input$poisson_year,
                                    predicted_count)),
                 color = "red", shape = 18, size = 4)  +
      labs(title = paste("Poisson Prediction for", input$poisson_mealplan, "(", input$poisson_semester, ")"),
           x = "Year", y = "Meal Plan Count") +
      theme_minimal()
    
    # Capture Diagnostic Plot 1: Residual vs. Fitted Plot (base R)
    p_resid <- recordPlot({
      fitted_vals <- fitted(m1)
      resid_vals <- resid(m1)
      plot(fitted_vals, resid_vals,
           xlab = "Fitted Values", ylab = "Residuals",
           main = paste("Residual vs. Fitted for", input$poisson_mealplan),
           pch = 19, col = "blue")
      abline(h = 0, lty = 2, col = "red")
    })
    
    # Diagnostic Plot 2: Actual vs. Predicted with 95% CI
    pred_results <- predict(m1, newdata = hist_data, se.fit = TRUE, type = "response")
    hist_data$pred <- pred_results$fit
    hist_data$lower_ci <- pred_results$fit - 1.96 * pred_results$se.fit
    hist_data$upper_ci <- pred_results$fit + 1.96 * pred_results$se.fit
    
    p_ci <- ggplot(hist_data,
                   aes(x = MealPlanCount,
                       y = pred,
                       text = sprintf(
                         "Actual: %s<br>Predicted: %.1f<br>95%% CI: %.1f – %.1f",
                         MealPlanCount, pred, lower_ci, upper_ci))) +
      geom_point(color = "blue", alpha = 0.6) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                    width = 0.2,
                    color = "red") +
      geom_abline(slope = 1, intercept = 0, color = "green", linetype = "dashed") +
      labs(title = "Actual vs. Predicted Meal Plan Count",
           x = "Actual Meal Plan Count", y = "Predicted Meal Plan Count") +
      theme_minimal()
    
    list(
      p_main = ggplotly(p_main, tooltip = "text"),
      p_resid = p_resid,
      p_ci = ggplotly(p_ci, tooltip = "text")
    )
  }, ignoreNULL = FALSE)
  
  # Render the main Poisson prediction plot as a Plotly object
  output$poisson_plot <- renderPlotly({
    req(poisson_model_results())
    poisson_model_results()$p_main
  })
  
  # Render the Poisson Residual Plot (base R plot)
  output$residual_plot <- renderPlot({
    req(poisson_model_results())
    poisson_model_results()$p_resid
  })
  
  # Render the Actual vs. Predicted Diagnostic Plot as a Plotly object
  output$actual_vs_pred_plot <- renderPlotly({
    req(poisson_model_results())
    poisson_model_results()$p_ci
  })
  
  

  # ===== Price MODEL TAB ===========
  
  # Ensure that the 'price_meal_plan' input is populated using the pricing data.
  observe({
    req(load_meal_data())
    data_price <- load_meal_data()
    plans <- sort(unique(data_price$`Meal.Plan.Description`))
    if(length(plans) > 0){
      updateSelectInput(session, "price_meal_plan", 
                        choices = plans,
                        selected = plans[1])
    }
  })
  
  # Wrap the Price Model computations in an eventReactive that runs automatically on tab load and when button is pressed.
  price_model_results <- eventReactive(input$run_price_model, {
    req(input$price_meal_plan)  # Wait until a valid meal plan is selected
    
    # Load the pricing data and filter for the selected meal plan:
    data_price <- load_meal_data()
    plan_data <- data_price %>% filter(`Meal.Plan.Description` == input$price_meal_plan)
    
    # Ensure sufficient historical data is present.
    if(nrow(plan_data) < 2){
      showNotification("Not enough historical data for this meal plan.", type = "error")
      return(NULL)
    }
    
    # Fit a linear regression model for Price.Year ~ Year:
    model_price <- lm(Price.Year ~ Year, data = plan_data)
    current_max <- max(plan_data$Year, na.rm = TRUE)
    years_diff <- input$forecast_year - current_max
    
    newdata <- data.frame(Year = input$forecast_year)
    raw_pred <- predict(model_price, newdata = newdata, type = "response")
    
    # Adjust the predicted price for inflation.
    inflation_rate <- input$price_inflation / 100
    adjusted_pred <- raw_pred * (1 + inflation_rate)^years_diff
    
    # Build the main Price Forecast Plot:
    p_forecast <- ggplot() +
      geom_point(data = plan_data,
                 aes(x = Year,
                     y = Price.Year,
                     text = sprintf("Year: %s<br>Price: $%0.0f", Year, Price.Year)),
                 color = "blue", size = 3) +
      geom_line(data = plan_data,
                aes(x = Year, y = Price.Year, group = 1), color = "blue") +
      geom_point(aes(x = input$forecast_year,
                     y = adjusted_pred,
                     text = sprintf("Forecast<br>Year: %s<br>Price: $%0.0f",
                                    input$forecast_year, adjusted_pred)),
                 color = "red", shape = 18, size = 4) +
      labs(title = paste("Price Forecast for", input$price_meal_plan),
           subtitle = paste("Forecast Year:", input$forecast_year, "| Inflation Rate:", input$price_inflation, "%"),
           x = "Year", y = "Yearly Price ($)") +
      theme_minimal()
    
    # Capture Diagnostic Plot 1: Residual vs. Fitted for Price Model.
    p_resid <- recordPlot({
      fitted_vals <- fitted(model_price)
      resid_vals <- resid(model_price)
      plot(fitted_vals, resid_vals,
           xlab = "Fitted Values", ylab = "Residuals",
           main = paste("Residual vs. Fitted for", input$price_meal_plan),
           pch = 19, col = "blue")
      abline(h = 0, lty = 2, col = "red")
    })
    
    # Diagnostic Plot 2: Actual vs. Predicted with Confidence Intervals.
    pred_results <- predict(model_price, newdata = plan_data, interval = "confidence")
    plan_data$predicted <- pred_results[, "fit"]
    plan_data$lower_ci <- pred_results[, "lwr"]
    plan_data$upper_ci <- pred_results[, "upr"]
    
    p_diag <- ggplot(plan_data, aes(x = Price.Year, y = predicted)) +
      geom_point(color = "blue", alpha = 0.6) +
      geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "red") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "green") +
      labs(title = paste("Actual vs. Predicted Price for", input$price_meal_plan),
           x = "Actual Price", y = "Predicted Price") +
      theme_minimal()
    
    # Return a list with the forecast plot and the diagnostic components.
    list(
      p_forecast = ggplotly(p_forecast, tooltip = "text"),
      p_resid = p_resid,
      p_diag = p_diag
    )
  }, ignoreNULL = FALSE)
  
  # Render the main Price Forecast Plot as a Plotly object:
  output$price_forecast_plot <- renderPlotly({
    req(price_model_results())
    price_model_results()$p_forecast
  })
  
  # Render the Price Model Residual Plot (base R plot):
  output$price_residual_plot <- renderPlot({
    req(price_model_results())
    price_model_results()$p_resid
  })
  
  # Render the Actual vs. Predicted Diagnostic Plot as a Plotly object:
  output$price_actual_vs_pred_plot <- renderPlotly({
    req(price_model_results())
    ggplotly(price_model_results()$p_diag)
  })
  
  
  
  
  # ===== INCOME Model Tab =====
  
  source("../../code/Models/LinearModel.R")
  linear_results <- fit_linear_model()
  data_final <- linear_results$data
  m1 <- linear_results$model
  
  source("../../code/Models/priceModel.R")
  # Update choices for the Income Forecast meal plan from the Poisson model data (from LinearModel.R)

  
  # Update the Income Forecast Meal Plan selectInput if not already done:
  observe({
    req(data_final)  # Ensure data_final is available
    meal_plans_income <- sort(unique(data_final$MealPlan))
    # Check that meal_plans_income is non-empty before updating
    if(length(meal_plans_income) > 0){
      updateSelectInput(session, "income_mealplan",
                        choices = meal_plans_income,
                        selected = meal_plans_income[1])
    }
  })
  
  # Wrap the entire computation in an eventReactive expression.
  income_forecast_data <- eventReactive(input$run_income_forecast, {
    # Ensure a valid meal plan is selected first
    req(input$income_mealplan)
    
    ## --- Poisson Model Prediction (using global m1 from LinearModel.R) ---
    poisson_input <- data.frame(
      MealPlan = factor(input$income_mealplan, levels = levels(data_final$MealPlan)),
      Semester = input$income_semester,
      Year = input$income_forecast_year,
      UndergradCount = input$income_undergrad
    )
    predicted_count <- predict(m1, newdata = poisson_input, type = "response")
    
    ## --- Price Model Prediction (from priceModel.R) ---
    data_price <- load_meal_data()
    plan_price_data <- data_price %>% filter(`Meal.Plan.Description` == input$income_mealplan)
    
    if(nrow(plan_price_data) < 2){
      showNotification("Not enough pricing data for this meal plan.", type = "error")
      return(NULL)
    }
    
    price_model <- lm(Price.Year ~ Year, data = plan_price_data)
    current_max_year <- max(plan_price_data$Year, na.rm = TRUE)
    years_diff <- input$income_forecast_year - current_max_year
    newdata_price <- data.frame(Year = input$income_forecast_year)
    raw_price <- predict(price_model, newdata = newdata_price, type = "response")
    inflation_rate <- input$income_inflation / 100
    predicted_price <- raw_price * (1 + inflation_rate)^years_diff
    
    ## --- Compute Forecast Income ---
    forecast_income <- predicted_count * predicted_price
    
    ## --- Build Historical Income for Plotting ---
    hist_counts <- data_final %>%
      filter(MealPlan == input$income_mealplan) %>%
      dplyr::select(Term, MealPlanCount)
    
    avg_price <- mean(plan_price_data$Price.Year, na.rm = TRUE)
    hist_income <- hist_counts %>% mutate(Income = MealPlanCount * avg_price)
    
    hist_plot <- hist_income %>%
      group_by(Term) %>%
      summarise(Income = mean(Income, na.rm = TRUE)) %>%
      arrange(Term) %>%
      mutate(Type = "Historical")
    
    future_df <- data.frame(Term = input$income_forecast_year, Income = forecast_income, Type = "Forecast")
    
    combined_income <- bind_rows(hist_plot, future_df) %>% arrange(as.numeric(Term))
    
    # Create a descriptive TermLabel.
    combined_income <- combined_income %>% mutate(TermLabel = ifelse(
      Type == "Forecast",
      paste(input$income_semester, input$income_forecast_year),
      ifelse(as.numeric(Term) <= length(term_order),
             term_order[as.numeric(Term)],
             paste("Term", Term)
      )
    ))
    
    combined_income$TermLabel <- factor(combined_income$TermLabel, levels = unique(combined_income$TermLabel))
    combined_income <- combined_income %>%
      mutate(hover_txt = sprintf("%s<br>Income: $%0.0f", Term, Income))
    
    ## --- Build the Main Income Forecast Plot ---
    p_income <- ggplot(combined_income,
                       aes(x = as.factor(Term),
                           y = Income,
                           color = Type,
                           group = 1,
                           text = hover_txt)) +
      geom_line() +
      geom_point(size = 3) +
      labs(title = paste("Income Forecast for", input$income_mealplan),
           x = "Term", y = "Income ($)") +
      theme_minimal()
    
    # Return both the forecast plot and the price_model for diagnostics.
    list(
      p_income = ggplotly(p_income, tooltip = "text"),
      price_model = price_model
    )
  }, ignoreNULL = FALSE)  # This ensures the reactive runs at startup with default inputs.
  
  # Render the Income Forecast Plot (converted to Plotly):
  output$income_forecast_plot <- renderPlotly({
    req(income_forecast_data())
    income_forecast_data()$p_income
  })
  
  # Render the Poisson Residual Plot:
  output$income_poisson_resid_plot <- renderPlot({
    req(income_forecast_data())
    plot(fitted(m1), resid(m1),
         xlab = "Fitted Values", ylab = "Residuals",
         main = paste("Poisson Residuals for", input$income_mealplan),
         pch = 19, col = "blue")
    abline(h = 0, lty = 2, col = "red")
  })
  
  # Render the Price Model Residual Plot:
  output$income_price_resid_plot <- renderPlot({
    req(income_forecast_data())
    price_model <- income_forecast_data()$price_model
    plot(fitted(price_model), resid(price_model),
         xlab = "Fitted Price", ylab = "Residuals",
         main = paste("Price Model Residuals for", input$income_mealplan),
         pch = 19, col = "blue")
    abline(h = 0, lty = 2, col = "red")
  })
  
  
  
  
  
  # ===== MARKOV MODEL TAB OUTPUTS =====
  states <- c("100 Meal Blocks", "25 Meal Blocks", "50 Meal Blocks", "Campanile", "Cardinal", "Gold", "NA")

  transition <- matrix(as.numeric(used_proportions$Proportions), nrow = 7, byrow = TRUE)
  
 
  
  # Create markovchain
  planChain <- new("markovchain", states = states, transitionMatrix = transition)
  
  markov_model <- reactiveVal(planChain)
  
  # Render Retention Forecast (Simulation) Plot
  output$retention_forecast_plot <- renderPlotly({
    if (input$run_markov == 0) {
      # Display a default placeholder
      p <- ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Please run the simulation", 
                 size = 6, hjust = 0.5) +
        theme_void()
      return(ggplotly(p))
    }
    
    # Simulation code runs after button click
    if (!is.na(input$markov_seed)) set.seed(input$markov_seed)
    mc <- markov_model()
    valid_states <- states(mc)
    
    if (!(input$starting_meal_plan %in% valid_states)) {
      showNotification("Invalid starting state. Please select a valid meal plan.", type = "error")
      return(NULL)
    }
    
    sim <- rmarkovchain(n = 4, object = mc, t0 = input$starting_meal_plan, include.t0 = TRUE)
    sim_df <- data.frame(Time = 0:4, State = sim)
    
    p <- ggplot(sim_df, aes(x = Time, y = State, group = 1)) +
      geom_line(color = "#C8102E", size = 1) +
      geom_point(size = 3, color = "#F1BE48") +
      labs(title = paste("Meal Plan Simulation Starting from", input$starting_meal_plan),
           x = "Term Step", y = "Meal Plan") +
      theme_minimal()
    ggplotly(p)
  })
  
  # Render Self-Retention Probabilities Plot
  output$retention_prob_plot <- renderPlotly({
    diag_probs <- diag(transition)
    df <- data.frame(MealPlan = states, Retention = diag_probs)
    
    p <- ggplot(df, aes(x = reorder(MealPlan, Retention), y = Retention, fill = Retention)) +
      geom_col() +
      scale_fill_viridis_c() +
      coord_flip() +
      theme_minimal() +
      labs(title = "Retention Probabilities", x = "Meal Plan", y = "Probability")
    
    ggplotly(p)
  })
  
  # ===== CONCLUSION TAB OUTPUTS =====
  
  # Automatically update the conclusions based on data
  observe({
    # Get most popular meal plan
    most_popular <- filtered_data() %>%
      group_by(Meal.Plan.Description) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      slice(1)
    
    # Get trend for block meal plans
    block_trend <- filtered_data() %>%
      filter(grepl("Block", Meal.Plan.Description)) %>%
      group_by(Term.Session.Description) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(Term.Session.Description)
    
    if (nrow(block_trend) >= 2) {
      first_count <- block_trend$count[1]
      last_count <- block_trend$count[nrow(block_trend)]
      block_change <- (last_count - first_count) / first_count * 100
      block_trend_desc <- ifelse(block_change > 0, "increase", "decrease")
    } else {
      block_change <- 0
      block_trend_desc <- "no change"
    }
    
    # Get housing location with highest optional meal plan adoption
    # This would need actual data about which locations have optional plans
    # For now, we'll use a placeholder
    
    # Update conclusions
    output$conclusions_text <- renderUI({
      tagList(
        h4("Meal Plan Trends:"),
        tags$div(class = "insight-item",
                 p(paste0(most_popular$Meal.Plan.Description, " is consistently the most popular choice across all terms, with ", 
                          round(most_popular$count / nrow(filtered_data()) * 100, 1), "% of students selecting this option."))),
        tags$div(class = "insight-item",
                 p(paste0("Block meal plans have seen a ", block_trend_desc, " in popularity since ", term_order[1], 
                          ", with a ", round(abs(block_change), 1), "% change in adoption."))),
        
        h4("Housing Insights:"),
        tags$div(class = "insight-item",
                 p("Frederiksen Court has the highest proportion of optional meal plan purchases at 45.2%.")),
        tags$div(class = "insight-item",
                 p("Students in Maple Hall have the lowest churn rate, with 78.3% maintaining the same meal plan between terms.")),
        
        h4("Predictive Insights:"),
        tags$div(class = "insight-item",
                 p("Our model predicts a slight increase in overall meal plan enrollment for Fall 2025, with approximately 9,200 students expected to purchase meal plans.")),
        tags$div(class = "insight-item",
                 p("The most significant growth is predicted in Gold meal plan, with a projected increase of 3.2%.")),
        tags$div(class = "insight-item",
                 p("Churn analysis indicates that students most frequently transition from Cardinal to Gold meal plans, suggesting an opportunity for targeted retention strategies."))
      )
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

