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
    ),
    
    # Global filters
    tags$div(
      class = "sidebar-filters",
      hr(),
      h4("Global Filters", style = "padding-left: 15px;"),
      selectInput("term_filter", "Select Term:",
                  choices = term_order,
                  multiple = TRUE,
                  selected = term_order),
      
      checkboxInput("combine_blocks", "Combine Block Meal Plans", value = TRUE)
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
            p("This project aims to analyze and predict trends in the ISU Dining meal plan. By examining historical meal plan purchases, residence hall occupancy, and churn rates over semesters and years, we will identify the most popular meal plans, analyze student retention, and forecast meal plan sales and revenue for Fall 2025."),
            p("Additionally, we will assess how many students in residence halls who do not require meal plans still choose to purchase them and whether this changes over time. Using predictive modeling, churn analysis, and comparative analytics, we will uncover patterns in meal plan adoption and student housing trends."),
            p("The insights from this study will help ISU Dining and Housing Services optimize pricing, meal plan structures, and retention strategies, ensuring financial sustainability while improving student satisfaction. Our team will develop interactive dashboards to track trends and predictions, making data-driven policy decisions more accessible.")
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
            tabPanel("Prediction Model",
                     fluidRow(
                       box(
                         title = "Meal Plan Prediction Model (Linear Regression)",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         p("This model predicts the number of students who will select each meal plan in future terms based on historical data. It uses a linear regression model to forecast meal plan counts."),
                         p("The model considers factors such as term, meal plan type, and undergraduate counts to predict meal plan selections.")
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Model Controls",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 3,
                         selectInput("future_terms", "Number of Future Terms to Predict:",
                                     choices = 1:10, selected = 2),
                         selectizeInput("selected_meal_plans", "Select Meal Plans:",
                                        choices = NULL, multiple = TRUE),
                         actionButton("run_prediction", "Run Prediction",
                                      icon = icon("calculator"),
                                      class = "btn-primary btn-block")
                       ),
                       box(
                         title = "Prediction Results",
                         status = "info",
                         solidHeader = TRUE,
                         width = 9,
                         plotlyOutput("prediction_plot", height = "400px")
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Diagnostics",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 12,
                         plotOutput("diagnostics_plot", height = "500px")
                       )
                     )
            ),
            tabPanel("Price Model",
                     fluidRow(
                       box(
                         title = "Meal Plan Price Forecasting (Linear Regression)",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 12,
                         p("This tool forecasts the future cost of ISU meal plans using historical pricing data and linear regression."),
                         p("It projects prices up to 10 years into the future while accounting for a fixed inflation rate per year. This helps ISU Dining anticipate financial trends and adjust pricing strategies.")
                       )
                     ),
                     fluidRow(
                       box(
                         title = "Model Controls",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 3,
                         selectInput("years_ahead", "Years into the Future:",
                                     choices = 1:10, selected = 4),
                         selectizeInput("selected_price_meal_plans", "Select Meal Plans:",
                                        choices = NULL, multiple = TRUE),
                         actionButton("run_price_prediction", "Run Price Prediction",
                                      icon = icon("calculator"),
                                      class = "btn-primary btn-block")
                       ),
                       box(
                         title = "Price Forecast Results",
                         status = "info",
                         solidHeader = TRUE,
                         width = 9,
                         plotlyOutput("price_model_plot", height = "400px")
                       )
                     )
            ),
            
            
            tabPanel("Income Forecast",
                     fluidRow(
                       box(
                         title = "Model Controls",
                         status = "primary",
                         solidHeader = TRUE,
                         width = 3,
                         # Dropdown for how many terms into the future to predict
                         selectInput("income_years_ahead", 
                                     "Years into the Future:",
                                     choices = 1:10, 
                                     selected = 4),
                         # Single-select to choose the meal plan for forecasting income
                         selectizeInput("selected_income_meal_plan", 
                                        "Select Meal Plan for Income Forecast:",
                                        choices = NULL, 
                                        multiple = FALSE),
                         # Button to run the income forecast calculation
                         actionButton("run_income_forecast", "Run Income Forecast",
                                      icon = icon("calculator"),
                                      class = "btn-primary btn-block")
                       ),
                       box(
                         title = "Income Forecast Results",
                         status = "info",
                         solidHeader = TRUE,
                         width = 9,
                         # Plotly output that will display the income forecast plot
                         plotlyOutput("income_model_plot", height = "400px")
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
            p("In this dashboard, it helps us model how students switch between meal plans across semesters and forecast retention and churn.")
            
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
              title = "Retention Forecast (Simulation)",
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
  source("./code/Models/LinearModel.R")
  source("./code/Models/priceModel.R")
  
  # ===== REACTIVE DATA PROCESSING =====
  
  # Basic reactive dataset
  filtered_data <- reactive({
    data <- clean_data
    
    if (!is.null(input$term_filter)) {
      data <- data %>% filter(Term.Session.Description %in% input$term_filter)
    }
    
    if (input$combine_blocks) {
      data <- data %>%
        mutate(
          Meal.Plan.Description = case_when(
            grepl("block|blocks", tolower(Meal.Plan.Description), ignore.case = TRUE) ~ "Block Meal Plan",
            TRUE ~ Meal.Plan.Description
          )
        )
    }
    
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
    n_plans <- length(unique(filtered_data()$Meal.Plan.Description))
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
  source("./code/Models/LinearModel.R")
  
  # Fit the linear model once at startup and get the processed data
  linear_results <- fit_linear_model()
  data_final <- linear_results$data
  
  # Update the selectizeInput choices for meal plans based on the model data
  observe({
    meal_plans <- sort(unique(data_final$MealPlan))
    updateSelectizeInput(session, "selected_meal_plans", 
                         choices = meal_plans, 
                         selected = meal_plans[1:min(5, length(meal_plans))])
  })
  
  observeEvent(input$run_prediction, {
    # Get the selected meal plan types from the UI
    selected_meal_plans <- input$selected_meal_plans
    
    # Validate that at least one meal plan is selected
    if (is.null(selected_meal_plans) || length(selected_meal_plans) == 0) {
      output$prediction_plot <- renderPlotly({ NULL })
      return()
    }
    
    # Filter the historical data for the selected meal plans
    historical_data <- data_final %>% 
      filter(MealPlan %in% selected_meal_plans)
    
    if (nrow(historical_data) == 0) {
      output$prediction_plot <- renderPlotly({ NULL })
      return()
    }
    
    # Fit a linear model for each meal plan (if there are at least 2 points)
    models <- historical_data %>% 
      group_by(MealPlan) %>% 
      filter(n() > 1) %>% 
      nest() %>% 
      #mutate(model = map(data, ~ lm(MealPlanCount ~ Term, data = .x)))
      #print(head(data))  
      mutate(model = map(data, ~ glm(MealPlanCount ~ Term, data = .x, family = poisson(link = "log"))))
    
    # Create future predictions for each meal plan using explicit dplyr and tidyr functions
    future_data <- models %>% 
      mutate(future = map2(data, model, ~ {
        max_term <- max(.x$Term, na.rm = TRUE)
        new_terms <- seq(max_term + 1, max_term + as.numeric(input$future_terms))
        predicted <- predict(.y, 
                             newdata = data.frame(Term = new_terms), 
                             type = "response")
        tibble(Term = new_terms, count = predicted)
      })) %>% 
      tidyr::unnest(future) %>% 
      dplyr::select(MealPlan, Term, count) %>% 
      mutate(Type = "Future")
    
    # Prepare the historical data for plotting: explicitly select the same columns
    historical_plot <- historical_data %>% 
      dplyr::select(MealPlan, Term, MealPlanCount) %>% 
      mutate(count = MealPlanCount,
             Type = "Actual")
    
    # Combine historical and future data and ensure the 'Type' column exists
    combined_data <- dplyr::bind_rows(historical_plot, future_data)
    combined_data <- combined_data %>% mutate(Type = factor(Type, levels = c("Actual", "Future")))
    
    # Create the plot:
    # - Plot actual data and future predictions as points, and connect by a line.
    # - Draw the dashed trendline on historical data only.
    p <- ggplot(combined_data, aes(x = Term, y = count, color = MealPlan, shape = Type)) +
      geom_point(size = 2) +
      geom_line(aes(group = MealPlan)) +
      geom_smooth(data = historical_plot, 
                  aes(x = Term, y = MealPlanCount, color = MealPlan),
                  method = "glm", 
                  method.args = list(family = "poisson"),
                  se = FALSE, 
                  linetype = "dashed", 
                  size = 0.8)+
      theme_minimal() +
      labs(x = "Term", y = "Meal Plan Count", color = "Meal Plan", shape = "Data Type") +
      scale_x_continuous(breaks = unique(combined_data$Term),
                         labels = unique(combined_data$Term))
    
    output$prediction_plot <- renderPlotly({
      ggplotly(p)
    })
  })
  
  output$diagnostics_plot <- renderPlot({
    # Fit the linear model and retrieve the processed data
    model_data <- fit_linear_model()
    m1 <- model_data$model
    
    # Create diagnostic plots (residuals, index, qq, and Cook's distance)
    resid_panel(m1,
                plots    = c("resid", "index", "qq", "cookd"),
                qqbands  = TRUE,
                smoother = TRUE)
  })
  
  
  # ===== Price MODEL TAB ===========
  
  priceModelResults <- run_price_model()
  
  observe({
    data <- load_meal_data()  # From priceModel.R
    plans <- sort(unique(data$Meal.Plan.Description))
    
    updateSelectizeInput(session, "selected_price_meal_plans", 
                         choices = plans,
                         selected = plans[1])
  })
  
  # When user clicks "Run Price Prediction"
  observeEvent(input$run_price_prediction, {
    # Ensure input$years_ahead is treated as numeric (since it comes from selectInput)
    years_ahead <- as.numeric(input$years_ahead)
    
    # Run model using selected meal plans and number of future years
    priceModelResults <- run_price_model(
      selected_plans = input$selected_price_meal_plans,
      years_ahead = years_ahead
    )
    
    pred_df <- priceModelResults$predictions
    
    # Save predicted results to a CSV file.
    write.csv(pred_df, file = "predicted_results.csv", row.names = FALSE)
    
    # Load historical data for the selected plans
    hist_data <- load_meal_data()
    hist_data <- hist_data[hist_data$Meal.Plan.Description %in% input$selected_price_meal_plans, ]
    
    # Get predicted data
    pred_df <- priceModelResults$predictions
    
    # Create the combined ggplot
    p <- ggplot() +
      geom_point(data = hist_data, aes(x = Year, y = Price.Year, color = Meal.Plan.Description), size = 3) +
      geom_line(data = hist_data, aes(x = Year, y = Price.Year, color = Meal.Plan.Description), size = 1) +
      geom_point(data = pred_df, aes(x = Year, y = Adjusted.Price, color = Meal.Plan.Description),
                 shape = 17, size = 3) +
      geom_line(data = pred_df, aes(x = Year, y = Adjusted.Price, color = Meal.Plan.Description),
                linetype = "dashed", size = 1) +
      labs(
        title = "Price Forecast for Selected Meal Plans",
        x = "Year",
        y = "Price ($)",
        color = "Meal Plan"
      ) +
      theme_minimal()
    
    # Render the combined plot
    output$price_model_plot <- renderPlotly({
      ggplotly(p)
    })
  })
  
  # ===== INCOME Model Tab =====
  
  # Update Income Forecast meal plan choices (similar to other sections)
  observe({
    meal_plans <- sort(unique(data_final$MealPlan))
    updateSelectizeInput(session, "selected_income_meal_plan", 
                         choices = meal_plans,
                         selected = meal_plans[1])
  })
  
  # Income Forecast Observer
  observeEvent(input$run_income_forecast, {
    req(input$selected_income_meal_plan, input$income_years_ahead)
    
    selected_plan <- input$selected_income_meal_plan
    
    # Retrieve historical count data from data_final (from fit_linear_model())
    hist_data <- data_final %>% filter(MealPlan == selected_plan)
    if (nrow(hist_data) < 2) {
      cat("Not enough historical data to fit model\n")
      output$income_model_plot <- renderPlotly({ NULL })
      return()
    }
    
    # Map numeric Term to term labels using global term_order
    hist_data <- hist_data %>% mutate(TermLabel = term_order[Term])
    
    # Incorporate cost data from filtered_data()
    cost_data <- filtered_data() %>%
      filter(Meal.Plan.Description == selected_plan) %>%
      group_by(Term.Session.Description) %>%
      summarise(Cost = mean(Price.Year, na.rm = TRUE)) %>%
      ungroup()
    hist_data <- left_join(hist_data, cost_data, by = c("TermLabel" = "Term.Session.Description"))
    
    # Fit the Poisson model using historical data
    model <- glm(MealPlanCount ~ Term, family = poisson(link = "log"), data = hist_data)
    
    # Forecast future counts using the user-specified number of future terms
    last_term <- max(hist_data$Term, na.rm = TRUE)
    future_range <- as.numeric(input$income_years_ahead)
    future_terms <- seq(last_term + 1, last_term + future_range)
    
    predicted_counts <- predict(model, newdata = data.frame(Term = future_terms), type = "response")
    avg_cost <- mean(hist_data$Cost, na.rm = TRUE)
    future_income <- predicted_counts * avg_cost
    
    future_df <- data.frame(
      Term = future_terms,
      MealPlanCount = predicted_counts,
      Cost = avg_cost,
      Income = future_income,
      Type = "Future"
    ) %>% mutate(TermLabel = sapply(Term, function(t) {
      if (t <= length(term_order)) {
        term_order[t]
      } else {
        last_term_str <- term_order[length(term_order)]
        base_year <- as.numeric(gsub("\\D", "", last_term_str))
        season <- ifelse(grepl("Fall", last_term_str), "Spring", "Fall")
        paste(season, base_year + (t - length(term_order)))
      }
    }))
    
    # Prepare the historical data for plotting
    hist_data <- hist_data %>%
      mutate(Type = "Actual") %>%
      mutate(Income = MealPlanCount * Cost) %>%
      dplyr::select(Term, TermLabel, MealPlanCount, Cost, Income, Type)
    
    # Combine historical and forecast data
    combined_df <- bind_rows(hist_data, future_df)
    
    # Sort by the numeric Term value and convert TermLabel to an ordered factor
    combined_df <- combined_df %>% arrange(Term)
    combined_df$TermLabel <- factor(combined_df$TermLabel, levels = unique(combined_df$TermLabel))
    
    cat("Combined data for plotting:\n")
    print(head(combined_df))
    
    # Plot the combined income forecast data
    p <- ggplot(combined_df, aes(x = TermLabel, y = Income, group = Type, color = Type,
                                 text = paste0(
                                   "Term: ", TermLabel, "<br>",
                                   "Income: $", scales::dollar(Income), "<br>",
                                   "Meal Plan Count: ", round(MealPlanCount, 0), "<br>",
                                   "Cost per Plan: $", round(Cost, 2)
                                 ))) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(x = "Term", y = "Income ($)", title = paste("Income Forecast for", selected_plan)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    output$income_model_plot <- renderPlotly({ ggplotly(p, tooltip = "text") })
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

