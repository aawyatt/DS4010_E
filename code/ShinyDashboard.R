# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(DT)
library(shinydashboard)

# Define term order globally
term_order <- c(
  "Fall 2021", "Spring 2022",
  "Fall 2022", "Spring 2023",
  "Fall 2023", "Spring 2024",
  "Fall 2024", "Spring 2025"
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Housing & Meal Plan Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Meal Plans", tabName = "meal_plans", icon = icon("utensils")),
      menuItem("Housing", tabName = "housing", icon = icon("home")),
      menuItem("Price Analysis", tabName = "prices", icon = icon("dollar-sign"))
    ),
    
    # Global filters
    selectInput("term_filter", "Select Term:",
                choices = term_order,
                multiple = TRUE,
                selected = term_order[1]),
    
    checkboxInput("combine_blocks", "Combine Block Meal Plans", value = TRUE)
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_students_box"),
                valueBoxOutput("total_meal_plans_box"),
                valueBoxOutput("avg_price_box")
              ),
              fluidRow(
                box(
                  title = "Student Distribution Over Time",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 8,
                  plotOutput("student_trend_plot")
                ),
                box(
                  title = "Quick Stats",
                  status = "info",
                  solidHeader = TRUE,
                  width = 4,
                  tableOutput("quick_stats")
                )
              )
      ),
      
      # Meal Plans Tab
      tabItem(tabName = "meal_plans",
              fluidRow(
                box(
                  title = "Meal Plan Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("meal_plan_dist")
                ),
                box(
                  title = "Top 5 Meal Plans",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("top_meal_plans")
                )
              ),
              fluidRow(
                box(
                  title = "Meal Plan Trends",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  plotOutput("meal_plan_trends")
                )
              ),
              fluidRow(
                box(
                  title = "Detailed Meal Plan Data",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  DTOutput("meal_plan_table")
                )
              )
      ),
      
      # Housing Tab
      tabItem(tabName = "housing",
              fluidRow(
                box(
                  title = "Housing Location Distribution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("housing_dist")
                ),
                box(
                  title = "Entry Status Distribution",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("entry_status_dist")
                )
              ),
              fluidRow(
                box(
                  title = "Housing Trends",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  plotOutput("housing_trends")
                )
              )
      ),
      
      # Price Analysis Tab
      tabItem(tabName = "prices",
              fluidRow(
                box(
                  title = "Price Distribution by Meal Plan",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("price_dist")
                ),
                box(
                  title = "Price vs Popularity",
                  status = "info",
                  solidHeader = TRUE,
                  width = 6,
                  plotOutput("price_popularity")
                )
              ),
              fluidRow(
                box(
                  title = "Price Trends Over Time",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  plotOutput("price_trends")
                )
              ),
              fluidRow(
                box(
                  title = "Meal Plan Price Summary",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  DTOutput("price_summary_table")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive dataset for general data
  filtered_data <- reactive({
    data <- combined_data
    
    if (!is.null(input$term_filter)) {
      data <- data %>% filter(Term.Session.Description %in% input$term_filter)
    }
    
    if (input$combine_blocks) {
      data <- data %>%
        mutate(
          Meal.Plan.Description = case_when(
            grepl("block|blocks", tolower(Meal.Plan.Description)) ~ "Block Meal Plan",
            TRUE ~ Meal.Plan.Description
          )
        )
    }
    
    data <- data %>%
      mutate(Term.Session.Description = factor(Term.Session.Description, 
                                               levels = term_order))
    
    data
  })
  
  # Reactive dataset for price data
  filtered_price_data <- reactive({
    data <- price_data
    
    if (!is.null(input$term_filter)) {
      data <- data %>% filter(Term.Session.Description %in% input$term_filter)
    }
    
    if (input$combine_blocks) {
      data <- data %>%
        mutate(
          Meal.Plan.Description = case_when(
            grepl("block|blocks", tolower(Meal.Plan.Description)) ~ "Block Meal Plan",
            TRUE ~ Meal.Plan.Description
          )
        )
    }
    
    data <- data %>%
      mutate(Term.Session.Description = factor(Term.Session.Description, 
                                               levels = term_order))
    
    data
  })
  
  # Overview Tab Outputs
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
      "Different Meal Plans",
      icon = icon("utensils"),
      color = "green"
    )
  })
  
  output$avg_price_box <- renderValueBox({
    avg_price <- mean(filtered_price_data()$Price.Semester, na.rm = TRUE)
    valueBox(
      dollar(avg_price),
      "Average Semester Price",
      icon = icon("dollar-sign"),
      color = "red"
    )
  })
  
  # Quick Stats Table
  output$quick_stats <- renderTable({
    filtered_data() %>%
      summarise(
        "Total Students" = n_distinct(ID),
        "Most Common Plan" = names(which.max(table(Meal.Plan.Description)))
      )
  })
  
  # Student Trend Plot
  output$student_trend_plot <- renderPlot({
    filtered_data() %>%
      group_by(Term.Session.Description) %>%
      summarise(n_students = n_distinct(ID)) %>%
      ggplot(aes(x = Term.Session.Description, y = n_students, group = 1)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Term", y = "Number of Students")
  })
  
  # Meal Plan Distribution
  output$meal_plan_dist <- renderPlot({
    filtered_data() %>%
      group_by(Meal.Plan.Description) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = reorder(Meal.Plan.Description, count), y = count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Meal Plan", y = "Number of Students")
  })
  
  # Top 5 Meal Plans
  output$top_meal_plans <- renderPlot({
    filtered_data() %>%
      group_by(Meal.Plan.Description) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(5) %>%
      ggplot(aes(x = reorder(Meal.Plan.Description, count), y = count)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Meal Plan", y = "Number of Students",
           title = "Top 5 Most Popular Meal Plans")
  })
  
  # Meal Plan Trends
  output$meal_plan_trends <- renderPlot({
    filtered_data() %>%
      group_by(Term.Session.Description, Meal.Plan.Description) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(Term.Session.Description) %>%
      mutate(percentage = count / sum(count) * 100) %>%
      ggplot(aes(x = Term.Session.Description, 
                 y = percentage, 
                 color = Meal.Plan.Description, 
                 group = Meal.Plan.Description)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom") +
      labs(x = "Term", y = "Percentage of Students (%)")
  })
  
  # Meal Plan Table
  output$meal_plan_table <- renderDT({
    filtered_data() %>%
      group_by(Term.Session.Description, Meal.Plan.Description) %>%
      summarise(
        Count = n(),
        `Percentage` = round(n() / nrow(filtered_data()) * 100, 2),
        .groups = 'drop'
      ) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # Housing Distribution
  output$housing_dist <- renderPlot({
    filtered_data() %>%
      group_by(Room.Location.Description) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(Room.Location.Description, count), y = count)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Location", y = "Number of Students",
           title = "Top 10 Housing Locations")
  })
  
  # Entry Status Distribution
  output$entry_status_dist <- renderPlot({
    filtered_data() %>%
      group_by(Entry.Status.Description) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = reorder(Entry.Status.Description, count), y = count)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Entry Status", y = "Number of Students")
  })
  
  # Housing Trends
  output$housing_trends <- renderPlot({
    filtered_data() %>%
      group_by(Term.Session.Description, Room.Location.Description) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(Term.Session.Description) %>%
      mutate(percentage = count / sum(count) * 100) %>%
      # Keep only top 5 locations for clarity
      group_by(Room.Location.Description) %>%
      mutate(total_count = sum(count)) %>%
      ungroup() %>%
      filter(Room.Location.Description %in% 
               (arrange(distinct(., Room.Location.Description, total_count), 
                        desc(total_count))$Room.Location.Description[1:5])) %>%
      ggplot(aes(x = Term.Session.Description, 
                 y = percentage, 
                 color = Room.Location.Description, 
                 group = Room.Location.Description)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom") +
      labs(x = "Term", 
           y = "Percentage of Students (%)",
           color = "Housing Location") +
      scale_color_brewer(palette = "Set2")
  })
  
  # Price Distribution
  output$price_dist <- renderPlot({
    filtered_price_data() %>%
      filter(!is.na(Price.Year)) %>%
      ggplot(aes(x = reorder(Meal.Plan.Description, Price.Year, FUN = median),
                 y = Price.Year)) +
      geom_boxplot(fill = "lightblue") +
      coord_flip() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Meal Plan Price Distribution",
           x = "Meal Plan", 
           y = "Yearly Price ($)") +
      scale_y_continuous(labels = dollar_format())
  })
  
  # Price vs Popularity
  output$price_popularity <- renderPlot({
    filtered_price_data() %>%
      filter(!is.na(Price.Year)) %>%
      group_by(Meal.Plan.Description) %>%
      summarise(
        avg_price = mean(Price.Year, na.rm = TRUE),
        count = n()
      ) %>%
      ggplot(aes(x = avg_price, y = count)) +
      geom_point(aes(size = count), alpha = 0.6) +
      geom_text(aes(label = Meal.Plan.Description),
                hjust = -0.1,
                size = 3) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(title = "Meal Plan Price vs Popularity",
           x = "Average Yearly Price ($)", 
           y = "Number of Students",
           size = "Number of Students") +
      scale_x_continuous(labels = dollar_format())
  })
  
  # Price Trends
  output$price_trends <- renderPlot({
    filtered_price_data() %>%
      filter(!is.na(Price.Semester)) %>%
      group_by(Term.Session.Description, Meal.Plan.Description) %>%
      summarise(
        avg_price = mean(Price.Semester, na.rm = TRUE),
        n_students = n(),
        .groups = 'drop'
      ) %>%
      ggplot(aes(x = Term.Session.Description, 
                 y = avg_price, 
                 color = Meal.Plan.Description,
                 group = Meal.Plan.Description)) +
      geom_line() +
      geom_point(aes(size = n_students)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom") +
      labs(title = "Meal Plan Price Trends Over Time",
           x = "Term", 
           y = "Average Semester Price ($)",
           size = "Number of Students") +
      scale_y_continuous(labels = dollar_format())
  })
  
  # Price Summary Table
  output$price_summary_table <- renderDT({
    filtered_price_data() %>%
      group_by(Meal.Plan.Description) %>%
      summarise(
        `Average Yearly Price` = mean(Price.Year, na.rm = TRUE),
        `Average Semester Price` = mean(Price.Semester, na.rm = TRUE),
        `Min Yearly Price` = min(Price.Year, na.rm = TRUE),
        `Max Yearly Price` = max(Price.Year, na.rm = TRUE),
        `Number of Students` = n(),
        `Terms Available` = n_distinct(Term.Session.Description)
      ) %>%
      arrange(desc(`Number of Students`)) %>%
      mutate(across(contains("Price"), ~dollar_format()(.))) %>%
      datatable(options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)