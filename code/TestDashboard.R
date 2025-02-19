library("tidyverse")
library("shiny")

theme_set(theme_bw())

combined <- read.csv("./data_folder/transformed_data/Combined_Data.csv", stringsAsFactors = FALSE)

# Convert Term.Session.Description to numeric representation (e.g., Fall 2023 -> 2023.75)
combined$Term.Numeric <- case_when(
  grepl("Fall", combined$Term.Session.Description) ~ as.numeric(gsub("Fall ", "", combined$Term.Session.Description)) + 0.75,
  grepl("Spring", combined$Term.Session.Description) ~ as.numeric(gsub("Spring ", "", combined$Term.Session.Description)) + 0.25,
  TRUE ~ as.numeric(gsub("[^0-9]", "", combined$Term.Session.Description))
)

ui <- fluidPage(
  titlePanel("Dining Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "X Axis Variable",
                  choices = c("Meal.Plan.Description", "Room.Location.Description"),
                  selected = "Meal.Plan.Description"),
      
      selectInput("Room.Location.Description", "Residence Hall",
                  choices = unique(combined$Room.Location.Description),
                  selected = unique(combined$Room.Location.Description)[1],
                  multiple = TRUE),
      
      sliderInput("time_range", "Select Term Range",
                  min = min(combined$Term.Numeric, na.rm = TRUE),
                  max = max(combined$Term.Numeric, na.rm = TRUE),
                  value = range(combined$Term.Numeric, na.rm = TRUE),
                  step = 0.25)
    ),
    
    mainPanel(
      plotOutput("dining_plot")
    )
  )
)

server <- function(input, output) {
  dining_data <- reactive({
    combined %>%
      filter(Room.Location.Description %in% input$Room.Location.Description,
             Term.Numeric >= input$time_range[1], Term.Numeric <= input$time_range[2])
  })
  
  plot_dining <- reactive({
    ggplot(dining_data(), aes_string(x = input$xvar)) +
      geom_bar(fill = "steelblue", color = "black") +
      theme_minimal() +
      labs(x = input$xvar, y = "Count", title = "Dining Data Distribution")
  })
  
  output$dining_plot <- renderPlot(plot_dining())
}

shinyApp(ui, server)

