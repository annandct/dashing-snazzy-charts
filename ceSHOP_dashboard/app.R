# Load libraries for Shiny, data manipulation, plotting, interactivity, and label formatting
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)

# --- 2. GLOBAL SCRIPT: DATA & THEME (Runs Once) ---

# --- Data Generation (User-Provided) ---
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2025-09-26")
course_names <- c(
  "75-Hr. PA Sales Pre-Licensing Course Only Package",
  "20-Hour Mortage Broker Education",
  "Appraisers State Exam",
  "Outsmartimg the 20215 Housing Market",
  "Real Estate Introduction",
  "Rental vs Sales - Defining Your Path",
  "Transition from Home Real Estate to Commerical Real Estate"
)
promotions <- c("30% Off", "40% Off", "50% Off", "None")
promo_probabilities <- c(0.25, 0.20, 0.15, 0.40)
platforms <- c("Email", "LinkedIn", "Facebook", "Conference", "Direct")
platform_probabilities <- c(0.30, 0.20, 0.20, 0.10, 0.20)
date_sequence <- seq(from = start_date, to = end_date, by = "day")
number_of_records <- length(date_sequence) * 5

sales_data <- tibble(
  Date = sample(date_sequence, number_of_records, replace = TRUE),
  Product_Name = sample(course_names, number_of_records, replace = TRUE),
  Promotion_Applied = sample(promotions, number_of_records, replace = TRUE, prob = promo_probabilities),
  Professions = sample(c("Real Estate Agent", "Mortgage Broker", "Appraiser", "Other"), number_of_records, replace = TRUE),
  Line_of_Business = sample(c("Commerical", "Residential", "Private", "Government", "Other"), number_of_records, replace = TRUE),
  Traffic_on_Site = sample(100:1000, number_of_records, replace = TRUE),
  Status = sample(c("Pre-Licensing", "Continuing Education", "Other"), prob = c(.45, .45, .1), number_of_records, replace = TRUE),
  Campaign_Platform = sample(platforms, number_of_records, replace = TRUE, prob = platform_probabilities),
  Tactic_Testing = sample(c("A", "B", "C"), number_of_records, replace = TRUE)
) %>%
  mutate(
    Promotion_Rate = ifelse(Promotion_Applied == "None", 0,
                            as.numeric(gsub(x = Promotion_Applied, pattern = "% Off", "")) / 100
    ),
    Course_Cost = case_when(
      Product_Name == "75-Hr. PA Sales Pre-Licensing Course Only Package" ~ 7500,
      Product_Name == "20-Hour Mortage Broker Education" ~ 2000,
      Product_Name == "Appraisers State Exam" ~ 750,
      Product_Name == "Outsmartimg the 20215 Housing Market" ~ 1000,
      Product_Name == "Real Estate Introduction" ~ 1500,
      Product_Name == "Rental vs Sales - Defining Your Path" ~ 200,
      Product_Name == "Transition from Home Real Estate to Commerical Real Estate" ~ 750,
      TRUE ~ 0
    ),
    Cost_Yeild = round(Course_Cost * Promotion_Rate, 2),
    Revenue = Course_Cost * (1 - Promotion_Rate)
  )

# --- Themeing (User-Provided) ---
.base_colors <- list(
  "primary_blues" = c("#005287", "#00354e", "#0278af", "#5a5c5d", "#c1c2c4"),
  "primary_reds" = c("#c42032", "#850101", "#6D2B2C", "#614041", "#555556", "#292323"),
  "combined_colors" = c("#005287", "#c42032", "#00354e", "#850101", "#0278af", "#6D2B2C", "#c1c2c4", "#5a5c5d", "#685786", "#ffcc05", "#0278af", "#00354e")
)

theme_cta_resize <- function() {
  theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(family = "Arial", face = "bold", size = rel(1.25), color = "#005287", hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Arial", size = rel(1), color = "#5a5c5d", hjust = 0.5, margin = margin(b = 10)),
      axis.title = element_text(family = "Arial", size = rel(.9), color = "#005287", face = "bold"),
      axis.text = element_text(family = "Arial", size = rel(.75), color = "#5a5c5d"),
      legend.title = element_text(family = "Arial", size = rel(.75), color = "#005287", face = "bold"),
      legend.text = element_text(family = "Arial", size = rel(.65), color = "#5a5c5d"),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid.major = element_line(color = "#c1c2c4"),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#0278af", color = "#005287"),
      strip.text = element_text(family = "Arial", size = rel(.9), color = "#ffffff", face = "bold")
    )
}

# --- Data Pre-Processing for App Performance ---

# Pre-aggregate daily data for charts g5 and g6
daily_data <- sales_data %>%
  group_by(Date) %>%
  summarise(
    Total_Revenue = sum(Revenue),
    Total_Traffic = sum(Traffic_on_Site)
  )

# Pre-build the static g3 plot
g3_static <- sales_data %>%
  count(Campaign_Platform) %>%
  ggplot(aes(x = reorder(Campaign_Platform, -n), y = n, fill = Campaign_Platform)) +
  geom_col() +
  geom_text(aes(label = comma(n)), vjust = -0.5, color = "#00354e", size = 3.5) +
  labs(
    title = "Sales Volume by Campaign Platform",
    subtitle = "Total number of sales attributed to each platform",
    x = "Campaign Platform",
    y = "Number of Sales"
  ) +
  scale_y_continuous(labels = comma_format()) +
  scale_fill_manual(values = .base_colors$primary_blues) +
  theme_cta_resize() +
  theme(legend.position = "none")

# Get date range for the slider
min_date <- min(daily_data$Date)
max_date <- max(daily_data$Date)


# --- 3. SHINY USER INTERFACE (UI) ---
ui <- fluidPage(
  titlePanel("Sales Performance Dashboard"),
  
  sidebarLayout(
    # --- Sidebar for Filters ---
    sidebarPanel(
      width = 3,
      h4("Filters"),
      # The date range slider is the primary filter
      sliderInput("dateRangeSlider",
                  label = "Select Date Range:",
                  min = min_date,
                  max = max_date,
                  # Default to the most recent 90 days for a cleaner initial view
                  value = c(max_date - days(90), max_date),
                  timeFormat = "%Y-%m-%d"
      )
    ),
    
    # --- Main Panel for Charts ---
    mainPanel(
      width = 9,
      # Organize charts into tabs
      tabsetPanel(
        type = "tabs",
        
        # --- Tab 1: Daily Performance (g5 and g6) ---
        tabPanel(
          "Daily Performance",
          h3("Revenue vs. Traffic (Interactive)"),
          p("This scatter plot shows the relationship between daily traffic and revenue for the selected period. You can hover, click, and drag to zoom."),
          # Output for plotly scatter plot (g5)
          plotlyOutput("scatterPlot"),
          
          hr(), # Horizontal rule for separation
          
          h3("Daily Revenue Detail (Lollipop)"),
          p("This plot shows the specific revenue for each day in the selected range. For clarity, this chart will only render if the selected range is 60 days or less."),
          # Output for static lollipop plot (g6)
          plotOutput("lollipopPlot")
        ),
        
        # --- Tab 2: Campaign Summary (g3) ---
        tabPanel(
          "Campaign Summary",
          h3("Campaign Platform Performance"),
          p("This chart shows the total sales volume from all campaign platforms over the entire dataset. It is not affected by the date filter."),
          # Output for static campaign plot (g3)
          plotOutput("campaignPlot")
        )
      )
    )
  )
)


# --- 4. SHINY SERVER LOGIC ---
server <- function(input, output) {
  
  # --- 1. Reactive Data ---
  # This expression filters the pre-aggregated daily data based
  # on the user's selection from the `dateRangeSlider`.
  filtered_daily_data <- reactive({
    daily_data %>%
      filter(Date >= input$dateRangeSlider[1] &
               Date <= input$dateRangeSlider[2])
  })
  
  # --- 2. Render Chart g5 (Scatter Plot) ---
  output$scatterPlot <- renderPlotly({
    
    # Use the reactive data `filtered_daily_data()`
    g5_reactive <- filtered_daily_data() %>%
      ggplot(aes(x = Total_Traffic, y = Total_Revenue, text = paste(
        "Date: ", Date,
        "<br>Traffic: ", comma(Total_Traffic),
        "<br>Revenue: ", dollar(Total_Revenue)
      ))) +
      geom_point(color = .base_colors$primary_blues[3], alpha = 0.6) +
      labs(
        title = "Daily Revenue vs. Daily Traffic",
        subtitle = "Each point represents one day in the selected range",
        x = "Total Daily Traffic on Site",
        y = "Total Daily Revenue"
      ) +
      scale_x_continuous(labels = comma_format()) +
      scale_y_continuous(labels = dollar_format()) +
      theme_cta_resize()
    
    # Convert to an interactive plotly object
    ggplotly(g5_reactive, tooltip = "text")
  })
  
  # --- 3. Render Chart g6 (Lollipop Plot) ---
  output$lollipopPlot <- renderPlot({
    
    # Get the reactive data
    plot_data <- filtered_daily_data()
    
    # Ensure data exists before trying to plot
    req(nrow(plot_data) > 0)
    
    # --- UX Guardrail ---
    # A lollipop chart with too many data points is unreadable.
    # We will only render it if the user selects 60 days or fewer.
    if (nrow(plot_data) > 60) {
      # Display a message instead of a crowded plot
      ggplot() +
        annotate("text",
                 x = 1, y = 1, size = 5, color = "#00354e",
                 label = "Please select a smaller date range (<= 60 days)\n to view the Daily Revenue Detail chart."
        ) +
        theme_void()
    } else {
      # If range is acceptable, build the lollipop chart
      plot_data %>%
        ggplot(aes(x = Date, y = Total_Revenue)) +
        geom_segment(
          aes(x = Date, xend = Date, y = 0, yend = Total_Revenue),
          color = .base_colors$primary_blues[4],
          size = 0.75
        ) +
        geom_point(color = .base_colors$primary_blues[1], size = 4) +
        labs(
          title = "Daily Revenue Detail",
          subtitle = "Revenue for each day in the selected range",
          x = "Date",
          y = "Total Revenue"
        ) +
        scale_y_continuous(labels = dollar_format()) +
        scale_x_date(date_labels = "%b %d") +
        theme_cta_resize() +
        theme(panel.grid.major.x = element_blank()) # Remove vertical gridlines
    }
  })
  
  # --- 4. Render Chart g3 (Campaign Plot) ---
  output$campaignPlot <- renderPlot({
    # This plot is static and was pre-built in the global section
    # for performance. We just need to print it.
    print(g3_static)
  })
}


# --- 5. RUN THE SHINY APP ---
shinyApp(ui = ui, server = server)