# --- 1. LOAD ALL LIBRARIES ---
# Load libraries for Shiny, data manipulation, plotting, interactivity, and label formatting
library(shiny)
library(dplyr)
#library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(plotly)

# --- 2. GLOBAL SCRIPT: DATA & THEME (Runs Once) ---
# 
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

# # Data Loading from RDS for speed
# preloaded_data <- readRDS("preloaded_sales_data.rds")
# # Access list elements into environment:
# sales_data <- preloaded_data$sales_data
# daily_data <- preloaded_data$daily_data
# min_date <- preloaded_data$min_date
# max_date <- preloaded_data$max_date
# categories <- preloaded_data$categories
# .base_colors <- preloaded_data$current_theme$base_colors
# theme_cta_resize <- preloaded_data$current_theme$theme_cta_resize


# # --- Data Pre-Processing for App Performance ---
# 
# Pre-aggregate daily data for charts g5 and g6
daily_data <- sales_data %>%
  group_by(Campaign_Platform, Date) %>%
  summarise(
    Total_Revenue = sum(Revenue),
    Total_Traffic = sum(Traffic_on_Site)
  )

# Get date range for the slider
min_date <- min(daily_data$Date)
max_date <- max(daily_data$Date)

# Columns for Categorical Select
categories <- unique(sales_data$Campaign_Platform)

# --- 3. SHINY USER INTERFACE (UI) ---
ui <- fluidPage(
  titlePanel("Comprehensive Sales Dashboard"),
  
  sidebarLayout(
    # --- Sidebar for Global Filter ---
    sidebarPanel(
      width = 3,
      h4("Global Date Filter"),
      # This slider now controls all charts
      sliderInput("dateRangeSlider",
                  label = "Select Date Range:",
                  min = min_date,
                  max = max_date,
                  # Default to the most recent 180 days
                  value = c(max_date - days(180), max_date),
                  timeFormat = "%Y-%m-%d"
      ),
      p("This date range filter applies to all charts in the dashboard."),
      #Category Input
      selectInput(
        "cat_filter",
        label = "Select Category",
        choices = categories,  # Uses unique values from your column
        selected = categories, #starts with ALL values selected
        multiple = TRUE
      ),
      p("Select a Specific Campaign Platform")
    ),
    
    # --- Main Panel for Charts ---
    mainPanel(
      width = 9,
      # Organize charts into tabs
      tabsetPanel(
        type = "tabs",
        
        # --- Tab 1: Revenue Trends (g1, g5) ---
        tabPanel(
          "Revenue Trends",
          h3("Revenue Over Time"),
          p("Monthly aggregated revenue and the daily relationship between traffic and sales."),
          plotlyOutput("monthlyRevenuePlot"), # g1
          hr(),
          plotlyOutput("scatterPlot") # g5
        ),
        
        # --- Tab 2: Product Performance (g2, g4) ---
        tabPanel(
          "Product Performance",
          h3("Revenue by Product"),
          p("Breakdown of total revenue by product and by business segment."),
          plotlyOutput("productRevenuePlot"), # g2
          hr(),
          plotlyOutput("facetedPlot") # g4
        ),
        
        # --- Tab 3: Marketing & Daily Detail (g3, g6) ---
        tabPanel(
          "Marketing & Daily Detail",
          h3("Campaign & Daily Performance"),
          p("Sales volume by marketing platform and a detailed look at daily revenue."),
          plotlyOutput("campaignPlot"), # g3
          hr(),
          plotlyOutput("lollipopPlot") # g6
        )
      )
    )
  )
)


# --- 4. SHINY SERVER LOGIC ---
server <- function(input, output) {
  
  # --- 1. Reactive Data Expressions ---
  
  # Reactive block 1: Filters the RAW sales_data
  # This is used for charts that aggregate from raw sales (g1, g2, g3, g4)
  filtered_sales_data <- reactive({
    sales_data %>%
      filter(Campaign_Platform %in% input$cat_filter,
             Date >= input$dateRangeSlider[1] &
               Date <= input$dateRangeSlider[2])
  })
  
  # Reactive block 2: Filters the PRE-AGGREGATED daily_data
  # This is more efficient for daily charts (g5, g6)
  filtered_daily_data <- reactive({
    daily_data %>%
      filter(Campaign_Platform %in% input$cat_filter,
             Date >= input$dateRangeSlider[1] &
               Date <= input$dateRangeSlider[2])
  })
  
  # --- 2. Render Charts (All as Plotly) ---
  
  # --- Chart g1: Monthly Revenue Over Time ---
  output$monthlyRevenuePlot <- renderPlotly({
    plot_data <- filtered_sales_data() %>%
      mutate(Month = floor_date(Date, "month")) %>%
      group_by(Campaign_Platform, Month) %>%
      summarise(Total_Revenue = sum(Revenue), .groups = 'keep')
    
    req(nrow(plot_data) > 0) # Ensure data exists
    
    g1 <- plot_data %>%
      ggplot(aes(x = Month, y = Total_Revenue, group=Campaign_Platform, color=Campaign_Platform, text = paste(
        "Month: ", format(Month, "%b %Y"),
        "<br>Revenue: ", dollar(Total_Revenue)
      ))) +
      geom_line()+#color = .base_colors$primary_blues[1], lwd = 1) +
      geom_point()+#color = .base_colors$primary_blues[1], lwd = 2) +
      labs(
        title = "Monthly Revenue Over Time",
        subtitle = "Total sales revenue aggregated by month for the selected period",
        x = "Month",
        y = "Total Revenue"
      ) +
      scale_y_continuous(labels = dollar_format()) +
      scale_x_date(date_labels = "%b %Y") +
      scale_color_manual(values = .base_colors$combined_colors)+
      theme_cta_resize()
    
    ggplotly(g1, tooltip = "text")
  })
  
  # --- Chart g2: Total Revenue by Product ---
  output$productRevenuePlot <- renderPlotly({
    plot_data <- filtered_sales_data() %>%
      group_by(Product_Name) %>%
      summarise(Total_Revenue = sum(Revenue))
    
    req(nrow(plot_data) > 0)
    
    g2 <- plot_data %>%
      ggplot(aes(x = Total_Revenue, y = reorder(Product_Name, Total_Revenue), fill = Product_Name,
                 text = paste("Product: ", Product_Name, "<br>Revenue: ", dollar(Total_Revenue)))) +
      geom_col() +
      labs(
        title = "Total Revenue by Product",
        subtitle = "Revenue generated from each course for the selected period",
        x = "Total Revenue",
        y = "Product"
      ) +
      scale_x_continuous(labels = dollar_format()) +
      scale_fill_manual(values = .base_colors$combined_colors) +
      theme_cta_resize() +
      theme(legend.position = "none")
    
    ggplotly(g2, tooltip = "text")
  })
  
  # --- Chart g3: Sales Volume by Campaign Platform ---
  output$campaignPlot <- renderPlotly({
    plot_data <- filtered_sales_data() %>%
      count(Campaign_Platform, name = "Sales_Count")
    
    req(nrow(plot_data) > 0)
    
    g3 <- plot_data %>%
      ggplot(aes(x = reorder(Campaign_Platform, -Sales_Count), y = Sales_Count, fill = Campaign_Platform,
                 text = paste("Platform: ", Campaign_Platform, "<br>Sales: ", comma(Sales_Count)))) +
      geom_col() +
      labs(
        title = "Sales Volume by Campaign Platform",
        subtitle = "Total number of sales from each platform for the selected period",
        x = "Campaign Platform",
        y = "Number of Sales"
      ) +
      scale_y_continuous(labels = comma_format()) +
      scale_fill_manual(values = .base_colors$primary_blues) +
      theme_cta_resize() +
      theme(legend.position = "none")
    
    ggplotly(g3, tooltip = "text")
  })
  
  # --- Chart g4: Revenue by Product and Business Status (Faceted) ---
  output$facetedPlot <- renderPlotly({
    plot_data <- filtered_sales_data() %>%
      group_by(Product_Name, Status) %>%
      summarise(Total_Revenue = sum(Revenue), .groups = 'keep')
    
    req(nrow(plot_data) > 0)
    
    g4 <- plot_data %>%
      ggplot(aes(x = Total_Revenue, y = reorder(Product_Name, Total_Revenue), fill = Status,
                 text = paste("Product: ", Product_Name, "<br>Status: ", Status, "<br>Revenue: ", dollar(Total_Revenue)))) +
      geom_col() +
      facet_wrap(~Status) +
      labs(
        title = "Revenue by Product, Segmented by Business Status",
        subtitle = "Product performance across customer segments for the selected period",
        x = "Total Revenue",
        y = "Product"
      ) +
      scale_x_continuous(labels = dollar_format(scale = .001, suffix = "K")) +
      scale_fill_manual(values = .base_colors$primary_blues) +
      theme_cta_resize() +
      theme(legend.position = "none")
    
    ggplotly(g4, tooltip = "text")
  })
  
  # --- Chart g5: Daily Revenue vs. Daily Traffic (Scatter) ---
  output$scatterPlot <- renderPlotly({
    plot_data <- filtered_daily_data()
    
    req(nrow(plot_data) > 0)
    
    g5 <- plot_data %>%
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
    
    ggplotly(g5, tooltip = "text")
  })
  
  # --- Chart g6: Daily Revenue (Lollipop) ---
  output$lollipopPlot <- renderPlotly({
    plot_data <- filtered_daily_data()
    
    req(nrow(plot_data) > 0)
    
    # --- UX Guardrail ---
    # Only render the lollipop chart if 60 days or fewer are selected.
    if (nrow(plot_data) > 60) {
      # Use plotly_empty to create a placeholder with a message
      plotly_empty() %>%
        layout(
          title = list(
            text = "Please select a smaller date range (<= 60 days)<br>to view the Daily Revenue Detail chart.",
            font = list(color = "#005287")
          )
        )
    } else {
      # If range is acceptable, build the lollipop chart
      dodge_width <- 0.2 
      
      g6 <- plot_data %>%
        ggplot(aes(x = Date, y = Total_Revenue, color = Campaign_Platform, text = paste(
          "Date: ", Date,
          "<br>Revenue: ", dollar(Total_Revenue),
          "Platform: ", Campaign_Platform
        ))) +
        geom_segment(
          # Add position_dodge() here
          aes(x = Date, xend = Date, y = 0, yend = Total_Revenue),
          lwd = 0.75,
          position = position_dodge(width = dodge_width) 
        ) +
        geom_point(
          # Add position_dodge() here as well
          lwd = 4,
          position = position_dodge(width = dodge_width) 
        ) +
        labs(
          title = "Daily Revenue Detail: Last 14 Days",
          subtitle = "Revenue for each day in the selected range",
          x = "Date",
          y = "Total Revenue"
        ) +
        scale_y_continuous(labels = dollar_format()) +
        scale_x_date(date_labels = "%b %d") +
        theme_cta_resize() +
        scale_color_manual(values = .base_colors$combined_colors) +
        theme(panel.grid.major.x = element_blank())
      
      ggplotly(g6, tooltip = "text")
      
      # g6 <- plot_data %>%
      #   ggplot(aes(x = Date, y = Total_Revenue, color = Campaign_Platform, text = paste(
      #     "Date: ", Date,
      #     "<br>Revenue: ", dollar(Total_Revenue),
      #     "Platform: ", Campaign_Platform
      #   ))) +
      #   geom_segment(
      #     aes(x = Date, xend = Date, y = 0, yend = Total_Revenue),
      #     #color = .base_colors$primary_blues[4],
      #     lwd = 0.75
      #   ) +
      #   geom_point(
      #     #color = .base_colors$primary_blues[1], 
      #     lwd = 4) +
      #   labs(
      #     title = "Daily Revenue Detail: Last 14 Days",
      #     subtitle = "Revenue for each day in the selected range",
      #     x = "Date",
      #     y = "Total Revenue"
      #   ) +
      #   scale_y_continuous(labels = dollar_format()) +
      #   scale_x_date(date_labels = "%b %d") +
      #   theme_cta_resize() +
      #   scale_color_manual(values = .base_colors$combined_colors) +
      #   theme(panel.grid.major.x = element_blank())
      # 
      # ggplotly(g6, tooltip = "text")
    }
  })
  
}


# --- 5. RUN THE SHINY APP ---
shinyApp(ui = ui, server = server)