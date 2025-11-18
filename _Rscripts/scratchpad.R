library(shiny)
library(dplyr)
#library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(plotly)

# # Data Loading from RDS for speed
preloaded_data <- readRDS("preloaded_sales_data.rds")
# Access list elements into environment:
sales_data <- preloaded_data$sales_data
daily_data <- preloaded_data$daily_data
min_date <- preloaded_data$min_date
max_date <- preloaded_data$max_date
categories <- preloaded_data$categories
.base_colors <- preloaded_data$current_theme$base_colors
theme_cta_resize <- preloaded_data$current_theme$theme_cta_resize

filtered_daily_data <- daily_data %>%
  filter(#Campaign_Platform %in% input$cat_filter,
  Date >= max_date-lubridate::days(30) &
  Date <= max_date)  

# --- Chart g6: Daily Revenue (Lollipop) ---
plot_data <- filtered_daily_data

select_categories <- categories
  # 1. Define the custom dodge function
  get_date_dodge_offsets <- function(selected_categories, spread_width = 0.6) {
    # selected_categories: a vector of unique category names
    # spread_width: total width of the spread in days (e.g., 0.8 = +/- 0.4)
    n <- length(selected_categories)
    # If only one category, no dodge needed
    if(n == 1) return(setNames(0, selected_categories))
    # Create a centered sequence (e.g., -0.4, -0.13, 0.13, 0.4)
    offsets <- seq(from = -spread_width/2, to = spread_width/2, length.out = n)
    # Return a named vector mapping Category -> Offset
    return(setNames(offsets, selected_categories))
  }
  
  # 2. Prepare the data with the offsets
  # Ensure Campaign_Platform is a factor to maintain consistent ordering
  platforms <- levels(factor(plot_data$Campaign_Platform))
  dodge_map <- get_date_dodge_offsets(platforms)
  
  plot_data_dodged <- plot_data %>%
    mutate(
      # Convert Date to POSIXct (seconds) to allow fractional day addition
      Date_Time = as.POSIXct(Date),
      
      # lookup the offset value
      day_offset_val = dodge_map[as.character(Campaign_Platform)],
      
      # Create the dodged position: Date + (Duration in Days)
      # 86400 seconds = 1 day. 
      Dodged_Date = Date_Time + lubridate::duration(day_offset_val, "days")
    )
  
  # 3. Generate the Plot
  g6 <- plot_data_dodged %>%
    ggplot(aes(x = Dodged_Date, y = Total_Revenue, color = Campaign_Platform, 
               text = paste(
                 "Date: ", Date, # Keep original date for tooltip
                 "<br>Revenue: ", dollar(Total_Revenue),
                 "Platform: ", Campaign_Platform
               ))) +
    # Segment uses the Dodged Date for both X and Xend
    geom_segment(
      aes(xend = Dodged_Date, y = 0, yend = Total_Revenue),
      lwd = 0.75
    ) +
    # Points also use the Dodged Date (inherited from global aes)
    geom_point(size = 4) +
    labs(
      title = "Daily Revenue Detail: Last 14 Days",
      subtitle = "Revenue for each day in the selected range",
      x = "Date",
      y = "Total Revenue"
    ) +
    scale_y_continuous(labels = dollar_format()) +
    # Use datetime scale to handle the fractional days gracefully
    scale_x_datetime(date_labels = "%b %d") + 
    theme_cta_resize() +
    scale_color_manual(values = .base_colors$combined_colors) +
    theme(panel.grid.major.x = element_blank())
  
  ggplotly(g6, tooltip = "text")
