sales_data %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Campaign_Platform, Month) %>%
  summarise(Total_Revenue = sum(Revenue)) %>% 
  ggplot(aes(x = Month, y = Total_Revenue, group=Campaign_Platform, color=Campaign_Platform, text = paste(
    "Month: ", format(Month, "%b %Y"),
    "<br>Revenue: ", dollar(Total_Revenue)
  ))) +
  geom_line()+#color = .base_colors$primary_blues[1], size = 1) +
  geom_point()+#color = .base_colors$primary_blues[1], size = 2) +
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


library(shiny)

# Sample data (replace with your dataframe)
df <- data.frame(
  category = c("X", "Y", "Z", "X", "Y"),
  value = 1:5
)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "cat_filter",
        label = "Select Category",
        choices = unique(df$category)  # Uses unique values from your column
      )
    )
  ),
  mainPanel(
    tableOutput("table")
  )
)

server <- function(input, output) {
  output$table <- renderTable({
    df %>% filter(category == input$cat_filter)
  })
}

shinyApp(ui, server)