#app script scratchpad
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

####v2

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "cat_filter",
        label = "Select Categories",
        choices = unique(df$category),
        multiple = TRUE
      )
    )
  ),
  mainPanel(
    tableOutput("table")
  )
)

server <- function(input, output) {
  output$table <- renderTable({
    df %>% filter(category %in% input$cat_filter)
  })
}

shinyApp(ui, server)