# Create the Shiny app
library(shiny)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Life Expectancy Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select a Country:", choices = unique(dat_lifeexp$country_name))
    ),
    mainPanel(
      plotOutput("lifeExpPlot")
    )
  )
)

# Define server logic required to draw the plot
server <- function(input, output) {
  output$lifeExpPlot <- renderPlot({
    filtered_data <- dat_lifeexp %>% filter(country_name == input$country)
    ggplot(filtered_data, aes(x = year, y = life_expectancy)) + 
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = paste("Life Expectancy Over Time in", input$country),
           x = "Year",
           y = "Life Expectancy") +
      theme_minimal()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)






# Define UI for the application
ui <- fluidPage(
  titlePanel("Life Expectancy Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country/Countries:", choices = sort(unique(dat_lifeexp$country_name)), multiple = TRUE)
    ),
    mainPanel(
      plotOutput("lifeExpPlot")
    )
  )
)

# Define server logic required to draw the plot
server <- function(input, output) {
  output$lifeExpPlot <- renderPlot({
    filtered_data <- dat_lifeexp %>% filter(country_name %in% input$country)
    ggplot(filtered_data, aes(x = year, y = life_expectancy, color = country_name)) + 
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Life Expectancy Over Time",
           x = "Year",
           y = "Life Expectancy",
           color = "Country") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




