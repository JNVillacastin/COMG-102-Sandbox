#z-score

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Z-score simulator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("zscore",
                  "Z-Score:",
                  min = -3,
                  max = 3,
                  value = 0,
                  step = 0.01,  
                  ticks = FALSE)
    ),
    
    mainPanel(
      plotOutput("distPlot"),
      textOutput("percentile"),
      textOutput("higher"),
      textOutput("lower"),
      HTML("<p style='font-size: small; color: gray;'> © 2024 J.N. Villacastin. For the use of COMG 102 students.</p>")

    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    z <- input$zscore
    x <- seq(-3, 3, length.out = 1000)
    y <- dnorm(x)  # Normal distribution
    
    # Create a data frame for ggplot
    df <- data.frame(x = x, y = y)
    
    # Plot
    ggplot(df, aes(x = x, y = y)) +
      geom_line(color = "green") +
      geom_area(data = df %>% filter(x < z), aes(y = y), fill = "lightgreen", alpha = 0.5) +
      geom_vline(xintercept = z, color = "red", linetype = "dashed", size = 1) +
      labs(title = "Normal Distribution with Z-Score",
           x = "Z-Score",
           y = "Density") +
      theme_minimal()
  })
  
  output$percentile <- renderText({
    z <- input$zscore
    percentile <- pnorm(z) * 100
    paste("Percentile Rank:", round(percentile, 2), "%")
  })
  
  output$lower <- renderText({
    z <- input$zscore
    lower_percent <- pnorm(z) * 100
    paste("% lower than z-score (left):", round(lower_percent, 2), "%")
  })
  
  output$higher <- renderText({
    z <- input$zscore
    higher_percent <- (1 - pnorm(z)) * 100
    paste("% higher than z-score (right):", round(higher_percent, 2), "%")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
