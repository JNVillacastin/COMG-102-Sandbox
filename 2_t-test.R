library(shiny)
library(ggdist)
library(ggplot2)

# Function to perform t-test and check significance
t_test_significance <- function(mean1, mean2, sd1, sd2, n1, n2) {
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  t_stat <- (mean1 - mean2) / (pooled_sd * sqrt(1/n1 + 1/n2))
  df <- n1 + n2 - 2
  p_value <- 2 * pt(-abs(t_stat), df)
  return(list(t_stat = t_stat, p_value = p_value))
}

ui <- fluidPage(
  titlePanel("T-test simulator"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("mean1", "Mean of Group 1:", min = 1, max = 7, value = 4, step = 0.5),
      sliderInput("sd1", "Standard Deviation of Group 1:", min = 0, max = 3, value = 1, step = 0.5),
      sliderInput("mean2", "Mean of Group 2:", min = 1, max = 7, value = 4, step = 0.5),
      sliderInput("sd2", "Standard Deviation of Group 2:", min = 0, max = 3, value = 1, step = 0.5),
      sliderInput("n", "Sample Size (per group):", min = 5, max = 100, value = 30)
    ),
    
    mainPanel(
      plotOutput("boxPlot"),
      h3("t-Test Results"),
      verbatimTextOutput("testResults"), 
      HTML("<p style='font-size: small; color: gray;'> © 2024 J.N. Villacastin. For the use of COMG 102 students.</p>")
    )
  )
)

server <- function(input, output) {
  
  output$boxPlot <- renderPlot({
    mean1 <- input$mean1
    sd1 <- input$sd1
    mean2 <- input$mean2
    sd2 <- input$sd2
    n <- input$n
    
    # Create sample data for both groups
    group1 <- rnorm(n, mean = mean1, sd = sd1)
    group2 <- rnorm(n, mean = mean2, sd = sd2)
    
    df <- data.frame(
      value = c(group1, group2),
      group = factor(rep(c("Group 1", "Group 2"), each = n))
    )
    
    ggplot(df, aes(x = group, y = value, fill = group)) +
      stat_dots(side = "left", justification = 1.1, binwidth = 0.15) +
      geom_boxplot(width = 0.12) +
      scale_y_continuous(limits = c(0, 8), breaks = c(1, 7)) +
      labs(title = "Raincloud Plots of Group 1 and Group 2",
           x = "Group",
           y = "Value") +
      theme_minimal()
  })
  
  output$testResults <- renderPrint({
    mean1 <- input$mean1
    sd1 <- input$sd1
    mean2 <- input$mean2
    sd2 <- input$sd2
    n <- input$n
    
    results <- t_test_significance(mean1, mean2, sd1, sd2, n, n)
    
    cat(sprintf("t-Test Statistic: %.2f\np-Value: %.4f\nSignificance: %s",
                results$t_stat, results$p_value, 
                ifelse(results$p_value < 0.05, "Significant (p < 0.05)", "Not Significant (p ≥ 0.05)")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
