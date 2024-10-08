##TEST 

library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("Normal Distribution Visualizer"),

    # Sliders
  sidebarLayout(
    sidebarPanel(
      sliderInput("sample_size",
                  "Sample Size:",
                  min = 10,
                  max = 200,
                  value = 100),
      sliderInput("mean",
                  "Mean: (should be within range)",
                  min = 1,
                  max = 7,
                  value = 4, 
                  step = .5),
      sliderInput("sd",
                  "Standard Deviation:",
                  min = 0,
                  max = 2,
                  value = 1, 
                  step = .5),
      sliderInput("range",
                  "Range:",
                  min = 1,
                  max = 7,
                  value = c(1, 7)),
    ),
    
    # Splot
    mainPanel(
      plotOutput("distPlot"), 
      HTML("<p style='font-size: small; color: gray;'> © 2024 J.N. Villacastin. For the use of COMG 102 students.</p>")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # Simulate sample based on input parameters
    samples <- numeric(input$sample_size)  # Initialize an empty vector
    for (i in 1:input$sample_size) {
      repeat {
        sample <- rnorm(1, mean = input$mean, sd = input$sd)
        if (sample >= input$range[1] && sample <= input$range[2]) {
          samples[i] <- sample
          break  # Exit the repeat loop once a valid sample is found
        }
      }
    }
    
    # Create a data frame for ggplot
    hist_data <- data.frame(samples = samples)
    
    # Plot the histogram
    ggplot(hist_data, aes(x = samples)) +
      geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black", alpha = 0.7) +
      scale_x_continuous(limits = c(0, 8), breaks = 1:7) +  # Set x-axis limits and ticks
      scale_y_continuous(breaks = seq(0, max(table(cut(hist_data$samples, breaks=seq(1, 7, 0.5)))), 5)) +  # Y-axis ticks
      ggtitle(paste("Histogram of Normal Distribution (Mean =", input$mean, ", SD =", input$sd, ")")) +
      xlab("Value") +
      ylab("Frequency") +
      theme_minimal()
  })
}

# Run 
shinyApp(ui = ui, server = server)
