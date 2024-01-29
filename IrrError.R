library(tidyverse)

ui <- fluidPage(
  # Application title
  titlePanel("Irreducible/Unaccountable Error"),
  # Sidebar with a numeric input for month
  fluidRow(
    
    column(3,
           selectInput(inputId = "true_model",
                        label = "Select a model",
                        choices = c("Linear", "Non-linear"),
                        selected = "Linear")),
    
    # column(3,
    #        numericInput(inputId = "sd_value",
    #                     label = "Input a standard deviation",
    #                     min = 1,
    #                     max = 20,
    #                     value = 10, 
    #                     step = 1)),
    
    column(3,
           sliderInput(inputId = "epsilon",
                       label = "Select variability",
                       min = 0,
                       max = 10,
                       value = 1,
                       step = 0.5))
  ),
  # Show a plot of the generated distribution
  
  fluidRow(
    column(4, 
           plotOutput("truePlot")),
    column(4,
           plotOutput("observedPlot"))
    # ,
    # column(4, 
    #        tableOutput("table"))
  )
)


server <- function(input, output) {
  
  df <- reactive({
    
    if(input$true_model == "Linear")
    {
      set.seed(208)
      
      # simulate data
      x <- runif(n = 100, min = 20, max = 40)   # input/predictor
      
      e <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
      a <- 3
      b <- 0.87
      c <- 0.5
      fx <- a + (b * x)  # true function
      
      y <- fx + e    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response = y)  
    }
    # else if (input$shape == "Positively Skewed")
    # {
    #   val_right <- rsnorm(1000, mean = input$mean_value, sd = input$sd_value, xi = 3)
    #   df <- data.frame(value = val_right)
    # }
    else 
    {
      set.seed(208)
      
      # simulate data
      x <- runif(n = 100, min = 20, max = 40)   # input/predictor
      
      e <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
      a <- 3
      b <- 0.87
      c <- 0.5
      fx <- a + (b * sqrt(x)) + (c * sin(x))   # true function
      
      y <- fx + e    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response = y)  
    }
    
    return(toy_data)
  })
  
  
  output$truePlot <- renderPlot({
    
    ggplot(data = df(), aes(x = inp, y = true_form)) + 
      geom_point() + labs(title = "True relationship without error", y = "f(x)", x = "x")
    
  })
  
  
  
  output$observedPlot <- renderPlot({
    
    ggplot(data = df(), aes(x = inp, y = response)) + 
      geom_point() + labs(title = "Observed relationship", y = "y", x = "x")
    
  })
  
  
  
  # output$table <- renderTable({
  #   
  #   res <- favstats(~ value, data=df())
  #   ftable <- data.frame(Summary = c("min", "Q1", "median", "Q3", "max", "IQR", "range"),
  #                        Values = c(round(res$min,3), round(res$Q1,3), round(res$median,3), 
  #                                   round(res$Q3,3), round(res$max,3), 
  #                                   round(res$Q3,3) - round(res$Q1,3),
  #                                   round(res$max,3) - round(res$min,3)))
  #   
  #   ftable
  # })
  
}


# Run the application
shinyApp(ui = ui, server = server)
