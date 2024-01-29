library(tidyverse)

ui <- fluidPage(
  # Application title
  titlePanel("Model Flexibility"),
  # Sidebar with a numeric input for month
  fluidRow(
    
    column(3,
           selectInput(inputId = "true_model",
                       label = "Select a 'true' model",
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
                       max = 5,
                       value = 1,
                       step = 0.5)),
    
    column(3,
           sliderInput(inputId = "flex",
                       label = "Select flexibility",
                       min = 0.1,
                       max = 2,
                       value = 1,
                       step = 0.1))
    
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
  
  
  a <- 3
  b <- 0.87
  c <- 0.5
  
  df <- reactive({
    
    if(input$true_model == "Linear")
    {
      set.seed(208)
      
      # simulate data
      x <- runif(n = 100, min = 20, max = 40)   # input/predictor
      
      e <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
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

      fx <- a + (b * sqrt(x)) + (c * sin(x))   # true function
      
      y <- fx + e    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response = y)  
    }
    
    return(toy_data)
  })
  
  
  output$truePlot <- renderPlot({
    
    ggplot(data = df(), aes(x = inp, y = true_form)) + 
      geom_point() + 
      labs(title = "True relationship without error", y = "f(x)", x = "x") +
      ylim(c(min(df()$response), max(df()$response)))
    
  })
  
  
  
  output$observedPlot <- renderPlot({
    
    if(input$true_model == "Linear")
    {
      ggplot(data = df(), aes(x = inp, y = response)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*x), aes(color = "true model"), linewidth = 1.5) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model")) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = input$flex, se = FALSE, aes(color = "non-linear model")) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        theme(legend.title = element_blank()) +
        labs(title = "Comparing two models", y = "y", x = "x")
    }
    else
    {
      ggplot(data = df(), aes(x = inp, y = response)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*sqrt(x))+(c*sin(x)), aes(color = "true model"), linewidth = 1.5) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model")) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = input$flex, se = FALSE, aes(color = "non-linear model")) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        theme(legend.title = element_blank()) +
        labs(title = "Comparing two models", y = "y", x = "x")
    }
    
    
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
