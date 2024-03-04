library(tidyverse)
library(caret)
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("K-Nearest Neighbors"),
  
  sidebarLayout(
    sidebarPanel(
    
    # column(3,
           selectInput(inputId = "true_model",
                       label = "Select a 'true' model",
                       choices = c("Linear", "Non-linear"),
                       selected = "Linear"),
    
    # column(3,
           sliderInput(inputId = "epsilon",
                       label = "Select variability:",
                       min = 0,
                       max = 5,
                       value = 1,
                       step = 0.5),
    
    # column(3,
           sliderInput(inputId = "k",
                       label = "Select a value of K:",
                       min = 1,
                       max = 50,
                       value = 10,
                       step = 1,
                       animate = TRUE),
    
    # column(3, 
           plotOutput("myLegend"))
    
  ,
  
  mainPanel(
    # column(4, 
           # plotOutput("truePlot"),
    # column(4,
           plotOutput("observedPlot")
  ))
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
      x <- runif(n = 50, min = 20, max = 40)   # input/predictor
      
      e <- rnorm(n = 50, mean = 0, sd = input$epsilon)  # error
      
      fx <- a + (b * x)  # true function
      
      y <- fx + e    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response = y)  
    }
    
    else 
    {
      set.seed(208)
      
      # simulate data
      x <- runif(n = 50, min = 20, max = 40)   # input/predictor
      
      e <- rnorm(n = 50, mean = 0, sd = input$epsilon)  # error
      
      fx <- a + (b * sqrt(x)) + (c * sin(x))   # true function
      
      y <- fx + e    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response = y)  
    }
    
    return(toy_data)
  })
  
  
  # output$truePlot <- renderPlot({
  #   
  #   ggplot(data = df(), aes(x = inp, y = true_form)) + 
  #     geom_point() + 
  #     labs(title = "True relationship without error", y = "f(x)", x = "x") +
  #     ylim(c(min(df()$response), max(df()$response)))
  #   
  # })
  # 
  
  
  output$observedPlot <- renderPlot({
    
    if(input$true_model == "Linear")
    {
      knnfit <- knnreg(response ~ inp, data = df(), k = input$k)   # KNN regression
      knn_preds <- predict(knnfit, newdata = data.frame(inp = seq(min(df()$inp, na.rm = TRUE), max(df()$inp, na.rm = TRUE), 0.001)))   # predictions
      knn_df <- data.frame(x = seq(min(df()$inp, na.rm = TRUE), max(df()$inp, na.rm = TRUE), 0.001), preds = knn_preds)
      
      ggplot(data = df(), aes(x = inp, y = response)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*x), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_line(data = knn_df, aes(x = x, y = preds, color = "KNN fit"), linewidth = 1, show.legend = FALSE) +
        # geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model")) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "KNN fit" = "cyan")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Comparing KNN fits", y = "y", x = "x")
    }
    
    else
    {
      knnfit <- knnreg(response ~ inp, data = df(), k = input$k)   # KNN regression
      knn_preds <- predict(knnfit, newdata = data.frame(inp = seq(min(df()$inp, na.rm = TRUE), max(df()$inp, na.rm = TRUE), 0.001)))   # predictions
      knn_df <- data.frame(x = seq(min(df()$inp, na.rm = TRUE), max(df()$inp, na.rm = TRUE), 0.001), preds = knn_preds)
      
      ggplot(data = df(), aes(x = inp, y = response)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*sqrt(x))+(c*sin(x)), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_line(data = knn_df, aes(x = x, y = preds, color = "KNN fit"), linewidth = 1, show.legend = FALSE) +
        # geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model")) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "KNN fit" = "cyan")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Comparing KNN fits", y = "y", x = "x")
      
    }
    
    
  })
  
  
  output$myLegend <- renderPlot({
    par(mai=rep(0.01,4))
    # plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,.1), ylim=c(0,.1))
    legend("center", legend=c("true model","linear model", "KNN fit"), lty=c(1,1,1), lwd=c(4,4,4), col=c("red", "darkblue", "cyan"))
  },height=50)
  
  
  
  
}


# Run the application
shinyApp(ui = ui, server = server)
