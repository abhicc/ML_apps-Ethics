library(tidyverse)
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Model Bias and Variance"),
  
  fluidRow(
    
    column(3,
           selectInput(inputId = "true_model",
                       label = "Select a 'true' model",
                       choices = c("Linear", "Non-linear"),
                       selected = "Linear"),
           
           selectInput(inputId = "which_viz",
                       label = "What type of vizualization would you like to see?",
                       choices = c("Training Data", "Residual Histograms", "Residual Freq Polygon", "Residual Density"),
                       selected = "Training Data")),
    
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
                       max = 10,
                       value = 1,
                       step = 1)),
    
    
    column(3, plotOutput("myLegend"))
    
    
    
  ),
  
  fluidRow(
    column(4, plotOutput("truePlot")),
    column(4, plotOutput("Plot1")),
    column(4, plotOutput("Plot2"))
  ),
  
  fluidRow(
    column(4, plotOutput("Plot3")),
    column(4, plotOutput("Plot4")),
    column(4, plotOutput("Plot5"))
  )
)


server <- function(input, output) {
  
  
  a <- 3
  b <- 0.15
  c <- 0.87
  d <- 0.5
  
  trueFunction <- reactive({
    if(input$true_model == "Linear"){
      return(function(x) a+(b*x))
    }
    else{
      return(function(x) a+(c*sqrt(x))+(d*sin(x)))
    }
  })
  
  df <- reactive({
    
      set.seed(208)
      
      # simulate data
      x <- runif(n = 100, min = 20, max = 40)   # input/predictor
      
      e1 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e2 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e3 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e4 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e5 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
      fx <- trueFunction()(x)  # true function
      
      y1 <- fx + e1    # observed responses
      y2 <- fx + e2    # observed responses
      y3 <- fx + e3    # observed responses
      y4 <- fx + e4    # observed responses
      y5 <- fx + e5    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5)  
    
    
    return(toy_data)
  })
  
  residuals_df <- reactive({
    resid_df <- data.frame(
      lm1_resid = resid(lm(response1 ~ inp, data = df())),
      loess1_resid = resid(loess(response1 ~ inp, data = df(), span = 1/input$flex)),
      lm2_resid = resid(lm(response2 ~ inp, data = df())),
      loess2_resid = resid(loess(response1 ~ inp, data = df(), span = 1/input$flex)),
      lm3_resid = resid(lm(response3 ~ inp, data = df())),
      loess3_resid = resid(loess(response1 ~ inp, data = df(), span = 1/input$flex)),
      lm4_resid = resid(lm(response4 ~ inp, data = df())),
      loess4_resid = resid(loess(response1 ~ inp, data = df(), span = 1/input$flex)),
      lm5_resid = resid(lm(response5 ~ inp, data = df())),
      loess5_resid = resid(loess(response1 ~ inp, data = df(), span = 1/input$flex))
      )
    return(resid_df)
  })


  
  
  output$truePlot <- renderPlot({

    ggplot(data = df(), aes(x = inp, y = true_form)) +
      geom_point() +
      labs(title = "True relationship without error", y = "f(x)", x = "x") +
      scale_y_continuous(limits = c(3, 13)) +
      scale_x_continuous(limits = c(20, 40))

  })



  output$Plot1 <- renderPlot({

    if(input$which_viz == "Training Data")
    {
      plotFits(df(), input$flex, trueFunction(), whichVar = response1)
    }
    else
    {
      plotHists(residuals_df(), lm1_resid, loess1_resid, plottype = input$which_viz)
    }
    

  })



  output$Plot2 <- renderPlot({
    if(input$which_viz == "Training Data")
    {
      plotFits(df(), input$flex, trueFunction(), whichVar = response2)
    }
    else
    {
      plotHists(residuals_df(), lm2_resid, loess2_resid, plottype = input$which_viz)
    }
    

  })



  output$Plot3 <- renderPlot({
    if(input$which_viz == "Training Data")
    {
      plotFits(df(), input$flex, trueFunction(), whichVar = response3)
    }
    else
    {
      plotHists(residuals_df(), lm3_resid, loess3_resid, plottype = input$which_viz)
    }
    

  })



  output$Plot4 <- renderPlot({
    if(input$which_viz == "Training Data")
    {
      plotFits(df(), input$flex, trueFunction(), whichVar = response4)
    }
    else
    {
      plotHists(residuals_df(), lm4_resid, loess4_resid, plottype = input$which_viz)
    }
    
    

  })



  output$Plot5 <- renderPlot({
    if(input$which_viz == "Training Data")
    {
      plotFits(df(), input$flex, trueFunction(), whichVar = response5)
    }
    else
    {
      plotHists(residuals_df(), lm5_resid, loess5_resid, plottype = input$which_viz)
    }
    

  })



  output$myLegend <- renderPlot({
    par(mai=rep(0.01,4))
    # plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,.1), ylim=c(0,.1))
    legend("center", legend=c("true model","linear model", "non-linear model"), lty=c(1,1,1), lwd=c(4,4,4), col=c("red", "darkblue", "green"))
  },height=50)




}

plotFits <- function(df, flex, trueFunc, whichVar){
  ggplot(data = df, aes(x = inp, y = !!enquo(whichVar))) +
  geom_point() +
  geom_function(fun = trueFunc, aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
  # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
  geom_smooth(span = 1/flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE) +
  scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
  # theme(legend.title = element_blank()) +
  labs(title = "Training Data 1", y = "y", x = "x") +
  scale_y_continuous(limits = c(3, 13)) +
  scale_x_continuous(limits = c(20, 40))
}

plotHists <- function(residuals_df, whichresidlm, whichresidloess, plottype = "Residual Histograms"){
  histplot <- ggplot(data = residuals_df)
    if(plottype == "Residual Freq Polygon"){
      histplot <- histplot +
        geom_freqpoly(aes(x = !!enquo(whichresidlm)), alpha = 0.5, bins = 7, color = "blue") +
        geom_freqpoly(aes(x= !!enquo(whichresidloess)), alpha = 0.5, bins = 7, color = "green")
    }
    else if(plottype == "Residual Density")
    {
      histplot <- histplot +
        geom_density(aes(x = !!enquo(whichresidlm)), alpha = 0.5, fill = "blue") +
        geom_density(aes(x= !!enquo(whichresidloess)), alpha = 0.5, fill = "green")
    }
  else{
    histplot <- histplot +
      geom_histogram(aes(x = !!enquo(whichresidlm)), alpha = 0.5, bins = 7, fill = "blue") +
      geom_histogram(aes(x= !!enquo(whichresidloess)), alpha = 0.5, bins = 7, fill = "green")
  }
    
    histplot +
      geom_vline(xintercept = mean(select(residuals_df, !!enquo(whichresidloess))), color = "blue") +
      geom_vline(xintercept = mean(select(residuals_df, !!enquo(whichresidloess))), color = "green") +
      geom_vline(xintercept = 0, color = "red")
}


# Run the application
shinyApp(ui = ui, server = server)
