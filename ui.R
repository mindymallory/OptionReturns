library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application Title
  titlePanel("Calculate Option Payoff"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
       textInput("ticker", "Ticker Symbol",
                 value = "SPY"),
       
       radioButtons("callput", "Call or Put Option",
                    choices = c("Call" = "call", "Put" = "put")),
       
       sliderInput("daystomat",
                   "Number of days until option matures:",
                   min = 0,
                   max = 251, 
                   value = 30),
       
       sliderInput("duration",
                  "Number of days to hold option:",
                  min = 0,
                  max = 251, 
                  value = 5),
      
      sliderInput("volatiltiy",
                  "Annualized volatility:",
                  min = 0,
                  max = 70, 
                  value = 25),
      sliderInput("interest",
                  "Interest rate:",
                  min = 0,
                  max = 10, 
                  value = 2, step=0.1),
      
      sliderInput("return",
                  "Target security price return over horizon on which option is held:",
                  min = -5,
                  max = 5, 
                  value = 1, step = 0.1)
                  
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot")
    )
    )
  )
)
  
