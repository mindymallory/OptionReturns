#[copyright: Mindy L Mallory 2015]
library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application Title
  titlePanel("Demonstrating Leverage in Option Markets"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      helpText("Make selections here about neccessary inputs to the Black-Scholes option pricing model. 
               Latest quotes are fetched from Yahoo! Finance. We assume the option is purchased at the money. That is, 
               strike, K, is equal to the quote fetched. You supply an expected return for the underlying, 
               and the number of days to hold the option, and amount to invest - in addition to the usual required parameters 
               for option pricing. This app returns the prices and returns from a position in the underlying versus 
               a position in the option."),
      
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
                  value = 1, step = 0.1),
      
      sliderInput("invested",
                  "Dollar amount to invest:",
                  min = 0,
                  max = 100000, 
                  value = 10000, step = 1000)
                  
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot"),
      h4("Illustrating the ability to leverage with Options"),
      tableOutput("view"),
      
      helpText("The table above shows that whatever your expected return in the underlying is, the return on the option price
               will be larger in absolute value."),
     
      tableOutput("view2"), 
      
      helpText("The second table just multiplies the individual returns in the underlying and option position by the amount 
               invested. For the same amount of money, you can generate much greater volatilty in your realized value with 
               an option position. This is good if you are right and bad if you are wrong.")
      
      
    )
    )
  )
)
  
