library(shiny)
library(ggplot2)
library(quantmod)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
 
  # Expression that generates a histogram. The expression is 
  # wrapped in a call to renderPlot to indicate that:
  #
  # 1) It is "reactive" and therefore should re-execute automatically
  #    when inputs change
  # 2) Its output type is a plot
  
  output$Plot <- renderPlot({
    s   = getQuote(input$ticker)
    K   = s$Last
    S   = K*(1+input$return/100)
    r   = input$interest/100
    t   = input$duration/251
    T   = input$daystomat/251
    vol = input$volatiltiy/100
 
       # BS Price of option when sold
    C     <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))))*x -
        pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))) - vol*sqrt((T-t)))*K*exp(-r*(T-t))}
    
    # BS Price of option when purchased
    C_    <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))))*x -
        pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))) - vol*sqrt((T)))*K*exp(-r*(T))} 
    Pay   <- function(x){x-K}
    zero  <- function(x){0}
  c_      <- C_(K)     
  c       <- C(S)
  X       <- data.frame(K, c_, "At Purchase of Option" )
             colnames(X) = c("Underlying", "Option", "Time")
  Y       <- data.frame(S, c, "At Sale of Option")
             colnames(Y) = c("Underlying", "Option", "Time")
  
  point   <- rbind(X,Y)
  factor(point$Time) 
  
 
  GL = c - c_
  g = toString(round(GL,2))
  GL_Percent = GL/c_*100
  g_percent = paste0(toString(round(GL_Percent,2)), '%')
 
  # Build the Call Graph
  palette <- c("red", "blue")
     ggplot(data=point, aes(x=Underlying, y=Option, colour = Time)) + 
                       geom_point(size=5, show_guide=TRUE) +
                       stat_function(fun = C, colour =palette[2]) +
                       stat_function(fun = C_, colour = palette[1]) +
                       stat_function(fun = Pay, colour="black", size=1) +
                       stat_function(fun = zero, colour="black") +
                       scale_colour_manual(values=palette) +
                       xlim(0.90*K, 1.1*K) +
                       ylim(0, 3*c)
                       
   })
})