library(shiny)

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
    
    # Price of option when purchased
    d1_ = (log(K/K)+(r+vol^2/2)*T)/(vol*sqrt(T))
    d2_ = d1_ - vol*sqrt(T)
    C_ = pnorm(d1_)*K-pnorm(d2_)*K*exp(-r*T)   #Call Price at Strike
    
    # Price of option when sold
    d1 = (log(S/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t)))
    d2 = d1 - vol*sqrt((T-t))
    C  = pnorm(d1)*S-pnorm(d2)*K*exp(-r*(T-t))
    
    curve(pnorm((log(x/K)+(r+vol^2/2)*T)/(vol*sqrt(T)))*x-pnorm((log(x/K)+(r+vol^2/2)*T)/(vol*sqrt(T))- vol*sqrt(T))*K*exp(-r*T), 
          from = .90*K, to=1.1*K, xlab = "Price of Underlying", ylab = "Option Price", main = "Option Price")
    par(new=TRUE)
    curve(pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))))*x-pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t)))- vol*sqrt((T-t)))*K*exp(-r*(T-t)), 
          from = .90*K, to=1.1*K, xlab = "Price of Underlying", ylab = "Option Price", main = "Option Price")
   
    GL = C - C_
    g = toString(round(GL,2))
    GL_Percent = GL/C_*100
    g_percent = paste0(toString(round(GL_Percent,2)), '%')
    par(new=TRUE)
    plot(K,C_, xlim = .90*K, ylim=1.1*K, xlab = "Price of Underlying", ylab = "Option Price", main = "Option Price")
    par(new=TRUE)
    plot(S,C, xlim = .90*K, ylim=1.1*K, xlab = "Price of Underlying", ylab = "Option Price", main = "Option Price")
    legend("topright", inset = .05, title = "Option Gain/Loss", c(g, g_percent))
    
#     #Graphing the Position Value
#     curve(q*Q*(pnorm((log(x/K)+(r+vol^2/2)*t)/(vol*sqrt(t)))*x-pnorm((log(x/K)+(r+vol^2/2)*t)/(vol*sqrt(t))- vol*sqrt(t))*K*exp(-r*t)), 
#           from = .9*K, to=1.1*K, xlab = "Price of Underlying", ylab = "Position Value", main = "Position Value")
#     abline(v=S)
#     abline(v=K)
#     PosV = C*q*Q 
#     PosV_ = C_*q*Q
#     GL = PosV - PosV_
#     g = toString(round(GL,2))
#     legend("topright", inset = .05, title = "Gain/Loss", c(g))
    
    
    
  })
})