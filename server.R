library(shiny)
library(ggplot2)

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
    C <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))))*x -
        pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))) - vol*sqrt((T-t)))*K*exp(-r*(T-t))}
    
    # BS Price of option when purchased
    C_ <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))))*x -
        pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))) - vol*sqrt((T)))*K*exp(-r*(T))} 
    
  X <- c(K, S)
  Y <- c(C_(K), C(S))
  df <- data.frame(X,Y)
  GL = C(S) - C_(K)
  g = toString(round(GL,2))
  GL_Percent = GL/C_(K)*100
  g_percent = paste0(toString(round(GL_Percent,2)), '%')
 
#    ggplot(df, aes(x=X, y=Y)) + 
#                        geom_point() +
#                        stat_function(fun = C) +
#                        stat_function(fun = C_) +
#                        xlim(0.95*K, 1.05*K) 
#                        
  #plot.window(c(.90*K,1.1*K), c(.9*C(S), 1.1*C(S)))
  curve(pnorm((log(x/K)+(r+vol^2/2)*T)/(vol*sqrt(T)))*x-pnorm((log(x/K)+(r+vol^2/2)*T)/(vol*sqrt(T))- vol*sqrt(T))*K*exp(-r*T), 
        from = .95*K, to=1.05*K, xlab = "Price of Underlying", ylim= c(0,3*C(S)), ylab = "Option Price", main = "Option Price", col="black")
    par(new=TRUE)
  curve(pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))))*x-pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t)))- vol*sqrt((T-t)))*K*exp(-r*(T-t)), 
        from = .95*K, to=1.05*K, xlab = "Price of Underlying", ylim= c(0,3*C(S)),ylab = "Option Price", main = "Option Price", col="red")
    par(new=TRUE)
#   plot(K,C_(K), xlim =c(.90*K,1.1*K), xlab = "Price of Underlying", ylab = "Option Price", main = "Option Price")
#     par(new=TRUE)
#   plot(S,C(S), xlim =c(.90*K,1.1*K), xlab = "Price of Underlying", ylab = "Option Price", main = "Option Price")
    legend("topright", inset = .05, title = "Option Gain/Loss", c(g, g_percent))
        abline(v=S)
        abline(v=K)   
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