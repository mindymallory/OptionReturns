#[copyright: Mindy L Mallory 2015]


library(shiny)
library(ggplot2)
library(quantmod)
library(xts)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
 
   output$Plot <- renderPlot({
   
     if(input$callput=="call"){
    s   = getQuote(input$ticker)
    K   = s$Last
    S   = K*(1+input$return/100)
    r   = input$interest/100
    t   = input$duration/251
    T   = input$daystomat/251
    vol = input$volatiltiy/100
 
       # BS Price of call option when sold
    C     <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))))*x -
        pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))) - vol*sqrt((T-t)))*K*exp(-r*(T-t))}
    
    # BS Price of call option when purchased
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
     } else{ 
     s   = getQuote(input$ticker)
     K   = s$Last
     S   = K*(1+input$return/100)
     r   = input$interest/100
     t   = input$duration/251
     T   = input$daystomat/251
     vol = input$volatiltiy/100
     
     # BS Price of call option when sold
     P     <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))))*x -
         pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))) - vol*sqrt((T-t)))*K*exp(-r*(T-t)) - x + K*exp(-r*(T-t))}
     
     # BS Price of call option when purchased
     P_    <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))))*x -
         pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))) - vol*sqrt((T)))*K*exp(-r*(T)) - x + K*exp(-r*(T))} 
     Pay   <- function(x){K-x}
     zero  <- function(x){0}
     p_      <- P_(K)     
     p       <- P(S)
     X       <- data.frame(K, p_, "At Purchase of Option" )
     colnames(X) = c("Underlying", "Option", "Time")
     Y       <- data.frame(S, p, "At Sale of Option")
     colnames(Y) = c("Underlying", "Option", "Time")
     
     point   <- rbind(X,Y)
     factor(point$Time) 
     
     
     GL = p - p_
     g = toString(round(GL,2))
     GL_Percent = GL/p_*100
     g_percent = paste0(toString(round(GL_Percent,2)), '%')
     
     # Build the Call Graph
     palette <- c("red", "blue")
     ggplot(data=point, aes(x=Underlying, y=Option, colour = Time)) + 
       geom_point(size=5, show_guide=TRUE) +
       stat_function(fun = P, colour =palette[2]) +
       stat_function(fun = P_, colour = palette[1]) +
       stat_function(fun = Pay, colour="black", size=1) +
       stat_function(fun = zero, colour="black") +
       scale_colour_manual(values=palette) +
       xlim(0.90*K, 1.1*K) +
       ylim(0, 3*p)
     }                   
   })
  
  output$view <- renderTable({
    if(input$callput=="call"){
    s   = getQuote(input$ticker)
    K   = s$Last
    S   = K*(1+input$return/100)
    r   = input$interest/100
    t   = input$duration/251
    T   = input$daystomat/251
    vol = input$volatiltiy/100
  # BS Price of option when sold
    C       <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))))*x -
                         pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))) - vol*sqrt((T-t)))*K*exp(-r*(T-t))}
    
  # BS Price of option when purchased
    C_      <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))))*x -
                         pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))) - vol*sqrt((T)))*K*exp(-r*(T))} 
    Pay     <- function(x){x-K}
    zero    <- function(x){0}
    c_      <- C_(K)     
    c       <- C(S)
    X       <- data.frame(K, c_, "At Purchase of Option" )
               colnames(X) = c("Underlying", "Option", "Time")
    Y       <- data.frame(S, c, "At Sale of Option")
               colnames(Y) = c("Underlying", "Option", "Time")
    
    point   <- rbind(X,Y)
    factor(point$Time) 
    GL      <- c - c_
    g       <- toString(round(GL,2))
    GL_Percent <- GL/c_*100
    g_percent  <- paste0(toString(round(GL_Percent,2)), '%')
    
     x1      <- data.frame(input$ticker, K, S, paste0(input$return,"%"), input$duration)
                colnames(x1) = c("Ticker", "Current Quote", "Expected Future Quote", "% Return", "Days Held" )
    x2      <- data.frame(paste("Black-Scholes", input$callput, "option price maturing in", input$daystomat, "days"), c_, c, 
                               paste0(toString(round(GL_Percent,2)), '%'), input$duration)
               colnames(x2) = c("Ticker", "Current Quote", "Expected Future Quote", "% Return", "Days Held" )
    
    table1  <- rbind(x1,x2)                    
    } else {
      s   = getQuote(input$ticker)
      K   = s$Last
      S   = K*(1+input$return/100)
      r   = input$interest/100
      t   = input$duration/251
      T   = input$daystomat/251
      vol = input$volatiltiy/100
      
      # BS Price of call option when sold
      P     <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))))*x -
          pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))) - vol*sqrt((T-t)))*K*exp(-r*(T-t)) - x + K*exp(-r*(T-t))}
      
      # BS Price of call option when purchased
      P_    <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))))*x -
          pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))) - vol*sqrt((T)))*K*exp(-r*(T)) - x + K*exp(-r*(T))} 
      Pay   <- function(x){K-x}
      zero  <- function(x){0}
      p_      <- P_(K)     
      p       <- P(S)
      X       <- data.frame(K, p_, "At Purchase of Option" )
      colnames(X) = c("Underlying", "Option", "Time")
      Y       <- data.frame(S, p, "At Sale of Option")
      colnames(Y) = c("Underlying", "Option", "Time")
      
      point   <- rbind(X,Y)
      factor(point$Time) 
      
      
      GL = p - p_
      g = toString(round(GL,2))
      GL_Percent = GL/p_*100
      g_percent = paste0(toString(round(GL_Percent,2)), '%')
      x1      <- data.frame(input$ticker, K, S, paste0(input$return,"%"), input$duration)
      colnames(x1) = c("Ticker", "Current Quote", "Expected Future Quote", "% Return", "Days Held" )
      x2      <- data.frame(paste("Black-Scholes", input$callput, "option price maturing in", input$daystomat, "days"), p_, p, 
                            paste0(toString(round(GL_Percent,2)), '%'), input$duration)
      colnames(x2) = c("Ticker", "Current Quote", "Expected Future Quote", "% Return", "Days Held" )
      
      table1  <- rbind(x1,x2)      
    }                         
                        
  })
  
  output$view2 <- renderTable({
    if(input$callput=="call"){
    s   = getQuote(input$ticker)
    K   = s$Last
    S   = K*(1+input$return/100)
    r   = input$interest/100
    t   = input$duration/251
    T   = input$daystomat/251
    vol = input$volatiltiy/100
    # BS Price of option when sold
    C       <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))))*x -
        pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))) - vol*sqrt((T-t)))*K*exp(-r*(T-t))}
    
    # BS Price of option when purchased
    C_      <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))))*x -
        pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))) - vol*sqrt((T)))*K*exp(-r*(T))} 
    Pay     <- function(x){x-K}
    zero    <- function(x){0}
    c_      <- C_(K)     
    c       <- C(S)
    X       <- data.frame(K, c_, "At Purchase of Option" )
    colnames(X) = c("Underlying", "Option", "Time")
    Y       <- data.frame(S, c, "At Sale of Option")
    colnames(Y) = c("Underlying", "Option", "Time")
    
    point   <- rbind(X,Y)
    factor(point$Time) 
    GL      <- c - c_
    g       <- toString(round(GL,2))
    GL_Percent <- GL/c_*100
    g_percent  <- paste0(toString(round(GL_Percent,2)), '%')
    
    contracts1     <- input$invested/K
    sec_realized   <- (1+input$return/100)*input$invested
    contracts2     <- input$invested/(c_*100)
    opt_realized   <- (1+GL_Percent/100)*input$invested
    
    x1      <- data.frame(input$ticker, paste0(input$return, "%"), contracts1, sec_realized)
    colnames(x1) <- c(paste0("Ticker: Invested ", "$", input$invested), "% Return", "Contracts Purchased", "Realized Value")
    x2      <- data.frame(paste("Black-Scholes", toString(input$callput), "option price maturing in", toString(input$daystomat), "days"), 
                          g_percent, contracts2, opt_realized)
    colnames(x2) <- c(paste0("Ticker: Invested ", "$", input$invested), "% Return", "Contracts Purchased", "Realized Value")
    
     table2  <- rbind(x1, x2)    
    } else{
      s   = getQuote(input$ticker)
      K   = s$Last
      S   = K*(1+input$return/100)
      r   = input$interest/100
      t   = input$duration/251
      T   = input$daystomat/251
      vol = input$volatiltiy/100
      
      # BS Price of call option when sold
      P     <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))))*x -
          pnorm((log(x/K)+(r+vol^2/2)*(T-t))/(vol*sqrt((T-t))) - vol*sqrt((T-t)))*K*exp(-r*(T-t)) - x + K*exp(-r*(T-t))}
      
      # BS Price of call option when purchased
      P_    <- function(x){pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))))*x -
          pnorm((log(x/K)+(r+vol^2/2)*(T))/(vol*sqrt((T))) - vol*sqrt((T)))*K*exp(-r*(T)) - x + K*exp(-r*(T))} 
      Pay   <- function(x){K-x}
      zero  <- function(x){0}
      p_      <- P_(K)     
      p       <- P(S)
      X       <- data.frame(K, p_, "At Purchase of Option" )
      colnames(X) = c("Underlying", "Option", "Time")
      Y       <- data.frame(S, p, "At Sale of Option")
      colnames(Y) = c("Underlying", "Option", "Time")
      
      point   <- rbind(X,Y)
      factor(point$Time) 
      
      
      GL = p - p_
      g = toString(round(GL,2))
      GL_Percent = GL/p_*100
      
      g_percent = paste0(toString(round(GL_Percent,2)), '%')
      contracts1     <- input$invested/K
      sec_realized   <- (1+input$return/100)*input$invested
      contracts2     <- input$invested/(p_*100)
      opt_realized   <- (1+GL_Percent/100)*input$invested
      
      
      x1      <- data.frame(input$ticker, paste0(input$return, "%"), contracts1, sec_realized)
      colnames(x1) <- c(paste0("Ticker: Invested ", "$", input$invested), "% Return", "Contracts Purchased", "Realized Value")
      x2      <- data.frame(paste("Black-Scholes", toString(input$callput), "option price maturing in", toString(input$daystomat), "days"), 
                            g_percent, contracts2, opt_realized)
      colnames(x2) <- c(paste0("Ticker: Invested ", "$", input$invested), "% Return", "Contracts Purchased", "Realized Value")
      
      table2  <- rbind(x1, x2)    
    }
     
  })
  
})