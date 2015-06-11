#graphing option prices and payoffs.
library(quantmod)

s = getQuote('SPY')
S = s$Last
#S = 50*.99
K = 212.33
r = .01
t = 1/252
vol = .53

q = 2  #number of option contracts
Q = 100 #number of underlying option represents

d1 = (log(S/K)+(r+vol^2/2)*t)/(vol*sqrt(t))
d2 = d1 - vol*sqrt(t)
C  = pnorm(d1)*S-pnorm(d2)*K*exp(-r*t)
C_ = pnorm(d1)*K-pnorm(d2)*K*exp(-r*t)   #Call Price at Strike


#Graphing the Option Price
curve(pnorm((log(x/K)+(r+vol^2/2)*t)/(vol*sqrt(t)))*x-pnorm((log(x/K)+(r+vol^2/2)*t)/(vol*sqrt(t))- vol*sqrt(t))*K*exp(-r*t), 
      from = .9*K, to=1.1*K, xlab = "Price of Underlying", ylab = "Option Price", main = "Option Price")
Pr = C 
Pr_ = C_
GL = Pr - Pr_
g = toString(round(GL,2))
abline(v=S)
abline(v=K)
legend("topright", inset = .05, title = "Gain/Loss", c(g))



#Graphing the Position Value
curve(q*Q*(pnorm((log(x/K)+(r+vol^2/2)*t)/(vol*sqrt(t)))*x-pnorm((log(x/K)+(r+vol^2/2)*t)/(vol*sqrt(t))- vol*sqrt(t))*K*exp(-r*t)), 
      from = .9*K, to=1.1*K, xlab = "Price of Underlying", ylab = "Position Value", main = "Position Value")
abline(v=S)
abline(v=K)
PosV = C*q*Q 
PosV_ = C_*q*Q
GL = PosV - PosV_
g = toString(round(GL,2))
legend("topright", inset = .05, title = "Gain/Loss", c(g))

