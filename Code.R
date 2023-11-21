library(tidyquant)
#install.packages("rugarch")
library(rugarch)
library(ggplot2)
library(dplyr)
library(lmtest)


# Get the data for S&P100 firms
getSymbols("^GSPC", from = "2012-01-01", to = "2022-01-01", src = "yahoo")
sp500_returns <- diff(log(Cl(GSPC)))
sp500_returns=sp500_returns[-1]

####volatility clustering####
#plot the time series data
ggplot(sp500_returns, aes(x = index(sp500_returns), y = GSPC.Close)) +
  geom_line(color = "blue") +
  labs(title = "S&P 500 Daily Returns",
       x = "Date",
       y = "Returns") +
  theme_minimal()


#### Leverage effect ####
df <- data.frame(adj_close = GSPC$GSPC.Close,date=index(GSPC))
log_ret <- 100 * sp500_returns
df$log_ret <- c(NA, log_ret)
df$moving_std_252 <- rollapply(df$log_ret, width = 252, FUN = sd, fill = NA, align = "right", na.rm = TRUE)
df$moving_std_21 <- rollapply(df$log_ret, width = 21, FUN = sd, fill = NA, align = "right", na.rm = TRUE)

# Set up the layout for the plots
par(mfrow=c(3,1), mar=c(3,3,2,1))

# Plot SP500 Time Series
plot(df$date,df$GSPC.Close, main="SP500 Time Series", ylab="Price($)",type="l")

# Plot Log Returns
plot(df$date,df$log_ret, ylab="Log Returns(%)",type="l")

# Plot Moving Volatility with legends
lines(df$date,df$moving_std_252, col="red", lwd=2, type="l")
lines(df$date,df$moving_std_21, col="blue", lwd=2, type="l")
legend("topleft", legend=c("Moving Volatility 252d", "Moving Volatility 21d"), col=c("red", "blue"), lwd=2)



####Simulate ARCH and GARCH series####
simulate_GARCH <- function(n, omega, alpha, beta = 0) {
  set.seed(4)
  # Initialize the parameters
  white_noise <- rnorm(n)
  resid <- rep(0, n)
  variance <- rep(0, n)
  
  for (t in 2:n) {
    # Simulate the variance (sigma squared)
    variance[t] <- omega + alpha * resid[t-1]^2 + beta * variance[t-1]
    # Simulate the residuals
    resid[t] <- sqrt(variance[t]) * white_noise[t]
  }
  
  return(list(resid = resid, variance = variance))
}


# Simulate an ARCH(1) series
arch <- simulate_GARCH(n = 200, omega = 0.1, alpha = 0.7)

# Simulate a GARCH(1,1) series
garch <- simulate_GARCH(n = 200, omega = 0.1, alpha = 0.7, beta = 0.3)

# Create a data frame for plotting
data <- data.frame(
  index = 1:200,
  arch_variance = arch$variance,
  garch_variance = garch$variance
)

# Plot using ggplot2
ggplot(data, aes(x = index)) +
  geom_line(aes(y = arch_variance, color = "ARCH Variance")) +
  geom_line(aes(y = garch_variance, color = "GARCH Variance")) +
  scale_color_manual(values = c("ARCH Variance" = "red", "GARCH Variance" = "blue")) +
  labs(x = "Time", y = "Variance", color = "Variance Type") +
  theme_minimal()

# ####Impact of Beta####
# # First simulated GARCH
# first_garch <- simulate_GARCH(n = 200, omega = 0.1, alpha = 0.3, beta = 0.2)
# first_data <- data.frame(
#   index = 1:200,
#   variance = first_garch$variance,
#   residuals = first_garch$resid
# )
# 
# # Plot first simulated GARCH using ggplot2
# ggplot(first_data, aes(x = index)) +
#   geom_line(aes(y = variance, color = "Variance")) +
#   geom_line(aes(y = residuals, color = "Residuals")) +
#   scale_color_manual(values = c("Variance" = "red", "Residuals" = "deepskyblue")) +
#   labs(x = "Time", y = "Value", color = "Variable") +
#   ggtitle("First simulated GARCH, Beta = 0.2") +
#   theme_minimal()
# 
# # Second simulated GARCH
# second_garch <- simulate_GARCH(n = 200, omega = 0.1, alpha = 0.3, beta = 0.7)
# second_data <- data.frame(
#   index = 1:200,
#   variance = second_garch$variance,
#   residuals = second_garch$resid
# )
# 
# # Plot second simulated GARCH using ggplot2
# ggplot(second_data, aes(x = index)) +
#   geom_line(aes(y = variance, color = "Variance")) +
#   geom_line(aes(y = residuals, color = "Residuals")) +
#   scale_color_manual(values = c("Variance" = "red", "Residuals" = "deepskyblue")) +
#   labs(x = "Time", y = "Value", color = "Variable") +
#   ggtitle("Second simulated GARCH, Beta = 0.7") +
#   theme_minimal()
# 
# 
# ####optimal weights ####
# 
# # Define symbols for the three stock indices
# symbols <- c("^GSPC", "^IXIC", "^DJI")
# 
# # Get historical data for the stock indices
# getSymbols(symbols, from = "2012-01-01", to = "2022-01-01", src = "yahoo")
# 
# # Extract adjusted closing prices for the three indices
# sp500_prices <- Cl(GSPC)
# nasdaq_prices <- Cl(IXIC)
# dow_prices <- Cl(DJI)
# 
# # Calculate daily returns for the three indices
# sp500_returns <- diff(log(sp500_prices))
# nasdaq_returns <- diff(log(nasdaq_prices))
# dow_returns <- diff(log(dow_prices))
# 
# # Create a data frame for the portfolio returns
# portfolio <- data.frame(SP500_Returns = sp500_returns,
#                                 NASDAQ_Returns = nasdaq_returns,
#                                 DowJones_Returns = dow_returns)
# #rownames(portfolio)=index(sp500_returns)
# portfolio=portfolio[-1,]
# 
# # Create the portfolio specification
# port_spec <- portfolio.spec(colnames(portfolio))
# 
# # Add a full investment constraint such that the weights sum to 1
# port_spec <- add.constraint(portfolio =port_spec, type = "full_investment")
# 
# # Add a long only constraint such that the weight of an asset is between 0 and 1
# port_spec <- add.constraint(portfolio = port_spec, type = "long_only")
# 
# # Add an objective to minimize portfolio standard deviation
# port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")
# 
# # Solve the optimization problem
# opt <- optimize.portfolio(portfolio, portfolio = port_spec, optimize_method = "ROI")
# 
# #optimal weights
# weights=opt$weights
# 
# # Calculate weighted portfolio returns
# portfolio$P_ret <- weights[1] * portfolio[,1] +
#   weights[2] * portfolio[,2] +
#   weights[3] * portfolio[,3]
# 
# portfolio$date=rownames(portfolio)
# #portfolio$Date=rownames(portfolio)

####BP-test,ADF test####
adf.test(sp500_returns)

# Linear regression model
lm_model <- lm(sp500_returns ~ lag(sp500_returns, 1) + lag(sp500_returns, 2), data = sp500_returns)

# Perform the Breusch-Pagan test
bptest_result <- bptest(lm_model)
bptest_result


####Basic GARCH model with skewed t-distribution####

# Specify GARCH model assumptions
garch_spec <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)),
                   mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                   distribution.model = "sstd")

# Fit the model
garch_model <- ugarchfit(data = sp500_returns, spec = garch_spec)

print(garch_model)
#garch_model@fit[["matcoef"]]

garch_vol=garch_model@fit[["sigma"]]
# garch_resid=garch_model@fit[["residuals"]]
# hist(garch_resid)

par(mfrow=c(1,1))
ggplot(sp500_returns, aes(x = index(sp500_returns))) +
  geom_line(aes(y = garch_vol, color = "blue"), size = 0.5, linetype = 1, show.legend = TRUE,) +
  geom_line(aes(y = sp500_returns, color = "red"), size = 0.5, linetype = 1, show.legend = TRUE, alpha = 0.4) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Volatility", "Returns")) +
  labs(title = "GJR-GARCH(1,1) Model with Skewed-t Distribution", x = "Date", y = "Value") +
  theme(legend.position = "bottom")



####Rolling Forecast####
n_start=2000
garchroll<-ugarchroll(garch_spec, data = sp500_returns,n.start =n_start, 
                      refit.window="moving", refit.every =10, calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05))

report(garchroll)
#coef(garchroll)
preds<-as.data.frame(garchroll)


plot(abs(preds$Realized), type = "p", col = "blue", xlab = "Observation", ylab = "vol", main = "GARCH vol vs Realized vol")
lines(x = preds$Sigma, col = "red", type="l")
lines(x =  col = "black", type="l")

# #MAE
# e=(preds$Realized-preds$Sigma)/preds$Sigma
# mean(abs(e))
# #MSE
# mse=(e)^2
# mean(abs(e))

####VaR####
var_1=garchroll@forecast[["VaR"]][["alpha(1%)"]]
var_5=garchroll@forecast[["VaR"]][["alpha(5%)"]]


returns=as.numeric(sp500_returns[(n_start+1):length(sp500_returns)])

# Identify exceedance points
exceedance_points1 <- which(returns < var_1)
length(exceedance_points1)
exceedance_points5<- which(returns < var_5)
length(exceedance_points5)

# Plotting
par(mfrow=c(1,1))
plot(returns, type = "l",pch = 16, col = "blue", xlab = "Observation", ylab = "Returns", main = "VaR vs Returns")
lines( var_1, col = "green", type="l")  # Add VaR line
lines(var_5, col = "red", type="l")  # Add VaR line

# Mark exceedance points
points(exceedance_points5, returns[exceedance_points5], col = "red", pch = 16)
points(exceedance_points1, returns[exceedance_points1], col = "green", pch = 16)

# Add legend
legend("topright", legend = c("Returns", "1% VaR","5% VaR", "1% Exceedance","5% Exceedance"), 
       col = c("blue", "green", "red","green","red"), lty = c(1, 1, 1,NA,NA), pch = c(NA, NA, NA,16,16))
#legend("bottomright",legend=c(paste0("1% exceedance ",length(exceedance_points1)),
 #                             paste0("5% exceedance ",length(exceedance_points5))))



####Back-testing####
c_t1=ifelse(returns < var_1,(1+(returns-var_1)^2),0)
QPS1=2*sum(c_t1-0.01)^2/length(returns)

c_t5=ifelse(returns < var_5,(1+(returns-var_5)^2),0)
QPS5=2*sum(c_t5-0.01)^2/length(returns)




