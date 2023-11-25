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

n=200
# Simulate an ARCH(1) series
arch <- simulate_GARCH(n = n, omega = 0.1, alpha = 0.7)

# Simulate a GARCH(1,1) series
garch <- simulate_GARCH(n = n, omega = 0.1, alpha = 0.7, beta = 0.3)

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


#extracting the model coefficients to verify
#ARCH
sim_arch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)),
                         mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                         distribution.model = "norm")

sim_arch_model <- ugarchfit(data = arch$resid, spec = sim_arch_spec)
sim_arch_model@fit[["matcoef"]][,1]

#GARCH
sim_garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
                         mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                         distribution.model = "norm")

# Fit the model
sim_garch_model <- ugarchfit(data = garch$resid, spec = sim_garch_spec)
sim_garch_model@fit[["matcoef"]][,1]


####BP-test,ADF test####
adf.test(sp500_returns)

# Linear regression model
lm_model <- lm(sp500_returns ~ lag(sp500_returns, 1) + lag(sp500_returns, 2), data = sp500_returns)

# Perform the Breusch-Pagan test
bptest(lm_model)



####GJR-GARCH model with skewed t-distribution####

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
  geom_line(aes(y = garch_vol, color = "blue"), linewidth = 0.5, linetype = 1, show.legend = TRUE,) +
  geom_line(aes(y = sp500_returns, color = "red"), linewidth = 0.5, linetype = 1, show.legend = TRUE, alpha = 0.4) +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Volatility", "Returns")) +
  labs(title = "GJR-GARCH(1,1) Model with Skewed-t Distribution", x = "Date", y = "Value") +
  theme(legend.position = "bottom")



####Rolling Forecast####
n_start=252*5
garchroll<-ugarchroll(garch_spec, data = sp500_returns,n.start =n_start, 
                      refit.window="moving", refit.every =5, calculate.VaR = TRUE, VaR.alpha = c(0.01,0.05))

report(garchroll)
preds<-as.data.frame(garchroll)


# plot(abs(preds$Realized), type = "l", col = "blue", xlab = "Observation", ylab = "vol", main = "GARCH vol vs Realized vol")
# lines(x = preds$Sigma, col = "red", type="l")
# lines(x =  col = "black", type="l")

#MSE
e=(preds$Realized-preds$Sigma)/preds$Sigma
mean(e^2)


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
points(exceedance_points1, returns[exceedance_points1], col = "black", pch = 16)

# Add legend
legend("topright", legend = c("Returns", "1% VaR","5% VaR", "1% Exceedance","5% Exceedance"), 
       col = c("blue", "green", "red","black","red"), lty = c(1, 1, 1,NA,NA), pch = c(NA, NA, NA,16,16))
legend("bottomright",legend=c(paste0("1% exceedance ",length(exceedance_points1))))



####Back-testing####
c_t1=ifelse(returns < var_1,(1+(returns-var_1)^2),0)
QPS1=2*sum(c_t1-0.01)^2/length(returns)

c_t5=ifelse(returns < var_5,(1+(returns-var_5)^2),0)
QPS5=2*sum(c_t5-0.01)^2/length(returns)




