library(tidyquant)
#install.packages("rugarch")
library(rugarch)
library(ggplot2)
library(dplyr)
library(PortfolioAnalytics)
library(fGarch)


# Get the data for S&P100 firms
sp500 = tq_get("^GSPC",from="2017-01-01",to="2022-01-01")

sp500_returns <- sp500 %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               col_rename = "returns",
               period = "daily",
               col_rename_timestamp = "date")


####volatility clustering####
#plot the time series data
ggplot(sp500_returns, aes(x = date, y = returns)) +
  geom_line(color = "blue") +
  labs(title = "S&P 500 Daily Returns",
       x = "Date",
       y = "Returns") +
  theme_minimal()


#### Leverage effect ####
#df <- quantmod::getSymbols(market, from = start_m, to = end_m, auto.assign = FALSE)[, "GSPC.Adjusted"]
df <- data.frame(adj_close = sp500$adjusted,date=sp500$date)
log_ret <- 100 * diff(log(sp500$adjusted))
df$log_ret <- c(NA, log_ret)
df$moving_std_252 <- rollapply(df$log_ret, width = 252, FUN = sd, fill = NA, align = "right", na.rm = TRUE)
df$moving_std_21 <- rollapply(df$log_ret, width = 21, FUN = sd, fill = NA, align = "right", na.rm = TRUE)

# Set up the layout for the plots
par(mfrow=c(3,1), mar=c(3,3,2,1))

# Plot SP500 Time Series
plot(df$date,df$adj_close, main="SP500 Time Series", ylab="Price($)",type="l")


# Plot Log Returns
plot(df$log_ret, ylab="Log Returns(%)",type="l")

# Plot Moving Volatility with legends
lines(df$moving_std_252, col="red", lwd=2, type="l")
lines(df$moving_std_21, col="blue", lwd=2, type="l")
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

####Impact of Beta####
# First simulated GARCH
first_garch <- simulate_GARCH(n = 200, omega = 0.1, alpha = 0.3, beta = 0.2)
first_data <- data.frame(
  index = 1:200,
  variance = first_garch$variance,
  residuals = first_garch$resid
)

# Plot first simulated GARCH using ggplot2
ggplot(first_data, aes(x = index)) +
  geom_line(aes(y = variance, color = "Variance")) +
  geom_line(aes(y = residuals, color = "Residuals")) +
  scale_color_manual(values = c("Variance" = "red", "Residuals" = "deepskyblue")) +
  labs(x = "Time", y = "Value", color = "Variable") +
  ggtitle("First simulated GARCH, Beta = 0.2") +
  theme_minimal()

# Second simulated GARCH
second_garch <- simulate_GARCH(n = 200, omega = 0.1, alpha = 0.3, beta = 0.7)
second_data <- data.frame(
  index = 1:200,
  variance = second_garch$variance,
  residuals = second_garch$resid
)

# Plot second simulated GARCH using ggplot2
ggplot(second_data, aes(x = index)) +
  geom_line(aes(y = variance, color = "Variance")) +
  geom_line(aes(y = residuals, color = "Residuals")) +
  scale_color_manual(values = c("Variance" = "red", "Residuals" = "deepskyblue")) +
  labs(x = "Time", y = "Value", color = "Variable") +
  ggtitle("Second simulated GARCH, Beta = 0.7") +
  theme_minimal()


####optimal weights ####

# Define symbols for the three stock indices
symbols <- c("^GSPC", "^IXIC", "^DJI")

# Get historical data for the stock indices
getSymbols(symbols, from = "2012-01-01", to = "2022-01-01", src = "yahoo")

# Extract adjusted closing prices for the three indices
sp500_prices <- Cl(GSPC)
nasdaq_prices <- Cl(IXIC)
dow_prices <- Cl(DJI)

# Calculate daily returns for the three indices
sp500_returns <- diff(log(sp500_prices))
nasdaq_returns <- diff(log(nasdaq_prices))
dow_returns <- diff(log(dow_prices))

# Create a data frame for the portfolio returns
portfolio <- data.frame(SP500_Returns = sp500_returns,
                                NASDAQ_Returns = nasdaq_returns,
                                DowJones_Returns = dow_returns)
portfolio=portfolio[-1,]

# Create the portfolio specification
port_spec <- portfolio.spec(colnames(portfolio))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio =port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Solve the optimization problem
opt <- optimize.portfolio(portfolio, portfolio = port_spec, optimize_method = "ROI")

#optimal weights
weights=opt$weights

# Calculate weighted portfolio returns
portfolio$P_ret <- weights[1] * portfolio[,1] +
  weights[2] * portfolio[,2] +
  weights[3] * portfolio[,3]

portfolio$Date=rownames(portfolio)

####Basic GARCH model with skewed t-distribution####
# Specify GARCH model assumptions
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                   distribution.model = "std")

# Fit the model
garch_model <- ugarchfit(data = portfolio$P_ret, spec = spec)

print(garch_model)
# Get model estimated volatility
garch_vol <- sigma(garch_model)
#garch_vol$Date=index(garch_vol)

ggplot(portfolio, aes(x = as.Date(portfolio$Date))) +
  geom_line(aes(y = garch_vol, color = "blue"), size = 0.5, linetype = 1, show.legend = TRUE, label = "Skewed-t Volatility") +
  geom_line(aes(y = portfolio$P_ret, color = "red"), size = 0.5, linetype = 1, show.legend = TRUE, alpha = 0.4, label = "Daily Returns") +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Skewed-t Volatility", "Portfolio Returns")) +
  theme_minimal() +
  labs(title = "GARCH(1,1) Model with Skewed-t Distribution",
       x = "Date",
       y = "Value") +
  theme(legend.position = "right")

####Rolling Forecast####
returns_xts <- xts(portfolio$P_ret, order.by = as.Date(portfolio$Date))
window_size <- 252 * 9  # Assuming daily data (252 trading days in a year)

# Initialize an empty dataframe to store forecasts
forecast_vol <- c()
actual_vol=c()
date=c()
var_df=data.frame()
# Perform rolling forecast
for (i in window_size:(length(index(returns_xts))-1)) {
  
  # Extract the current window of data
  window_data <- returns_xts[(i - window_size + 1):i]
  
  # Fit GARCH(1,1) model to the current window of data
  garch_model <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                            mean.model = list(armaOrder = c(0, 0)),
                            distribution.model = "std")
  
  garch_fit <- ugarchfit(spec = garch_model, data = window_data)
  
  # Forecast volatility for the next day
  forecast <- ugarchforecast(garch_fit, n.ahead = 1)
  forecast_values <- sigma(forecast)
  forecast_vol=c(forecast_vol,forecast_values)
  date=c(index(window_data[nrow(window_data)]),date)

  #corresponding actual volatility
  actual_data <- returns_xts[(i - window_size + 2):i+1]
  vol=sd(actual_data)
  actual_vol=c(actual_vol,vol)
  
  # Extract conditional mean and variance
  cond_mean <- as.numeric(forecast@forecast$seriesFor)
  cond_var <- as.numeric(sigma(forecast))
  q <- qnorm(c(0.01, 0.05))
  var <- -cond_mean - sqrt(cond_var) * q
  var_val <- data.frame('1%' = var[1], '5%' = var[2])
  var_df=rbind(var_df,var_val)

}


####Backtesting with MAE and MSE####
MAE=sum((actual_vol-forecast_vol))/length(actual_vol)
MSE=sum((actual_vol-forecast_vol)^2)/length(actual_vol)

plot_data=data.frame(date,forecast_vol,actual_vol,var_df)

# Plot using ggplot2
ggplot(plot_data, aes(x = date)) +
  geom_line(aes(y = actual_vol, color = 'Actual Volatility'), alpha = 0.4) +
  geom_line(aes(y = forecast_vol, color = 'GARCH Volatility')) +
  xlab('Date') +
  ylab('Volatility') +
  ggtitle('Volatility Comparison') +
  scale_color_manual(values = c('Actual Volatility' = 'blue', 'GARCH Volatility' = 'red')) +
  theme_minimal() +
  theme(legend.position = 'top')




####VaR####

# Assuming rets_2019 is your time series data in R
# Replace "rets_2019" with your actual data frame or vector name

rets_2019=returns_xts[year(index(returns_xts)) == 2021, ]
var_2019=plot_data[year(plot_data$date) == 2021, ]
# Create color and label vectors
colors <- character()
labels <- character()

for (i in 1:nrow(var_2019)) {
  if (rets_2019[i] > -var_2019[i, 5]) {
    colors <- c(colors, '#000000')  # black color for no exceedence
    labels <- c(labels, 'No Exceedence')
    print(1)
  } else if (rets_2019[i] < -var_2019[i, 4]) {
    colors <- c(colors, '#BB0000')  # red color for 1% exceedence
    labels <- c(labels, '1% Exceedence')
    print(2)
  } else {
    colors <- c(colors, '#BB00BB')  # purple color for 5% exceedence
    labels <- c(labels, '5% Exceedence')
  }
}

# Create a data frame for plotting
var_plot_data <- data.frame(Date = index(rets_2019),
                        Returns = -rets_2019,
                        Color = as.factor(colors),
                        Label = factor(labels))


ggplot(var_plot_data, aes(x = Date, y = Returns, color = Color, shape = Label)) +
  geom_point() +
  scale_color_manual(values = c('#000000', '#BB0000', '#BB00BB')) +
  scale_shape_manual(values = c('No Exceedence' = 1, '1% Exceedence' = 3, '5% Exceedence' = 15)) +
  labs(title = 'Parametric VaR', x = 'Date', y = 'Returns') +
  theme_minimal() +
  theme(legend.position = 'top', legend.title = element_blank(), legend.spacing.x = unit(0.1, 'cm'))






