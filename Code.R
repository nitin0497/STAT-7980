library(tidyquant)
#install.packages("rugarch")
library(rugarch)
library(ggplot2)
library(dplyr)


# Get the data for S&P100 firms
sp500 = tq_get("^GSPC",from="2000-01-04",to="2023-01-04")

sp500_returns <- sp500 %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               col_rename = "returns",
               period = "daily",
               col_rename_timestamp = "date")

#sp500_returns <- sp500_returns[-1]    # Remove NA in the first position

####volatility clustering####
#plot the time series data
ggplot(sp500_returns, aes(x = date, y = returns)) +
  geom_line(color = "blue") +
  labs(title = "S&P 500 Daily Returns",
       x = "Date",
       y = "Returns") +
  theme_minimal()

#plot histogram of returns 
ggplot(sp500_returns, aes(x = returns)) +
  geom_histogram(binwidth = 0.0005, fill = "blue", color = "black") +
  labs(title = "Histogram of S&P 500 Returns",
       x = "Returns",
       y = "Frequency") +
  theme_minimal()

#####Leverage effect#####
# Calculate daily log-returns
log_prices <- log(sp500$close)

# Calculate 20-day recursive standard deviation of log-returns
recursive_sd <- rollapply(sp500_returns$returns, width = 22, FUN = sd, align = "right", fill = NA)

# Create a data frame for plotting
plot_data <- data.frame(Date = sp500$date, LogPrices = log_prices, RecursiveSD = c(recursive_sd))

# Plot log-returns and recursive standard deviation with separate y-axes
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = LogPrices, color = "Log-Returns"), size = 1) +
  geom_line(aes(y = RecursiveSD*100, color = "Recursive SD"), size = 1) +
  scale_color_manual(values = c("LogPrices" = "blue", "Recursive SD" = "red")) +
  labs(y = "LogPrices", x = "Date", title = "S&P500 LogPrices and Recursive Standard Deviation") +
  theme_minimal() +
  scale_y_continuous(name = "LogPrices", sec.axis = sec_axis(~ .*0.01, name = "Recursive SD"))

####ARCH MODEL####







####GARCH MODEL####

# Specify the GARCH(1,1) model
garch_spec <- ugarchspec(variance.model = list(model= "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
                         distribution.model = "std")

# Fit the GARCH model
garch_fit <- ugarchfit(spec = garch_spec, data = unlist(sp500_returns),outsample=100)

# Fit the GARCH model
arch_fit <- ugarchfit(spec = arch_spec, data = unlist(sp500_returns),outsample=100)
print(arch_fit)

# Print model summary
print(garch_fit)
print(coef(garch.fit))

# Forecast volatility for the next n days
n <- 10
#forecast_volatility <- ugarchforecast(garch_fit, n.ahead = n, n.roll=100)
garch.forecast <- ugarchforecast(garch_fit, data=NULL, n.roll=100, n.ahead=10)
# Print forecast
print(garch.forecast)

#forecast_volatility <- ugarchforecast(garch_fit, n.ahead = n, n.roll=100)
arch.forecast <- ugarchforecast(arch_fit, data=NULL, n.roll=10, n.ahead=10)
# Print forecast
print(arch.forecast)


# Load the rugarch package
library(rugarch)

# Define the order of the ARCH model (p)
arch_order <- 1  # For example, an ARCH(1) model

# Define the ARCH(p) model using ugarchspec
arch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0, 1)))

# Print the ARCH specification
print(arch_spec)


