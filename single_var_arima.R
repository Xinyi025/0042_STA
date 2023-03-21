# Convert data to a time series object
single_ts <- ts(train_data[, 1], frequency = 24)

# ADF test
library(tseries)
adf_test <- adf.test(single_ts)
print(adf_test)


# Plot ACF graph
acf(single_ts, lag.max = 24*3, main = "Autocorrelation Function (ACF)")

# Perform seasonal differencing
single_ts.diff <- diff(single_ts, lag=24, differences=1)

# Plot ACF graph of the differenced series
acf(single_ts.diff, lag.max = 24*3, main = "Autocorrelation Function (ACF)")

# Plot PACF graph
pacf(single_ts.diff, lag.max = 24*3,main = "Partial Autocorrelation Function (PACF)")


# Build an ARIMA(4,1,5) model
arima_model <- arima(single_ts, order = c(4, 1, 5))
auto_arima_model  <- auto.arima(single_ts)

# Display model summary
summary(arima_model)
summary(auto_arima_model)

# Forecast n_periods future time points
n_periods <- 24 * 6


# Extract actual values
actual_values <- test_data[, 1]

# Create timestamp column
timestamps <- seq(as.POSIXct("2023-01-26 00:00:00", tz = "UTC"),
                  by = "hour", length.out = n_periods)

# Create a data frame to compare actual and forecasted values
comparison_data <- data.frame(
  Timestamp = c(timestamps, timestamps),
  Value = c(actual_values, predicted_values),
  Source = c(rep("Actual", length(actual_values)),
             rep("Forecast", length(predicted_values)))
)

# Plot line chart
ggplot(comparison_data, aes(x = Timestamp, y = Value, color = Source)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Comparison of Actual and Forecasted Traffic Volume",
       x = "Date",
       y = "Traffic Volume") +
  theme(legend.title = element_blank())
