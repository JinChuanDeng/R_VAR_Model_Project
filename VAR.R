# import csv file
data <- read.csv("C:\\Users\\10717\\Desktop\\introduce report\\data.csv")

# Convert the Date column to time format
data$Date <- as.Date(as.yearqtr(data$Date, format = "%Y Q%q"))

# Make sure data is sorted by time
data <- data[order(data$Date), ]

# Convert to time series
ts_data <- ts(
  data[, -1],            
  start = c(1991, 1),    
  frequency = 4          
)

# Plot each variable in the time series
par(mfrow = c(2, 2))  # Divide the plotting area into a 2x2 grid
plot(ts_data[, "GDP_Growth"], main = "GDP Growth", ylab = "Growth Rate", xlab = "Time")
plot(ts_data[, "Agriculture_Growth"], main = "Agriculture Growth", ylab = "Growth Rate", xlab = "Time")
plot(ts_data[, "Manufacturing_Growth"], main = "Manufacturing Growth", ylab = "Growth Rate", xlab = "Time")
plot(ts_data[, "Service_Growth"], main = "Service Growth", ylab = "Growth Rate", xlab = "Time")
par(mfrow = c(1, 1))  # Reset the plotting layout


install.packages("tseries")
library(tseries)

# Perform ADF test for each variable
adf_gdp <- adf.test(ts_data[, "GDP_Growth"])
adf_agriculture <- adf.test(ts_data[, "Agriculture_Growth"])
adf_manufacturing <- adf.test(ts_data[, "Manufacturing_Growth"])
adf_service <- adf.test(ts_data[, "Service_Growth"])

# Print the ADF test results
print(adf_gdp)
print(adf_agriculture)
print(adf_manufacturing)
print(adf_service)

library(urca)

# Perform KPSS test for Agriculture Growth
kpss_agriculture <- ur.kpss(ts_data[, "Agriculture_Growth"])
summary(kpss_agriculture)


library(vars)

# Use VARselect() to determine the optimal lag order
lag_selection <- VARselect(ts_data, lag.max = 10, type = "const")

# Print the results
print(lag_selection)

# Fit the VAR model with lag order 5
var_model <- VAR(ts_data, p = 5, type = "const")

# Summary of the model
summary(var_model)

# Granger causality test for Agriculture_Growth -> GDP_Growth
granger_agriculture <- causality(var_model, cause = "Agriculture_Growth")
print(granger_agriculture)

# Granger causality test for Manufacturing_Growth -> GDP_Growth
granger_manufacturing <- causality(var_model, cause = "Manufacturing_Growth")
print(granger_manufacturing)

# Granger causality test for Service_Growth -> GDP_Growth
granger_service <- causality(var_model, cause = "Service_Growth")
print(granger_service)

# Granger causality test for Manufacturing_Growth -> Service_Growth
granger_manu_service <- causality(var_model, cause = "Manufacturing_Growth")
print(granger_manu_service)

# Impulse response: Manufacturing Growth -> GDP Growth
irf_manu_gdp <- irf(var_model, impulse = "Manufacturing_Growth", response = "GDP_Growth", n.ahead = 10, boot = TRUE)
plot(irf_manu_gdp, main = "Impulse Response: Manufacturing -> GDP")

# Impulse response: Manufacturing Growth -> Service Growth
irf_manu_service <- irf(var_model, impulse = "Manufacturing_Growth", response = "Service_Growth", n.ahead = 10, boot = TRUE)
plot(irf_manu_service, main = "Impulse Response: Manufacturing -> Service")

# Impulse response: Service Growth -> GDP Growth
irf_serv_gdp <- irf(var_model, impulse = "Service_Growth", response = "GDP_Growth", n.ahead = 10, boot = TRUE)
plot(irf_serv_gdp, main = "Impulse Response: Service -> GDP")

# Variance Decomposition
var_decomp <- fevd(var_model, n.ahead = 10)  # Decompose for 10 future periods

# Print the results
print(var_decomp)

# Plot Variance Decomposition
plot(var_decomp)



# Split data into training (80%) and testing (20%)
train_size <- floor(0.8 * nrow(ts_data))
train_data <- ts_data[1:train_size, ]
test_data <- ts_data[(train_size + 1):nrow(ts_data), ]

# Fit VAR model on training data
var_model_train <- VAR(train_data, p = 5, type = "const")

# Forecast using the trained VAR model
forecast_train <- predict(var_model_train, n.ahead = nrow(test_data))

# Extract predicted GDP growth from the forecast
predicted_gdp <- forecast_train$fcst$GDP_Growth[, "fcst"]

# Calculate RMSE for validation
actual_gdp <- test_data[, "GDP_Growth"]
rmse <- sqrt(mean((actual_gdp - predicted_gdp)^2))
cat("RMSE on test data:", rmse, "\n")

# Refit VAR model on the entire dataset
var_model_full <- VAR(ts_data, p = 5, type = "const")

# Forecast future GDP growth for 4 periods
future_forecast <- predict(var_model_full, n.ahead = 4)

# Extract future predictions and confidence intervals
future_gdp <- future_forecast$fcst$GDP_Growth
print(future_gdp)

#Convert to data frame
if (!is.data.frame(ts_data)) {
  ts_data <- as.data.frame(ts_data)
}
# Combine actual and predicted data
time_index <- c(1:nrow(ts_data), (nrow(ts_data) + 1):(nrow(ts_data) + 4))
gdp_data <- c(ts_data$GDP_Growth, rep(NA, 4))
gdp_pred <- c(rep(NA, nrow(ts_data)), future_gdp[, "fcst"])
data_plot <- data.frame(
  Time = time_index,
  GDP_Actual = gdp_data,
  GDP_Predicted = gdp_pred
)

# Plot
ggplot(data = data_plot, aes(x = Time)) +
  geom_line(aes(y = GDP_Actual, color = "Actual GDP Growth")) +
  geom_line(aes(y = GDP_Predicted, color = "Predicted GDP Growth")) +
  labs(title = "Actual vs Predicted GDP Growth",
       y = "GDP Growth Rate",
       x = "Time") +
  scale_color_manual(values = c("Actual GDP Growth" = "blue", "Predicted GDP Growth" = "red")) +
  theme_minimal()

# Extract actual GDP growth from the test data
actual_gdp <- test_data[, "GDP_Growth"]

# Extract predicted GDP growth for the test data
predicted_gdp <- forecast_train$fcst$GDP_Growth[, "fcst"]

# Calculate performance metrics
# Mean Squared Error (MSE)
mse <- mean((actual_gdp - predicted_gdp)^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Mean Absolute Error (MAE)
mae <- mean(abs(actual_gdp - predicted_gdp))

# R-squared (R²)
ss_total <- sum((actual_gdp - mean(actual_gdp))^2)
ss_residual <- sum((actual_gdp - predicted_gdp)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Print performance metrics
cat("Performance Metrics:\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared (R²):", r_squared, "\n")

# Remove outliers: 2020 Q2 (2020-04-01) and 2020 Q3 (2020-07-01)
ts_data_cleaned <- ts_data[-c(118, 119), ]

# Verify removal
print(paste("Rows removed: ", nrow(ts_data) - nrow(ts_data_cleaned)))

# Split data into training (80%) and testing (20%)
train_size <- floor(0.8 * nrow(ts_data_cleaned))
train_data <- ts_data_cleaned[1:train_size, ]
test_data <- ts_data_cleaned[(train_size + 1):nrow(ts_data_cleaned), ]

# Fit VAR model on training data
var_model_train <- VAR(train_data, p = 5, type = "const")

# Forecast using the trained VAR model
forecast_train <- predict(var_model_train, n.ahead = nrow(test_data))

# Extract predicted GDP growth from the forecast
predicted_gdp <- forecast_train$fcst$GDP_Growth[, "fcst"]

# Calculate RMSE for validation
actual_gdp <- test_data[, "GDP_Growth"]
rmse <- sqrt(mean((actual_gdp - predicted_gdp)^2))
cat("RMSE on test data after removing rows 118 and 119:", rmse, "\n")

# Refit VAR model on the entire cleaned dataset
var_model_full <- VAR(ts_data_cleaned, p = 5, type = "const")

# Forecast future GDP growth for 4 periods
future_forecast <- predict(var_model_full, n.ahead = 4)

# Extract future predictions and confidence intervals
future_gdp <- future_forecast$fcst$GDP_Growth
print(future_gdp)

# Convert to data frame (if not already)
if (!is.data.frame(ts_data_cleaned)) {
  ts_data_cleaned <- as.data.frame(ts_data_cleaned)
}

# Combine actual and predicted data
time_index <- c(1:nrow(ts_data_cleaned), (nrow(ts_data_cleaned) + 1):(nrow(ts_data_cleaned) + 4))
gdp_data <- c(ts_data_cleaned$GDP_Growth, rep(NA, 4))
gdp_pred <- c(rep(NA, nrow(ts_data_cleaned)), future_gdp[, "fcst"])
data_plot <- data.frame(
  Time = time_index,
  GDP_Actual = gdp_data,
  GDP_Predicted = gdp_pred
)

# Plot
ggplot(data = data_plot, aes(x = Time)) +
  geom_line(aes(y = GDP_Actual, color = "Actual GDP Growth")) +
  geom_line(aes(y = GDP_Predicted, color = "Predicted GDP Growth")) +
  labs(title = "Actual vs Predicted GDP Growth After Removing Rows 118 and 119",
       y = "GDP Growth Rate",
       x = "Time") +
  scale_color_manual(values = c("Actual GDP Growth" = "blue", "Predicted GDP Growth" = "red")) +
  theme_minimal()

