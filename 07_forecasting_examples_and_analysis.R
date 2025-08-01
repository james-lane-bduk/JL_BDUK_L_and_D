#Load fpp3 package, which attaches dplyr, tidyr, lubridate etc, and also load zoo for moving averages, and forecast
library(fpp3)
library(zoo)
library(forecast)
library(patchwork)

#Focus on US Employment Data Time Series in Retail Trade, from 1980 onwards, which is monthly data 
us_employment_1980 <- us_employment %>%
  filter(year(Month) >= 1980, Title == 'Retail Trade') %>%
  mutate(N_Employed_Millions = Employed/1e3)
  

#1) Exploratory Analysis


#1a) Explore US Employment Data Time Series, from 1980 onwards
ggplot(us_employment_1980, aes(x = Month, y = N_Employed_Millions)) +
  geom_line(linewidth = 1.0, colour = '#12346D') +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("The Number of US Employees in Retail Trade (Millions), by month")



#This time series exhibits all three:
#-The gradual, upward increase (e.g. from around 10m in 1980, to 16m in 2020) - Trend
#-The annual 'spikes' in the summer - Seasonality
#-The broader 'peaks and troughs' (e.g. early 1990s, near 2010) - potentially related to the economy


#1b) As above, but now create seasonal plot:
us_employment_1980_for_seasonal <- us_employment_1980 %>%
  as_tibble() %>%
  mutate(year = as.factor(year(Month)), 
         Month = month(Month, label = TRUE, abbr = TRUE))

ggplot(us_employment_1980_for_seasonal, aes(x = Month, y = N_Employed_Millions, group = year, colour = year)) +
  geom_line(linewidth = 1.0) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("The Number of US Employees in Retail Trade (Millions), by month")




#1c) Lag plot of no. of employees vs no. of employees in previous months:
us_employment_1980 %>%
  gg_lag(N_Employed_Millions, geom = "point") +
  labs(x = "lag(N_Employed_Millions, k)")

#Lag 1 shows e.g. plots for No. Employed in Feb vs No. employed in Jan and No. Employed in Mar vs No.employed in Feb, etc.
#Strongly positive correlations in the lags indicate strong seasonality in data


#1d) Check autocorrelation via correlogram
us_employment_1980 %>%
  ACF(N_Employed_Millions, lag_max = 120) %>%
  autoplot()

#This time series has very strong correlation at all lags (going back 10 years, 120 months), indicating both trend and seasonality
#Small bumps at the 12-month intervals in particular indicate the seasonality



#1e) Break down components of time-series via STL
dcmp <- us_employment_1980 %>%
  model(stl = STL(N_Employed_Millions))

components(dcmp) %>%
  autoplot()



#1f) Apply simple centered moving average to get trend of employment data (classical decomposition)
#-Compute 12 month moving average
us_employment_1980_mov_avg <- us_employment_1980 %>%
  mutate(N_Employed_Millions_12MA = rollapply(N_Employed_Millions, width = 12, FUN = mean, align = "center", fill = NA))


#Plot moving avg, along with the data:
ggplot(us_employment_1980_mov_avg, aes(x = Month, y = N_Employed_Millions)) +
  geom_line(linewidth = 1.0, colour = '#12346D') +
  geom_line(aes(y = N_Employed_Millions_12MA), colour = 'orange', linewidth = 1.5) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("The Number of US Employees in Retail Trade (Millions), by month")



#1g) Perform classical decomposition manually, to compare with built-in function:
#E.g. 13-month MA (i.e. current month +/-6)
us_employment_1980_decomp_pt1 <- us_employment_1980 %>%
  as_tibble() %>%
  mutate(trend = rollapply(N_Employed_Millions, width = 13, FUN = mean, align = "center", fill = NA),
         detrended = N_Employed_Millions - trend) %>%
  mutate(Month = month(Month, label = TRUE, abbr = TRUE))

#Seasonal component 
us_employment_1980_decomp_pt2 <- us_employment_1980_decomp_pt1 %>%
  group_by(Month) %>%
  summarise(seasonal = mean(detrended, na.rm=TRUE))

#Link back (many-to-one join on Month) and use to get remainder
us_employment_1980_final <- us_employment_1980_decomp_pt1 %>%
  left_join(us_employment_1980_decomp_pt2, by = "Month") %>%
  mutate(remainder = detrended - seasonal)


#Compare to function approach:
us_employment_1980 %>%
  model(classical_decomposition(N_Employed_Millions, type = "additive")) %>%
  components() %>%
  head(10)

us_employment_1980_final %>%
  head(10)



#----------------------------------------------------


#2) Simple Forecasting Methods - forecasting for the next 12 months
us_employment_1980_ts <- ts(us_employment_1980$N_Employed_Millions, start = c(1980,01), frequency=12)

#2a) Mean Forecast - future values = mean of past observations
mean_fc <- meanf(us_employment_1980_ts, h = 12)

#Residuals based on standard deviation of residuals, i.e.
sd_res <- sd(us_employment_1980_ts - mean(us_employment_1980_ts))

pct_95 <- mean(us_employment_1980_ts) + 1.96*(sd_res)


#Turn Time series objects back into dataframes to plot
mean_fc_df <- data.frame(Month = as.Date(time(us_employment_1980_ts)),
                         N_Employed_Millions = as.numeric(us_employment_1980_ts)) %>%
  mutate(type = "data")

forecast_mean_fc_df <- data.frame(
    Month = as.Date(time(mean_fc$mean)),
    N_Employed_Millions = as.numeric(mean_fc$mean),
    lower_95 = mean_fc$lower[, "95%"],
    upper_95 = mean_fc$upper[, "95%"]) %>%
  mutate(type = "forecast")
  

final_mean_fc_df <- mean_fc_df %>%
  bind_rows(forecast_mean_fc_df) %>%
  as_tibble()

mean_forecast_plot <- ggplot(data = final_mean_fc_df, aes(x = Month, y = N_Employed_Millions)) +
  geom_line(aes(colour = type), linewidth = 1.0) +
  scale_colour_manual(values = c("data" = 'black', "forecast" = "#12346D")) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = '#12346D', alpha = 0.3) +
  theme_minimal() +
  theme(text = element_text(size = 11)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("Mean")




#2b) Naive Forecast - future values = most recent observation
naive_fc <- naive(us_employment_1980_ts, h = 12)

#Turn Time series objects back into dataframes to plot
naive_fc_df <- data.frame(Month = as.Date(time(us_employment_1980_ts)),
                         N_Employed_Millions = as.numeric(us_employment_1980_ts)) %>%
  mutate(type = "data")

forecast_naive_fc_df <- data.frame(
  Month = as.Date(time(naive_fc$mean)),
  N_Employed_Millions = as.numeric(naive_fc$mean),
  lower_95 = naive_fc$lower[, "95%"],
  upper_95 = naive_fc$upper[, "95%"]) %>%
  mutate(type = "forecast")


final_naive_fc_df <- naive_fc_df %>%
  bind_rows(forecast_naive_fc_df) %>%
  as_tibble()

naive_forecast_plot <- ggplot(data = final_naive_fc_df, aes(x = Month, y = N_Employed_Millions)) +
  geom_line(aes(colour = type), linewidth = 1.0) +
  scale_colour_manual(values = c("data" = 'black', "forecast" = "#12346D")) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = '#12346D', alpha = 0.3) +
  theme_minimal() +
  theme(text = element_text(size = 11)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("Naive")




#2c) Seasonal Naive Forecast - future values = most recent observation from same season (here, season being 'month')
snaive_fc <- snaive(us_employment_1980_ts, h = 12)

#Turn Time series objects back into dataframes to plot
snaive_fc_df <- data.frame(Month = as.Date(time(us_employment_1980_ts)),
                          N_Employed_Millions = as.numeric(us_employment_1980_ts)) %>%
  mutate(type = "data")

forecast_snaive_fc_df <- data.frame(
  Month = as.Date(time(snaive_fc$mean)),
  N_Employed_Millions = as.numeric(snaive_fc$mean),
  lower_95 = snaive_fc$lower[, "95%"],
  upper_95 = snaive_fc$upper[, "95%"]) %>%
  mutate(type = "forecast")


final_snaive_fc_df <- snaive_fc_df %>%
  bind_rows(forecast_snaive_fc_df) %>%
  as_tibble()

snaive_forecast_plot <- ggplot(data = final_snaive_fc_df, aes(x = Month, y = N_Employed_Millions)) +
  geom_line(aes(colour = type), linewidth = 1.0) +
  scale_colour_manual(values = c("data" = 'black', "forecast" = "#12346D")) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = '#12346D', alpha = 0.3) +
  theme_minimal() +
  theme(text = element_text(size = 11)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("Seasonal Naive")



#2d) Drift Forecast - variation on naive uses straight line between most recent and first observation and extrapolates into future
drift_fc <- rwf(us_employment_1980_ts, h = 12, drift=TRUE)

#Turn Time series objects back into dataframes to plot
drift_fc_df <- data.frame(Month = as.Date(time(us_employment_1980_ts)),
                           N_Employed_Millions = as.numeric(us_employment_1980_ts)) %>%
  mutate(type = "data")

forecast_drift_fc_df <- data.frame(
  Month = as.Date(time(drift_fc$mean)),
  N_Employed_Millions = as.numeric(drift_fc$mean),
  lower_95 = drift_fc$lower[, "95%"],
  upper_95 = drift_fc$upper[, "95%"]) %>%
  mutate(type = "forecast")


final_drift_fc_df <- drift_fc_df %>%
  bind_rows(forecast_drift_fc_df) %>%
  as_tibble()

drift_forecast_plot <- ggplot(data = final_drift_fc_df, aes(x = Month, y = N_Employed_Millions)) +
  geom_line(aes(colour = type), linewidth = 1.0) +
  scale_colour_manual(values = c("data" = 'black', "forecast" = "#12346D")) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = '#12346D', alpha = 0.3) +
  theme_minimal() +
  theme(text = element_text(size = 11)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("Drift")


#Combine plots

simple_forecast_plots <- mean_forecast_plot / naive_forecast_plot / snaive_forecast_plot / drift_forecast_plot



#2e) Evaluating model fit and residuals - fitted models and residuals - e.g. look at drift model
drift_fc_fitted <- drift_fc$fitted
drift_fc_residuals <- drift_fc$residuals


#Plot fitted values with actual values:
drift_fc_df_add_fitted_and_resid <- drift_fc_df %>%
  mutate(fitted = as.vector(drift_fc_fitted),
         residuals = as.vector(drift_fc_residuals))

ggplot(data = drift_fc_df_add_fitted_and_resid, aes(x = Month)) +
  geom_line(aes(y = N_Employed_Millions, colour = "data"), linewidth = 1.0) +
  geom_line(aes(y = fitted, colour = "fitted"), linewidth = 1.0) +
  scale_colour_manual(values = c("data" = "black", "fitted" = "#12346D")) +
  theme_minimal() +
  theme(text = element_text(size = 11)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("Drift")


#Fitted value y^(hat)_t = actual data value at y_(t-1) + drift term - explains why it looks like it's shifted across and moved up/down slightly



#Are residuals uncorrelated in time (no autocorrelation)?
Acf(drift_fc_df_add_fitted_and_resid$residuals) %>%
  autoplot()



#Are residuals normally distributed?
ggplot(data = drift_fc_df_add_fitted_and_resid, aes(x = residuals)) +
  geom_histogram() +
  theme_minimal() +
  theme(text = element_text(size = 11)) +
  xlab("Residual Value") +
  ylab("No. of Observations")


#Compute RMSE - #This is equivalent to looking at prediction error of the training data, as these data points were involved in 'fitting' the model
drift_fc_df_add_fitted_and_resid %>%
  mutate(residuals2 = residuals^2) %>%
  summarise(rmse = sqrt(mean(residuals2, na.rm=TRUE)))

#Approx 0.22 million - so on avg, forecasted value approx 0.22 million employees away from actual


#Do the residuals look like white noise?
Box.test(drift_fc_df_add_fitted_and_resid$residuals, lag = 24, type = "Box-Pierce")



#Very small p-value - almost 0% chance we'd see autocorrelation like this if residuals actually resembled white noise
#Peaks at lags 12 and 24 - indicative of yearly seasonality




#-------------



#2e) Evaluating model performance - this time, hold 18 most recent months back

#'Training' data
us_employment_1980_training_ts <- ts(us_employment_1980$N_Employed_Millions, start = c(1980,01), end = c(2018, 03), frequency=12)

#'Test' data to be held back
us_employment_1980_for_testing <- us_employment_1980 %>%
  filter(Month >= yearmonth('2018 Apr'))
  
us_employment_1980_test_ts <- ts(us_employment_1980_for_testing$N_Employed_Millions, start = c(2018,04), end = c(2019, 09), frequency=12)


#'Fit' model on 'training' data, and forecast next 18 months:
drift_training_fc <- rwf(us_employment_1980_training_ts, h = 18, drift=TRUE)



#Create dataframe to store training data
drift_fc_df_training <- data.frame(Month = as.Date(time(us_employment_1980_training_ts)),
                          N_Employed_Millions = as.numeric(us_employment_1980_training_ts)) %>%
  mutate(type = "training data")


#And the actual values for the test data
drift_fc_df_test_actuals <- data.frame(Month = as.Date(time(us_employment_1980_test_ts)),
                                       N_Employed_Millions = as.numeric(us_employment_1980_test_ts)) %>%
  mutate(type = "test data")


#And now the forecasted values (beyond the training data), to be compared to the actual test data
forecast_drift_fc_df_to_compare <- data.frame(
  Month = as.Date(time(drift_training_fc$mean)),
  N_Employed_Millions = as.numeric(drift_training_fc$mean),
  lower_95 = drift_training_fc$lower[, "95%"],
  upper_95 = drift_training_fc$upper[, "95%"]) %>%
  mutate(type = "forecast")

#Combine
final_drift_fc_df_train_test <- drift_fc_df_training %>%
  bind_rows(drift_fc_df_test_actuals) %>% 
  bind_rows(forecast_drift_fc_df_to_compare) %>%
  as_tibble()


#Plot the training data, test data, and the predicted (forecasted) values from fitting on the training data, for evaluation against the test data
drift_forecast_train_test_plot <- ggplot(data = final_drift_fc_df_train_test, aes(x = Month, y = N_Employed_Millions)) +
  geom_line(aes(colour = type), linewidth = 1.0) +
  scale_colour_manual(values = c("training data" = 'black', "test data" = "#12346D", "forecast" = 'orange')) +
  geom_ribbon(data = forecast_drift_fc_df_to_compare, aes(x = Month, ymin = lower_95, ymax = upper_95), fill = 'orange', alpha = 0.3) +
  theme_minimal() +
  theme(text = element_text(size = 11)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("Drift")


#Compute RMSE
residual_df <- data.frame(Month = drift_fc_df_test_actuals$Month,
                          actuals = drift_fc_df_test_actuals$N_Employed_Millions,
                          forecast_preds = forecast_drift_fc_df_to_compare$N_Employed_Millions)

rmse_drift_fc_example <- residual_df %>%
  mutate(residuals2 = (actuals - forecast_preds)^2) %>%
  summarise(rmse = sqrt(mean(residuals2, na.rm=TRUE)))


#So avg. prediction error is 0.23 million people 
#I.e. suggests that on avg. our forecasted values on a given month for the no. of people employed will be out by 0.23m

#------------------------------


#2f) Prediction interval testing

#Take US Employment data and use Naive forecast to forecast the next value
naive_fc_1_step <- naive(us_employment_1980_ts, h = 1)

#One step forecast standard deviation, M = 1 (1 missing residual)
non_NA_residuals <- as.numeric(naive_fc_1_step$residuals)[!is.na(naive_fc_1_step$residuals)]
sigma_hat <- sqrt((1/(length(us_employment_1980_ts) - 1)) * sum(non_NA_residuals^2))

#This is basically just the same as doing sd(non_NA_residuals)

#Then, note how Lo_95 and Hi_95 = Forecast +/- 1.96*sigma_hat

#Then, for e.g. 3 steps ahead:
sigma_3 <- sigma_hat * sqrt(3)

#Should get Lo_95 and Hi_95 = Forecast +/- 1.96*sigma_3:
naive_fc_3_step <- naive(us_employment_1980_ts, h = 3)


#------------------------