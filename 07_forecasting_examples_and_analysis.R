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

#Prediction intervals based on standard deviation of residuals, i.e.
sd_res <- sd(us_employment_1980_ts - mean(us_employment_1980_ts))

#For 95% interval
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


#----------------------------------------------------


#2e) Evaluating fitted model and residuals - e.g. look at drift model
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

#No - there is some correlation. Implies this model potentially not a great fit (hasn't captured all temporal dependencies)


#Are residuals normally distributed?
ggplot(data = drift_fc_df_add_fitted_and_resid, aes(x = residuals)) +
  geom_histogram() +
  theme_minimal() +
  theme(text = element_text(size = 11)) +
  xlab("Residual Value") +
  ylab("No. of Observations")

#Generally yes, but there are some outliers


#Compute RMSE - this is equivalent to looking at prediction error of the training data, as these data points were involved in 'fitting' the model
drift_fc_df_add_fitted_and_resid %>%
  mutate(residuals2 = residuals^2) %>%
  summarise(rmse = sqrt(mean(residuals2, na.rm=TRUE)))


#Approx 0.22 million - so on avg, forecasted value approx 0.22 million employees away from actual


#Do the residuals look like white noise?
Box.test(drift_fc_df_add_fitted_and_resid$residuals, lag = 24, type = "Box-Pierce")



#Very small p-value - almost 0% chance we'd see autocorrelation like this if residuals actually resembled white noise
#Peaks at lags 12 and 24 - indicative of yearly seasonality




#-----------------



#2e) Evaluating model performance (accuracy) - this time, hold 18 most recent months back

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

#This is basically just the same as doing sd(non_NA_residuals), since we assume mean(residuals) = 0 -

#Then, note how Lo_95 and Hi_95 = Forecast +/- 1.96*sigma_hat

#Then, for e.g. 3 steps ahead:
sigma_3 <- sigma_hat * sqrt(3)

#Should get Lo_95 and Hi_95 = Forecast +/- 1.96*sigma_3:
naive_fc_3_step <- naive(us_employment_1980_ts, h = 3)

pct_95_3_step <-  tail(as.vector(naive_fc_3_step$mean),1) + (1.96 * sigma_3)


#------------------------


#2g) Quantile Scores - consider train/test split performed prior:

#'Training' data
us_employment_1980_training_ts <- ts(us_employment_1980$N_Employed_Millions, start = c(1980,01), end = c(2018, 03), frequency=12)

#'Test' data to be held back
us_employment_1980_for_testing <- us_employment_1980 %>%
  filter(Month >= yearmonth('2018 Apr'))

us_employment_1980_test_ts <- ts(us_employment_1980_for_testing$N_Employed_Millions, start = c(2018,04), end = c(2019, 09), frequency=12)


#'Fit' model on 'training' data, and forecast next 18 months:
drift_training_fc <- rwf(us_employment_1980_training_ts, h = 18, drift=TRUE)



#Now, for each month, we want to see if the actual value falls below the 10th percentile (i.e. below Lo 80), and calculate Q_(0.1, t)
quantile_score_df <- data.frame(Month = as.Date(time(us_employment_1980_test_ts)), 
                                pct_10 = as.vector(drift_training_fc$lower[, "80%"]),
                                actuals = as.numeric(us_employment_1980_test_ts)) %>%
  mutate(Qpt = case_when(
    actuals < pct_10 ~ 2*(1-actuals)*(pct_10 - actuals),
    actuals >= pct_10 ~ 2*0.1*(actuals - pct_10),
    TRUE ~ NA))



#Should be able to compare via:
accuracy(drift_training_fc, us_employment_1980_test_ts, measures = list(qs=quantile_score), probs=0.10)
                                          



#------------------------


#2h) Time series cross validation. E.g. make 2 sets of training data

#'Training' data - starting in Jan 1980 and ending in Mar 2018
us_employment_1980_training_ts_1 <- ts(us_employment_1980$N_Employed_Millions, start = c(1980,01), end = c(2018, 03), frequency=12)

#'Test' data to be held back - just Apr 2018
us_employment_1980_for_testing_1 <- us_employment_1980 %>%
  filter(Month == yearmonth('2018 Apr'))

us_employment_1980_test_ts_1 <- ts(us_employment_1980_for_testing$N_Employed_Millions, start = c(2018,04), end = c(2018, 04), frequency=12)

drift_training_fc_1 <- rwf(us_employment_1980_training_ts_1, h = 1, drift=TRUE)

diff_1 <- as.numeric(us_employment_1980_test_ts_1) - as.numeric(drift_training_fc_1$mean)

rmse_1_on_test_set <- sqrt(mean(diff_1^2))


#'Training' data - starting in Jan 1980 and ending in Apr 2018
us_employment_1980_training_ts_2 <- ts(us_employment_1980$N_Employed_Millions, start = c(1980,01), end = c(2018, 04), frequency=12)

#'Test' data to be held back - just May 2018
us_employment_1980_for_testing_2 <- us_employment_1980 %>%
  filter(Month == yearmonth('2018 May'))

us_employment_1980_test_ts_2 <- ts(us_employment_1980_for_testing$N_Employed_Millions, start = c(2018,05), end = c(2018, 05), frequency=12)

drift_training_fc_2 <- rwf(us_employment_1980_training_ts_2, h = 1, drift=TRUE)

diff_2 <- as.numeric(us_employment_1980_test_ts_2) - as.numeric(drift_training_fc_2$mean)

rmse_2_on_test_set <- sqrt(mean(diff_2^2))


#Now compute average (mean) RMSE:
rmse_avg <- (rmse_1_on_test_set + rmse_2_on_test_set)/2.


#Could then repeat this for future months, and indeed across different models, to see which model is best overall

#------------------------------------

#3) Time Series Regression Modelling

#3a) Fit a simple linear regression to forecast N_Employed where the predictor variable is 'time'
#-This is basically just a linear model for the trend of the time series, forecasted into the future
us_employment_1980_modified <- us_employment_1980 %>%
  as_tibble() %>%
  mutate(Month = as.Date(Month)) %>%
  select(Month, N_Employed_Millions) %>%
  mutate(type = "data")

#Note - this technically turns it to 'day-level' in the context of lm - see difference between <yearmon> and <date>

slr_employed <- lm(N_Employed_Millions ~ Month, data = us_employment_1980_modified)


#Now generate an additional 18 months that we want to use the model to 'forecast'
future_months_df <- data.frame(Month = seq(from = tail(us_employment_1980_modified$Month, 1) + months(1), to = tail(us_employment_1980_modified$Month, 1) + months(19), by = "1 month"))

slr_employed_preds <- predict(slr_employed, newdata = future_months_df, se.fit = TRUE)
#Note - this is similar to predict with interval = "confidence" and level = 0.67, but SE also needs multiplying by t-distribution critical value

us_employment_lm_preds <- future_months_df %>%
  mutate(N_Employed_Millions = slr_employed_preds$fit,
         type = "lm",
         N_Employed_Millions_95lower = slr_employed_preds$fit - 1.96*slr_employed_preds$se.fit,
         N_Employed_Millions_95upper = slr_employed_preds$fit + 1.96*slr_employed_preds$se.fit)

#Combine into a single df
us_employment_1980_modified_add_lm_preds <- us_employment_1980_modified %>%
  bind_rows(us_employment_lm_preds)


#Plot
lm_forecast_just_time <- ggplot(data = us_employment_1980_modified_add_lm_preds, aes(x = Month, y = N_Employed_Millions, colour = type)) +
  geom_line(linewidth = 1.0) +
  geom_ribbon(aes(ymin = N_Employed_Millions_95lower, ymax = N_Employed_Millions_95upper), fill = '#12346D', alpha = 0.3) +
  scale_colour_manual(values = c("data" = "black", "lm" = "#12346D")) +
  theme_minimal() +
  theme(text = element_text(size = 11)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("Linear Model of Trend")


#------------------------------------

#4) Exponential Smoothing

#a) Simple exponential smoothing - some weighted 'mix' of Naive & Mean, where weights determined based on minimising SSE
ses_fc <- ses(us_employment_1980_ts, h=12)

#Turn Time series objects back into dataframes to plot
ses_fc_df <- data.frame(Month = as.Date(time(us_employment_1980_ts)),
                           N_Employed_Millions = as.numeric(us_employment_1980_ts)) %>%
  mutate(type = "data")

forecast_ses_fc_df <- data.frame(
  Month = as.Date(time(ses_fc$mean)),
  N_Employed_Millions = as.numeric(ses_fc$mean),
  lower_95 = ses_fc$lower[, "95%"],
  upper_95 = ses_fc$upper[, "95%"]) %>%
  mutate(type = "forecast")

#Values very similar to Naive - not suprising as summary(ses_fc) shows alpha (weight) parameter ~ 1


final_ses_fc_df <- ses_fc_df %>%
  bind_rows(forecast_ses_fc_df) %>%
  as_tibble()

ses_forecast_plot <- ggplot(data = final_ses_fc_df, aes(x = Month, y = N_Employed_Millions)) +
  geom_line(aes(colour = type), linewidth = 1.0) +
  scale_colour_manual(values = c("data" = 'black', "forecast" = "#12346D")) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = '#12346D', alpha = 0.3) +
  theme_minimal() +
  theme(text = element_text(size = 11)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("Simple Exponential Smoothing")
