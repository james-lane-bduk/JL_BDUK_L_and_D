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
mean_fc <- meanf(us_employment_1980_ts)

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
  

final_fc_df <- mean_fc_df %>%
  bind_rows(forecast_mean_fc_df) %>%
  as_tibble()

mean_forecast_plot <- ggplot(data = final_fc_df, aes(x = Month, y = N_Employed_Millions)) +
  geom_line(aes(colour = type), linewidth = 1.0) +
  scale_colour_manual(values = c("data" = 'black', "forecast" = "#12346D")) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = '#12346D', alpha = 0.3) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("Mean")




#2b) Naive Forecast - future values = mean of past observations

