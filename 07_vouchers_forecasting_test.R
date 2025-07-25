#Load fpp3 package, which attaches dplyr, tidyr, lubridate etc, and also load zoo for moving averages, and forecast
library(fpp3)
library(zoo)
library(forecast)

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

ggplot(data = final_fc_df, aes(x = Month, y = N_Employed_Millions)) +
  geom_line(aes(colour = type), linewidth = 1.0) +
  scale_colour_manual(values = c("data" = 'black', "forecast" = "#12346D")) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), fill = '#12346D', alpha = 0.3) +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  xlab("Month") +
  ylab("No. Employed (Millions) ") +
  ggtitle("Mean")







#Define BQ Object Function
bq_object <- function(sql, page_size = "skip", billing = "bduk-analysis-workspace") {
  tb <- bigrquery::bq_project_query(billing, sql)
  if(page_size == "skip"){
    OBJ <- bigrquery::bq_table_download(tb)
  } else{
    OBJ <- bigrquery::bq_table_download(tb, page_size = page_size)
  }
}

#DEVELOP THIS AT NEXT L&D DAY


#---Step 1) Initial vouchers Data Extraction and Transformation

#Load voucher time series containing paid voucher totals, by connected month
#Cohort is from Jan 2024 onwards
vouchers_paid_by_month <- bq_object("SELECT t.voucher_connected_month, 
                                                 SUM(voucher_paid_value) AS vouchers_paid_value
                                          FROM 

                                         (SELECT DATE_TRUNC(payment_customer_approved_date, month) AS voucher_connected_month,
                                                 voucher_name,
                                                 BDUK_payable_amount AS voucher_paid_value
                                          FROM bduk-staging.bduk_fibre_datalake_analytics.voucher
                                          WHERE payment_customer_approved_date >= '2024-01-01'
                                          AND payment_customer_approved_date < '2025-07-01') as t
                                         
                                          GROUP BY t.voucher_connected_month
                                          ORDER BY t.voucher_connected_month")


#We also need the amount of voucher funding available across all voucher projects, by month, but we want to know what it was like 'at the time'
#To do this, we can iteratively filter for project dates
#This is still a simplification, because we're including all projects regardless of their present day status
#E.g. a project might have been live in Jan 2024, but then cancelled in Mar 2024. So realistically, we wouldn't want to include it...
#...in our 'total voucher funding' available from Mar 2024 onwards, but we will here for simplicity - requires previous snapshots otherwise

#And we want to start 5 months earlier, as e.g. projects published in Jan will typically pay out 5 months later
start_date <- vouchers_paid_by_month$voucher_connected_month[1] - months(5)
end_date <- tail(vouchers_paid_by_month$voucher_connected_month, 1) - months(5)
date_seq <- seq(from = start_date + months(1), to = end_date + months(1), by = "1 month")



project_funding_available_to_date <- c()

#Loop over each value in date_seq, calculating the total voucher value available across projects published prior to that date
for(month in 1:length(date_seq)) {
  
  max_project_value_in_vouchers <- bq_object(glue("SELECT SUM(max_project_value) AS total_voucher_budget
                                                FROM bduk-staging.bduk_fibre_datalake_analytics.prp
                                                WHERE DATE(package_date) < '{date_seq[month]}'")) %>%
    pull()
  
  project_funding_available_to_date[month] <- max_project_value_in_vouchers
  
  
}


#Now, we need to match this with the vouchers data - we've already taken care of the 5 month lag


#Before forecasting, for each month, we want to calculate the % of voucher funding remaining
vouchers_paid_by_month <- vouchers_paid_by_month %>%
  mutate(project_funding_available_to_date = project_funding_available_to_date) %>%
  mutate(cumulative_voucher_paid_value = cumsum(vouchers_paid_value)) %>%
  mutate(funding_remaining_pct = 100.0*(project_funding_available_to_date - cumulative_voucher_paid_value)/project_funding_available_to_date)


#---End of Step 1)




#---Step 2) Data Exploration

#Apply differencing of 1 month to time series
vouchers_paid_by_month_with_lag <- vouchers_paid_by_month %>%
  mutate(vouchers_paid_value_prev_month = lag(vouchers_paid_value, 1),
         diff = vouchers_paid_value - vouchers_paid_value_prev_month)


#Then plot differences to see if mean is constant (i.e. stationary series)
ggplot(vouchers_paid_by_month_with_lag, aes(x = voucher_connected_month, y = diff)) +
  geom_line(linewidth = 1.5, colour = '#12346D') +
  geom_hline(yintercept = mean(vouchers_paid_by_month_with_lag$diff, na.rm=TRUE), linewidth = 1.0, linetype = "dashed") +
  theme_minimal() +
  theme(text = element_text(size = 14)) +
  xlab("Voucher Connected Month") +
  ylab("Difference in Vouchers Paid (£)")


#Check autocorrelation to ensure independence of differenced values (should be minimal AC)
acf(vouchers_paid_by_month_with_lag$diff)


#---End of Step 2)





#---Step 3) Prepare data for vouchers forecast

#Prepare for applying forecasting model - turn data to time-series object
vouchers_paid_by_month_ts <- ts(vouchers_paid_by_month$vouchers_paid_value, start = c(2024, 1), frequency = 12)

#Crude visual of time series
plot(vouchers_paid_by_month_ts)


#Fit model, using pct of funding remaining as an external predictor
arima_model <- auto.arima(d = 1, vouchers_paid_by_month_ts, xreg = vouchers_paid_by_month$funding_remaining_pct)


#Get info on fitted model
summary(arima_model)


#Look at the residuals
residuals <- residuals(arima_model)

# Extract fitted values
fitted_values <- fitted(arima_model)



#------------------

#SHOULD DO A MONTH AT A TIME?

#Forecast next 12 months, assuming % of funding remaining declines at a consistent rate
funding_remaining_pct_pt_change <- diff(vouchers_paid_by_month$funding_remaining_pct) %>%
  mean()

#Next 12 values of 'estimated funding remaining'
estimated_funding_remaining_pct <- seq(last(vouchers_paid_by_month$funding_remaining_pct), by = funding_remaining_pct_pt_change, length.out=12)

#And use to forecast connected vouchers over the next 12 months
forecasted_values <- forecast(arima_model, h = 12, xreg = estimated_funding_remaining_pct)


#Plot forecast
plot(forecasted_values)


#Doesn't quite work - need a way to model drop-off effectively.

