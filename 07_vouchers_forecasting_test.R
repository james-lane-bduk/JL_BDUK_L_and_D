library(bigrquery)
library(forecast)
library(dplyr)
library(ggplot2)
library(lubridate)
library(glue)

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


#So that we can paste these into BQ, we need to modify them slightly


project_funding_available_to_date <- c()

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
