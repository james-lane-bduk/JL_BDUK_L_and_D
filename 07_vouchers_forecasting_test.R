library(bigrquery)
library(forecast)
library(dplyr)

#Define BQ Object Function
bq_object <- function(sql, page_size = "skip", billing = "bduk-analysis-workspace") {
  tb <- bigrquery::bq_project_query(billing, sql)
  if(page_size == "skip"){
    OBJ <- bigrquery::bq_table_download(tb)
  } else{
    OBJ <- bigrquery::bq_table_download(tb, page_size = page_size)
  }
}



#Load voucher time series containing number of connected vouchers, by connected month
#Along with total paid in vouchers.
#Cohort is from Project Gigabit launch (2021-04-01) onwards
connected_vouchers_by_month <- bq_object("SELECT t.voucher_connected_month, 
                                                 COUNT(DISTINCT t.voucher_name) AS N_VOUCHERS, 
                                                 SUM(voucher_value) AS total_vouchers_connected_value
                                          FROM 

                                         (SELECT DATE_TRUNC(payment_customer_approved_date, month) AS voucher_connected_month,
                                                 voucher_name,
                                                 BDUK_payable_amount AS voucher_value
                                          FROM bduk-staging.bduk_fibre_datalake_analytics.voucher
                                          WHERE payment_customer_approved_date >= '2021-04-01'
                                          AND payment_customer_approved_date < '2025-05-01') as t
                                         
                                          GROUP BY t.voucher_connected_month
                                          ORDER BY t.voucher_connected_month")

bduk_total_voucher_funding <- bq_object("SELECT SUM(max_project_value) AS total_voucher_budget
                                         FROM bduk-staging.bduk_fibre_datalake_analytics.prp
                                         WHERE package_title > '0'
                                         AND package_title NOT LIKE '%test%'") %>%
  pull()


#Before forecasting, for each month, we want to calculate the % of voucher funding remaining
connected_vouchers_by_month <- connected_vouchers_by_month %>%
  mutate(cumulative_voucher_connected_value = cumsum(total_vouchers_connected_value)) %>%
  mutate(funding_remaining_pct = 100.0*(bduk_total_voucher_funding - cumulative_voucher_connected_value)/bduk_total_voucher_funding)


#Prepare for applying forecasting model - turn data to time-series object
connected_vouchers_by_month_ts <- ts(connected_vouchers_by_month$N_VOUCHERS, start = c(2021, 4), frequency = 12)

#Crude visual of time series
plot(connected_vouchers_by_month_ts)


#Fit model, using pct of funding remaining as an external predictor
arima_model <- auto.arima(connected_vouchers_by_month_ts, xreg = connected_vouchers_by_month$funding_remaining_pct)


#Get info on fitted model
summary(arima_model)


#Look at the residuals
residuals <- residuals(arima_model)

# Extract fitted values
fitted_values <- fitted(arima_model)



#------------------

#Forecast next 12 months, assuming % of funding remaining declines at a consistent rate
funding_remaining_pct_pt_change <- diff(connected_vouchers_by_month$funding_remaining_pct) %>%
  mean()

#Next 12 values of 'estimated funding remaining'
estimated_funding_remaining_pct <- seq(last(connected_vouchers_by_month$funding_remaining_pct), by = funding_remaining_pct_pt_change, length.out=12)

#And use to forecast connected vouchers over the next 12 months
forecasted_values <- forecast(arima_model, h = 12, xreg = estimated_funding_remaining_pct)


#Plot forecast
plot(forecasted_values)


#Doesn't quite work - need a way to model drop-off effectively.
