library(bigrquery)
library(forecast)

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
connected_vouchers_by_month <- bq_object("SELECT t.voucher_connected_month, COUNT(DISTINCT t.voucher_name) AS N_VOUCHERS
                                          FROM 

                                         (SELECT DATE_TRUNC(payment_customer_approved_date, month) AS voucher_connected_month,
                                                 voucher_name
                                          FROM bduk-staging.bduk_fibre_datalake_analytics.voucher
                                          WHERE payment_customer_approved_date >= '2022-01-01'
                                          AND payment_customer_approved_date < '2025-05-01') as t
                                         
                                          GROUP BY t.voucher_connected_month
                                          ORDER BY t.voucher_connected_month")


#Prepare for applying forecasting model - turn data to time-series object
connected_vouchers_by_month_ts <- ts(connected_vouchers_by_month$N_VOUCHERS, start = c(2022, 1), frequency = 12)

#Crude visual of time series
plot(connected_vouchers_by_month_ts)

#Fit model
arima_model <- auto.arima(connected_vouchers_by_month_ts)

#Get info on fitted model
summary(arima_model)


#Look at the residuals
residuals <- residuals(arima_model)

#Forecast next 12 months
forecasted_values <- forecast(arima_model, h = 12)


# Extract fitted values
fitted_values <- fitted(arima_model)
