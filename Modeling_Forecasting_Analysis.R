
# TIME SERIES MODELING AND FORECASTING ---------------------------------------

##1.Convert to tsibble ---------------------------------------------------------
# 'yearmonth()' ensures a proper monthly index for time series analysis

cases_tsibble <- cases_per_month %>%
  mutate(year_month = yearmonth(year_month)) %>%  # yearmonth object
  as_tsibble(index = year_month)


##2.Split into training (up to Dec 2023) and testing (Jan 2024 onward)----------
# I split this way to evaluate model performance on unseen future data,simulating
# a real-world forecasting scenario

train <- cases_tsibble %>%
  filter(year_month <= yearmonth("2023 Dec"))

test <- cases_tsibble %>%
  filter(year_month >= yearmonth("2024 Jan"))


##3.Fit multiple forecasting models on training data---------------------------
# Includes ARIMA (with and without log), ETS, Seasonal Naive, and Linear Trend + Seasonality

fit_models <- train %>%
  model(
    arima = ARIMA(num_cases),
    arima_log = ARIMA(log(num_cases)),
    ets = ETS(num_cases),
    ets_log = ETS(log(num_cases)),
    snaive = SNAIVE(num_cases),
    tslm = TSLM(num_cases ~ trend() + season())
  )

report(fit_models) # Summarize fitted models
saveRDS(fit_models, "results/fit_models.rds") # Guardar modelos

##4.Forecasting on test period-------------------------------------------------
# Forecast 12 months ahead to match the test period (Jan–Dec 2024)
fc_test <- fit_models %>%
  forecast(h = 12)

saveRDS(fc_test, "results/fc_test.rds")

##5.Visual comparison of forecast vs actual------------------------------------
forecast_plot <- autoplot(fc_test, train) +
  autolayer(cases_tsibble, colour = "black") +
  labs(title = "Forecast vs Actual", y = "Cases")

ggsave("figures/forecast_plot.png", forecast_plot, width = 10, height = 6)

# MODEL ACCURACY ON TEST SET --------------------------------------------------
##1.Evaluate model performance using RMSE and MAPE------------------------------
test_accuracy <- accuracy(fc_test, test)
test_accuracy_summary <- test_accuracy %>%
  group_by(.model) %>%
  summarise(RMSE = mean(RMSE, na.rm = TRUE), MAPE = mean(MAPE, na.rm = TRUE))
test_accuracy_summary

# ROLLING ORIGIN CROSS-VALIDATION (ROBUST METRICS)----------------------------
# We use a rolling origin approach to compute out-of-sample accuracy at each step

##1.training------------------------------------------------------------------
cv <- train %>%
  stretch_tsibble(.init = 48, .step = 1)  # 48 months initial training window

cv_models <- cv %>%
  model(
    arima = ARIMA(num_cases),
    arima_log = ARIMA(log(num_cases)),
    ets = ETS(num_cases),
    ets_log = ETS(log(num_cases))
  )

##2.1-step ahead forecasts for cross-validation-------------------------------
fc_cv <- cv_models %>% forecast(h = 1) 

# Filter actual values to match forecasted periods
# This ensures correct calculation of accuracy metrics (RMSE, MAPE, MAE)
actuals_cv <- cases_tsibble %>% filter(year_month %in% fc_cv$year_month)

##3.Accuracy-------------------------------------------------------------------
cv_accuracy <- accuracy(fc_cv, actuals_cv, measures = list(RMSE = RMSE, MAPE = MAPE, MAE = MAE))

# 5. Summarize
cv_accuracy_summary <- cv_accuracy %>%
  group_by(.model) %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    MAPE = mean(MAPE, na.rm = TRUE),
    MAE  = mean(MAE, na.rm = TRUE)
  )
cv_accuracy_summary

write_csv(cv_accuracy_summary, "results/cv_accuracy_summary.csv")

# MODEL COMPARISON-------------------------------------------------------------
# Combine information criteria (AIC, BIC, sigma²) with rolling CV metrics

##1. AIB,BIC,sigma2------------------------------------------------------------
metrics <- fit_models %>%
  glance() %>%
  select(.model, AIC, BIC, sigma2)

final_summary <- left_join(metrics, cv_accuracy_summary, by = ".model")

write_csv(final_summary, "results/final_table.csv")

##2.Residual autocorrelation (Ljung-Box test)----------------------------------
res_ets   <- fit_models %>% select(ets)   %>% augment()
res_ets_log  <- fit_models %>% select(ets_log)   %>% augment()
res_arima <- fit_models %>% select(arima) %>% augment()
res_arima_log <- fit_models %>% select(arima_log) %>% augment()

p_values <- tibble(
  Model = c("ets","ets_log","arima", "arima_log"),
  `p-value Ljung-Box` = c(
    Box.test(res_ets$.resid, lag = 20, type = "Ljung-Box")$p.value,
    Box.test(res_ets_log$.resid, lag = 20, type = "Ljung-Box")$p.value,
    Box.test(res_arima$.resid, lag = 20, type = "Ljung-Box")$p.value,
    Box.test(res_arima_log$.resid, lag = 20, type = "Ljung-Box")$p.value
  )
)

##3.Final comparison table ----------------------------------------------------
##Merge everything into a final comparison table
final_table <- head(left_join(final_summary, p_values, by = c(".model" = "Model")),-2)
final_table



# Display table (sorted by RMSE)
final_table %>%
 
  kable(
    caption = "Model Comparison Metrics",
    digits = 2,
    col.names = c("Model", "AIC", "BIC","sigma2","RMSE", "MAPE", "MAE", "p-value Ljung-Box")
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))


##4.Detailed reports for best models-------------------------------------------
fit_models %>% select(arima_log) %>% report()