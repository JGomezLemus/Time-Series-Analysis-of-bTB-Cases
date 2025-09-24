
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

## Forecast extendido para proyección (2025–2026) ---
fc_long <- fit_models %>%
  forecast(h = 36)   # 36 meses desde enero 2024 = 2024–2026

## Plot largo (2024–2026, solo proyección más datos reales) ---
forecast_plot_long <- autoplot(fc_long, train) +
  autolayer(cases_tsibble, colour = "black") +
  labs(title = "Forecast 2024–2026 vs Actual", y = "Cases")

ggsave("figures/forecast_plot_long.png", forecast_plot_long,
       width = 10, height = 6)

#interactive graph

## --- Forecast data ---
fc_long_clean <- fc_long %>%
  as_tibble() %>%
  rename(
    Model = .model,
    Cases = .mean,
    `Year-Month` = year_month
  )

## --- Observed data ---
obs_clean <- cases_tsibble %>%
  as_tibble() %>%
  rename(
    `Year-Month` = year_month,
    Cases = num_cases
  )

## --- Plot ---
forecast_plot_long <- ggplot(fc_long_clean, aes(x = `Year-Month`, y = Cases, color = Model)) +
  geom_line(size = 0.5) +  # grosor más fino para forecast
  geom_line(data = obs_clean, aes(x = `Year-Month`, y = Cases), color = "black", size = 0.5) +  # Observed en negro
  scale_y_continuous(limits = c(0, 10000)) +
  labs(
    title = "Forecast of Cases 2024–2026",
    y = "Number of Cases",
    x = "Year-Month"
  )

## --- Interactive ---
forecast_plot_long_interactive <- ggplotly(
  forecast_plot_long,
  tooltip = c("Model", "Cases", "Year-Month")
)

forecast_plot_long_interactive

# ROLLING ORIGIN CROSS-VALIDATION (ROBUST METRICS)----------------------------
# We use a rolling origin approach to compute out-of-sample accuracy at each step

##1.training------------------------------------------------------------------
cv <- train %>%
  stretch_tsibble(.init = 48, .step = 1)  # 48 months initial training window

cv_models <- cv %>%
  model(
    arima = ARIMA(num_cases),
    arima_log = ARIMA(log(num_cases)),
    ets = ETS(num_cases)
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
res_arima <- fit_models %>% select(arima) %>% augment()
res_arima_log <- fit_models %>% select(arima_log) %>% augment()

p_values <- tibble(
  Model = c("ets", "arima", "arima_log"),
  `p-value Ljung-Box` = c(
    Box.test(res_ets$.resid, lag = 20, type = "Ljung-Box")$p.value,
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
  arrange(RMSE) %>%  # opcional, ordena por RMSE
  kable(
    caption = "Model Comparison Metrics",
    digits = 2,
    col.names = c("Model", "AIC", "BIC","sigma2","RMSE", "MAPE", "MAE", "p-value Ljung-Box")
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))


##4.Detailed reports for best models-------------------------------------------
fit_models %>% select(arima_log) %>% report()





# SUBGROUP ANALYSIS-------------------------------------------------------------

#without reconciliatio

# 1. Filter data -------------------------------------------------------------
#Filter out NAs and Unknown herd types

cases_plot <- all_cases_collapsed %>%
  filter(!is.na(herd_type_ml_description),
         herd_type_ml_description != "Unknown",
         !is.na(best_estimate_gif_skin_lab_string)) %>%
  filter(year != 2025) %>%
  group_by(year_month, herd_type_ml_description, best_estimate_gif_skin_lab_string) %>%
  summarise(num_cases = n(), .groups = "drop") %>%
  mutate(
    year_month = as.Date(paste0(year_month, "-01")),
    series_id = paste(herd_type_ml_description, best_estimate_gif_skin_lab_string, sep = "_")
  ) %>%
  as.data.frame()  # make sure it's a regular data.frame for ggplot

# 2. Plot trends
ggplot(cases_plot, aes(x = year_month, y = num_cases, color = series_id)) +
  geom_line(size = 1) +
  geom_point(size = 1) +
  facet_grid(herd_type_ml_description ~ best_estimate_gif_skin_lab_string, scales = "free_y") +
  labs(
    title = "bTB Cases by Herd Type and Test Type",
    x = "Month",
    y = "Number of cases"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    legend.position = "none"  # remove legend
  )

# 1 Prepare the herd-level data

# We filter the herd types we want, excluding unknown an NA
# Also, we combine "Beef Store" and "Fattener" into a single group "Beef Store + Fattener".
#1. Preprocesamiento ---------------------------------------
cases_herd <- all_cases_collapsed %>%
  # quitamos NA
  filter(!is.na(herd_type_ml_description)) %>%
  # seleccionamos solo tipos de interés
  filter(herd_type_ml_description %in% c("Dairy", "Beef Store", "Fattener", "Mixed", "Trader")) %>%
  # quitamos 2025
  filter(year != 2025) %>%
  # fusionamos Beef Store + Fattener en uno solo
  mutate(
    herd_type_ml_description = case_when(
      herd_type_ml_description %in% c("Beef Store", "Fattener") ~ "Beef Store+Fattener",
      TRUE ~ herd_type_ml_description
    )
  ) %>%
  # número de casos por mes y tipo de herd
  group_by(year_month, herd_type_ml_description) %>%
  summarise(num_cases = n(), .groups = "drop") %>%
  # convertimos en fecha real (último día del mes)
  mutate(year_month = yearmonth(paste0(year_month, "-01")))

# 2. Convertir a tsibble y rellenar gaps --------------------
cases_herd_ts <- cases_herd %>%
  as_tsibble(index = year_month, key = herd_type_ml_description) %>%
  fill_gaps(num_cases = 0)  # completar meses sin casos

# 3. Train/test split --------------------------------------
train <- cases_herd_ts %>%
  filter(year_month <= yearmonth("2023 Dec"))

test <- cases_herd_ts %>% filter(year_month >= yearmonth("2024 Jan") & year_month <= yearmonth("2024 Dec"))

# 4. Crear jerarquía: total -> herd_type -------------------
cases_hierarchy <- cases_herd_ts %>%
  aggregate_key(
    herd_type_ml_description,
    num_cases = sum(num_cases)
  )

# 5. Ajustar modelo base (ARIMA con log) -------------------
fit_base <- cases_hierarchy %>%
  model(
    arima_log = ARIMA(log(num_cases + 0.1))
  )

# 6. Reconciliación  ------------------
fit_reconciled <- fit_base %>%
  reconcile(
    bu = bottom_up(arima_log),               # Bottom-up
    ols  = min_trace(arima_log, method = "ols"),   # OLS
    mint = min_trace(arima_log, method = "mint_shrink")  # MinT
  )

# 7. Forecast a 12 meses -----------------------------------
fc <- fit_reconciled %>%
  forecast(h = "12 months")

# 8. Datos recientes para graficar (últimos 5 años) --------
cases_hierarchy_recent <- cases_hierarchy %>%
  filter(year_month >= yearmonth("2020 Jan"))



```