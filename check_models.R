# Fits the default app settings for all six varietals and prints specs + accuracy.
source(file.path("R", "helpers.R"))
wines <- load_wines()
sp  <- split_data(wines, "1992-12-01")
t0  <- Sys.time()
fit <- fit_models(sp$train, c("TSLM", "ETS", "ARIMA"), FALSE, TRUE)
fc  <- forecast(fit, new_data = sp$valid)
acc <- accuracy_table(fit, fc, wines)
cat("\nFIT TIME (6 varietals):", round(difftime(Sys.time(), t0, units = "secs"), 1), "s\n")
cat("\n==== SPECS ====\n")
print(as.data.frame(spec_table(fit)[, c("Varietal", "Model", "Specification")]), right = FALSE)
cat("\n==== VALIDATION ====\n")
print(as.data.frame(acc |> dplyr::filter(Window == "Validation") |>
  dplyr::mutate(dplyr::across(c(RMSE, MAE, MAPE, MASE), ~ round(.x, 1)))))
cat("\n==== BEST ====\n"); print(as.data.frame(best_models(acc)))
b <- forecast_bounds(fc, 80); cat("\nNA interval rows:", sum(is.na(b$Lower)), "of", nrow(b), "\n")
p <- plot_forecasts(wines |> dplyr::filter(Varietal %in% c("Sparkling","Sweet white")),
                    fc |> dplyr::filter(Varietal %in% c("Sparkling","Sweet white")), 80, "side",
                    train_end = "1992-12-01", show_from = "1990-01-01")
ggplot2::ggsave("check_plot.png", p, width = 10, height = 5, dpi = 90)
cat("\nCHECK DONE\n")
