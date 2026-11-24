# helpers.R
# Pure functions shared by app.R and the submission report (report/Week5_*.qmd).
# Keeping them here means every number in the report is produced by the same
# code the Shiny app runs.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(lubridate)
  library(tsibble)
  library(fable)
  library(fabletools)
  library(feasts)
})

MODEL_COLORS <- c(TSLM = "#E69F00", ETS = "#0072B2", ARIMA = "#009E73", Ensemble = "#CC79A7")

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------

# Parses month labels such as "1980 Jan", "Jan-80", "1980-01" or "1980-01-01".
parse_month <- function(x) {
  d <- suppressWarnings(lubridate::parse_date_time(
    x, orders = c("Y b", "b Y", "b-y", "b-Y", "Y-m", "Y-m-d", "m/d/Y", "m/d/y")
  ))
  yearmonth(d)
}

# Linear interpolation of interior/edge NAs (Rose has two "*" values in 1994).
interpolate_na <- function(y) {
  ok <- which(!is.na(y))
  if (length(ok) < 2 || length(ok) == length(y)) return(y)
  stats::approx(ok, y[ok], xout = seq_along(y), rule = 2)$y
}

# Reads the CSV with a project-relative path and returns a monthly tsibble
# keyed by Varietal, with columns Month, Varietal, Sales, Imputed.
load_wines <- function(path = file.path("data", "AustralianWines.csv")) {
  raw <- utils::read.csv(path, na.strings = c("", "NA", "*"), check.names = FALSE,
                         stringsAsFactors = FALSE)
  names(raw)[1] <- "Month"
  names(raw) <- trimws(gsub("[._]+", " ", names(raw)))   # "Sweet.white" -> "Sweet white"

  raw |>
    mutate(Month = parse_month(Month)) |>
    pivot_longer(-Month, names_to = "Varietal", values_to = "Sales") |>
    mutate(Sales = suppressWarnings(as.numeric(Sales))) |>
    arrange(Varietal, Month) |>
    group_by(Varietal) |>
    mutate(Imputed = is.na(Sales), Sales = interpolate_na(Sales)) |>
    ungroup() |>
    as_tsibble(index = Month, key = Varietal)
}

# ---------------------------------------------------------------------------
# Modelling
# ---------------------------------------------------------------------------

split_data <- function(data, train_end) {
  train_end <- yearmonth(train_end)
  list(train = filter(data, Month <= train_end),
       valid = filter(data, Month >  train_end))
}

# Fits the selected auto-specified models per varietal (one row per key).
# log_transform = TRUE fits on log(Sales); fable back-transforms forecasts.
# ensemble = TRUE adds an equal-weight combination of the fitted models.
fit_models <- function(train, models = c("TSLM", "ETS", "ARIMA"),
                       log_transform = FALSE, ensemble = TRUE) {
  stopifnot(length(models) >= 1)
  specs <- if (log_transform) {
    list(TSLM  = TSLM(log(Sales) ~ trend() + season()),
         ETS   = ETS(log(Sales)),
         ARIMA = ARIMA(log(Sales)))
  } else {
    list(TSLM  = TSLM(Sales ~ trend() + season()),
         ETS   = ETS(Sales),
         ARIMA = ARIMA(Sales))
  }
  fit <- model(train, !!!specs[models])

  if (ensemble && length(models) >= 2) {
    ens <- rlang::parse_expr(sprintf("(%s) / %d", paste(models, collapse = " + "),
                                     length(models)))
    fit <- mutate(fit, Ensemble = !!ens)
  }
  fit
}

# Training + validation accuracy in one long table.
accuracy_table <- function(fit, fc_valid, full_data) {
  # Training accuracy of a combination model needs its fitted values; if that
  # ever fails, report training metrics for the component models only.
  tr <- tryCatch(accuracy(fit),
                 error = function(e) accuracy(select(fit, -any_of("Ensemble"))))
  va <- accuracy(fc_valid, full_data)
  bind_rows(tr, va) |>
    mutate(Window = ifelse(.type == "Training", "Training", "Validation")) |>
    select(Varietal, Model = .model, Window, RMSE, MAE, MAPE, MASE) |>
    arrange(Varietal, Window, RMSE)
}

# Lowest validation RMSE per varietal.
best_models <- function(acc) {
  acc |>
    filter(Window == "Validation", is.finite(RMSE)) |>
    group_by(Varietal) |>
    slice_min(RMSE, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(Varietal, Best = Model, Best_RMSE = RMSE, Best_MAPE = MAPE)
}

# ---------------------------------------------------------------------------
# Human-readable model specifications
# ---------------------------------------------------------------------------

safe_model_sum <- function(m) {
  out <- tryCatch(fabletools::model_sum(m), error = function(e) NA_character_)
  if (is.null(out) || length(out) == 0) NA_character_ else as.character(out)[1]
}

explain_ets <- function(s) {
  parts <- regmatches(s, regexec("ETS\\(([^,]+),([^,]+),([^)]+)\\)", s))[[1]]
  if (length(parts) < 4) return("")
  word <- c(N = "none", A = "additive", Ad = "additive damped", M = "multiplicative")
  sprintf("Error: %s; Trend: %s; Season: %s",
          word[parts[2]], word[parts[3]], word[parts[4]])
}

explain_arima <- function(s) {
  m <- regmatches(s, regexec("\\((\\d+),(\\d+),(\\d+)\\)\\((\\d+),(\\d+),(\\d+)\\)\\[(\\d+)\\]", s))[[1]]
  if (length(m) < 8) {
    m2 <- regmatches(s, regexec("\\((\\d+),(\\d+),(\\d+)\\)", s))[[1]]
    if (length(m2) < 4) return("")
    return(sprintf("%s AR, %s difference(s), %s MA; no seasonal terms", m2[2], m2[3], m2[4]))
  }
  extra <- if (grepl("drift", s)) "; with drift" else if (grepl("mean", s)) "; with mean" else ""
  sprintf("Non-seasonal: %s AR, %s diff, %s MA. Seasonal (lag %s): %s AR, %s diff, %s MA%s",
          m[2], m[3], m[4], m[8], m[5], m[6], m[7], extra)
}

spec_table <- function(fit, log_transform = FALSE) {
  model_cols <- setdiff(names(fit), "Varietal")
  out <- lapply(model_cols, function(mc) {
    tibble(Varietal = fit$Varietal, Model = mc,
           Specification = vapply(fit[[mc]], safe_model_sum, character(1)))
  }) |> bind_rows()

  out |>
    mutate(
      Specification = case_when(
        Model == "TSLM"     ~ "TSLM: linear trend + 11 monthly dummies",
        Model == "Ensemble" ~ paste0("Equal-weight average of ",
                                     paste(setdiff(model_cols, "Ensemble"), collapse = ", ")),
        TRUE ~ Specification),
      `Plain-language reading` = case_when(
        Model == "ETS"   ~ vapply(Specification, explain_ets, character(1)),
        Model == "ARIMA" ~ vapply(Specification, explain_arima, character(1)),
        Model == "TSLM"  ~ "Straight-line trend plus a fixed effect for each month",
        TRUE             ~ "Averages point forecasts to reduce single-model risk"),
      Scale = if (log_transform) "log(Sales)" else "Sales"
    ) |>
    arrange(Varietal, Model)
}

# Estimated parameters (smoothing weights, ARIMA coefficients, TSLM terms).
param_table <- function(fit) {
  cols <- intersect(c("TSLM", "ETS", "ARIMA"), names(fit))
  if (!length(cols)) return(tibble())
  fit |>
    select(Varietal, all_of(cols)) |>
    tidy() |>
    transmute(Varietal, Model = .model, Term = term,
              Estimate = signif(estimate, 4))
}

# ---------------------------------------------------------------------------
# Forecast tables and plots
# ---------------------------------------------------------------------------

# Flattens a fable into mean + interval bounds for ggplot.
forecast_bounds <- function(fc, level = 80) {
  level  <- as.numeric(level)   # distributional::hilo() needs a double, not an integer
  pi_col <- paste0(level, "%")
  tb <- fc |> hilo(level = level) |> as_tibble()
  tibble(Varietal = tb$Varietal, Model = tb$.model, Month = tb$Month,
         Mean  = tb$.mean,
         Lower = vctrs::field(tb[[pi_col]], "lower"),
         Upper = vctrs::field(tb[[pi_col]], "upper"))
}

plot_overview <- function(data, train_end = NULL) {
  p <- ggplot(data, aes(x = as.Date(Month), y = Sales)) +
    geom_line(colour = "grey25", linewidth = 0.5) +
    facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
    labs(x = NULL, y = "Sales (thousands of litres)") +
    theme_minimal(base_size = 12)
  if (!is.null(train_end)) {
    te <- as.Date(yearmonth(train_end))
    p <- p +
      annotate("rect", xmin = te, xmax = as.Date(max(data$Month)),
               ymin = -Inf, ymax = Inf, alpha = 0.12, fill = "#0072B2") +
      geom_vline(xintercept = te, linetype = "dashed", colour = "#0072B2")
  }
  imp <- filter(data, Imputed)
  if (nrow(imp)) {
    p <- p + geom_point(data = imp, colour = "red", size = 2)
  }
  p
}

plot_seasonal <- function(data) {
  d <- data |>
    as_tibble() |>
    mutate(Year = factor(year(Month)), MonthName = month(Month, label = TRUE))
  ggplot(d, aes(MonthName, Sales, group = Year, colour = as.integer(as.character(Year)))) +
    geom_line(alpha = 0.8) +
    stat_summary(aes(group = 1), fun = mean, geom = "line", colour = "black",
                 linewidth = 1.1) +
    facet_wrap(~ Varietal, scales = "free_y", ncol = 1) +
    scale_colour_viridis_c(name = "Year") +
    labs(x = NULL, y = "Sales (thousands of litres)",
         subtitle = "Thin lines: one per year. Black line: average for that month.") +
    theme_minimal(base_size = 12)
}

plot_stl <- function(data) {
  data |>
    model(STL(Sales ~ season(window = "periodic"))) |>
    components() |>
    as_tibble() |>
    select(Varietal, Month, Sales, trend, season_year, remainder) |>
    pivot_longer(c(Sales, trend, season_year, remainder), names_to = "Component") |>
    mutate(Component = factor(Component, c("Sales", "trend", "season_year", "remainder"),
                              c("Data", "Trend", "Seasonal", "Remainder"))) |>
    ggplot(aes(as.Date(Month), value)) +
    geom_line(colour = "grey25") +
    facet_grid(Component ~ Varietal, scales = "free_y") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 11)
}

# layout = "side" : one panel per model (Varietal x Model grid)
# layout = "overlay": all models in one panel per varietal
plot_forecasts <- function(history, fc, level = 80, layout = c("side", "overlay"),
                           train_end = NULL, show_from = NULL) {
  layout <- match.arg(layout)
  b <- forecast_bounds(fc, level) |> mutate(Model = factor(Model, names(MODEL_COLORS)))
  h <- as_tibble(history)
  if (!is.null(show_from)) h <- filter(h, Month >= yearmonth(show_from))

  p <- ggplot() +
    geom_line(data = h, aes(as.Date(Month), Sales), colour = "grey30", linewidth = 0.45) +
    geom_ribbon(data = b, aes(as.Date(Month), ymin = Lower, ymax = Upper, fill = Model),
                alpha = if (layout == "side") 0.30 else 0.15) +
    geom_line(data = b, aes(as.Date(Month), Mean, colour = Model), linewidth = 0.8) +
    scale_colour_manual(values = MODEL_COLORS, name = "Model (line = mean)") +
    scale_fill_manual(values = MODEL_COLORS,
                      name = sprintf("%d%% prediction interval", level)) +
    labs(x = NULL, y = "Sales (thousands of litres)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.box = "vertical")

  if (!is.null(train_end)) {
    p <- p + geom_vline(xintercept = as.Date(yearmonth(train_end)),
                        linetype = "dashed", colour = "grey50")
  }
  if (layout == "side") {
    p + facet_grid(Varietal ~ Model, scales = "free_y")
  } else {
    p + facet_wrap(~ Varietal, scales = "free_y", ncol = 1)
  }
}
