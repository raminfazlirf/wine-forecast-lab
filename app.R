# Australian Wine Forecast Lab
# ADS 506, Module 5: compare TSLM, ETS, ARIMA (and an ensemble) per varietal.
# Run locally with shiny::runApp() from this folder. All paths are project-relative.

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
if (FALSE) { library(urca); library(markdown) }  # used indirectly (ARIMA unit-root tests, About tab); lets deployment detect them

source(file.path("R", "helpers.R"))

wines      <- load_wines(file.path("data", "AustralianWines.csv"))
varietals  <- sort(unique(wines$Varietal))
min_date   <- as.Date(min(wines$Month))
max_date   <- as.Date(max(wines$Month))
default_te <- as.Date("1992-12-01")   # 1993-1994 (24 months) held out by default

tip <- function(label, text) {
  tagList(label, tooltip(bsicons_or_q(), text, placement = "right"))
}
bsicons_or_q <- function() tags$span("ⓘ", style = "cursor:help; color:#0072B2;")

ui <- page_sidebar(
  title = "Australian Wine Forecast Lab: which model should forecast which wine?",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebar(
    width = 320,
    selectizeInput("varietals", tip("Varietals", "Pick one or more wine types. Each is modelled separately."),
                   choices = varietals, selected = c("Sparkling", "Sweet white"),
                   multiple = TRUE, options = list(plugins = list("remove_button"))),
    sliderInput("dates", tip("Date range", "Restrict the data used for plots and modelling."),
                min = min_date, max = max_date, value = c(min_date, max_date),
                timeFormat = "%b %Y"),
    sliderInput("train_end", tip("Training ends", "Data up to this month trains the models; the rest is the validation window used for scoring."),
                min = min_date, max = max_date, value = default_te,
                timeFormat = "%b %Y"),
    numericInput("h", tip("Forecast horizon (months)", "How far past the last data point to forecast after refitting on all selected data."),
                 value = 12, min = 1, max = 60),
    checkboxGroupInput("models", "Models (auto-specified)",
                       choices = c("TSLM", "ETS", "ARIMA"),
                       selected = c("TSLM", "ETS", "ARIMA"), inline = TRUE),
    checkboxInput("ensemble", tip("Add equal-weight Ensemble", "Averages the selected models' forecasts. Often more robust than any single model."), TRUE),
    checkboxInput("log", tip("Fit on log scale", "Stabilises variance when seasonal swings grow with the level. Forecasts are back-transformed."), FALSE),
    radioButtons("level", "Prediction interval", c("80%" = 80, "95%" = 95), inline = TRUE),
    actionButton("run", "Fit / update models", class = "btn-primary w-100"),
    helpText("Models refit when you click the button, so you can change several settings first.")
  ),

  navset_card_underline(
    id = "tabs",
    nav_panel("1. Explore",
      layout_columns(col_widths = c(12),
        card(card_header("Monthly sales (shaded = validation window, red dots = imputed values)"),
             plotOutput("overview", height = "auto")),
        navset_card_tab(
          nav_panel("Seasonal pattern", plotOutput("seasonal", height = "auto")),
          nav_panel("STL decomposition", plotOutput("stl", height = "520px"))
        )
      )),
    nav_panel("2. Forecasts",
      layout_columns(col_widths = c(12),
        card(
          card_header(
            div(class = "d-flex gap-4 align-items-center flex-wrap",
                radioButtons("view", NULL, inline = TRUE,
                             c("Validation check (fit on training only)" = "valid",
                               "Future forecast (refit on all selected data)" = "future")),
                radioButtons("layout", NULL, inline = TRUE,
                             c("Side-by-side" = "side", "Overlay" = "overlay")))),
          plotOutput("fc_plot", height = "auto")),
        card(card_header("Recommended model per varietal (lowest validation RMSE)"),
             tableOutput("best"))
      )),
    nav_panel("3. Model specs",
      card(card_header("Selected model forms"), DTOutput("specs")),
      card(card_header("Estimated parameters (optional detail)"), DTOutput("params"))),
    nav_panel("4. Accuracy",
      card(card_header("Training vs validation accuracy (lower is better; best validation RMSE per varietal in bold)"),
           DTOutput("acc"),
           div(class = "mt-2", downloadButton("dl_acc", "Download metrics (CSV)"),
               downloadButton("dl_fc", "Download forecasts (CSV)")))),
    nav_panel("About",
      card(includeMarkdown("ABOUT.md")))
  )
)

server <- function(input, output, session) {

  # Keep the training cut-off inside the chosen date range.
  observeEvent(input$dates, {
    lo <- input$dates[1] %m+% months(24)
    hi <- input$dates[2] %m-% months(1)
    val <- min(max(input$train_end, lo), hi)
    updateSliderInput(session, "train_end", min = lo, max = hi, value = val, timeFormat = "%b %Y")
  })

  filtered <- reactive({
    req(input$varietals)
    wines |>
      filter(Varietal %in% input$varietals,
             Month >= yearmonth(input$dates[1]),
             Month <= yearmonth(input$dates[2]))
  })

  n_panels <- reactive(length(input$varietals))

  # ---- Modelling (runs on button click; also once at start-up) ----
  settings <- eventReactive(input$run, {
    validate(
      need(length(input$varietals) >= 1, "Select at least one varietal."),
      need(length(input$models) >= 1, "Select at least one model."),
      need(is.numeric(input$h) && input$h >= 1, "Horizon must be at least 1 month.")
    )
    list(data = filtered(), train_end = input$train_end, models = input$models,
         ensemble = input$ensemble, log = input$log, h = input$h)
  }, ignoreNULL = FALSE)

  split <- reactive({
    s <- settings()
    sp <- split_data(s$data, s$train_end)
    validate(
      need(nrow(sp$valid) > 0, "The validation window is empty: move 'Training ends' earlier."),
      need(n_distinct(sp$train$Month) >= 24, "Need at least 24 training months for seasonal models.")
    )
    sp
  })

  fit_train <- reactive({
    s <- settings()
    withProgress(message = "Fitting models on the training window...", {
      fit_models(split()$train, s$models, s$log, s$ensemble)
    })
  })

  fc_valid <- reactive(forecast(fit_train(), new_data = split()$valid))

  fit_full <- reactive({
    s <- settings()
    withProgress(message = "Refitting on all selected data...", {
      fit_models(s$data, s$models, s$log, s$ensemble)
    })
  })
  fc_future <- reactive(forecast(fit_full(), h = settings()$h))

  acc <- reactive(accuracy_table(fit_train(), fc_valid(), settings()$data))

  # ---- Explore tab ----
  output$overview <- renderPlot(plot_overview(filtered(), input$train_end),
                                height = function() 170 * n_panels() + 40)
  output$seasonal <- renderPlot(plot_seasonal(filtered()),
                                height = function() 190 * n_panels() + 60)
  output$stl <- renderPlot(plot_stl(filtered()))

  # ---- Forecast tab ----
  output$fc_plot <- renderPlot({
    s <- settings()
    lvl <- as.numeric(input$level)
    if (input$view == "valid") {
      plot_forecasts(s$data, fc_valid(), lvl, input$layout,
                     train_end = s$train_end,
                     show_from = as.Date(s$train_end) %m-% months(36))
    } else {
      plot_forecasts(s$data, fc_future(), lvl, input$layout,
                     show_from = max(as.Date(s$data$Month)) %m-% months(48))
    }
  }, height = function() 260 * n_panels() + 90)

  output$best <- renderTable({
    best_models(acc()) |>
      mutate(Best_RMSE = round(Best_RMSE, 1), Best_MAPE = paste0(round(Best_MAPE, 1), "%")) |>
      rename(`Best model` = Best, `Validation RMSE` = Best_RMSE, `Validation MAPE` = Best_MAPE)
  })

  # ---- Specs tab ----
  output$specs <- renderDT({
    datatable(spec_table(fit_train(), settings()$log), rownames = FALSE,
              options = list(dom = "t", pageLength = 50))
  })
  output$params <- renderDT({
    datatable(param_table(fit_train()), rownames = FALSE, filter = "top",
              options = list(pageLength = 10))
  })

  # ---- Accuracy tab ----
  output$acc <- renderDT({
    a <- acc()
    b <- best_models(a)
    a <- a |>
      left_join(b |> select(Varietal, Best), by = "Varietal") |>
      mutate(IsBest = ifelse(Window == "Validation" & Model == Best, "yes", "no")) |>
      select(-Best) |>
      mutate(across(c(RMSE, MAE, MASE), ~ round(.x, 2)), MAPE = round(MAPE, 1))
    datatable(a, rownames = FALSE,
              options = list(dom = "t", pageLength = 100,
                             columnDefs = list(list(visible = FALSE, targets = ncol(a) - 1)))) |>
      formatStyle("Window", target = "row",
                  backgroundColor = styleEqual(c("Training", "Validation"), c("#f4f6f7", "#e8f1fa"))) |>
      formatStyle("IsBest", target = "row", fontWeight = styleEqual("yes", "bold"))
  })

  output$dl_acc <- downloadHandler(
    filename = function() "wine_accuracy.csv",
    content  = function(f) utils::write.csv(acc(), f, row.names = FALSE))
  output$dl_fc <- downloadHandler(
    filename = function() "wine_forecasts.csv",
    content  = function(f) {
      lvl <- as.numeric(input$level)
      out <- bind_rows(
        forecast_bounds(fc_valid(), lvl)  |> mutate(Type = "Validation"),
        forecast_bounds(fc_future(), lvl) |> mutate(Type = "Future")) |>
        mutate(Month = as.character(Month), PI = paste0(lvl, "%"))
      utils::write.csv(out, f, row.names = FALSE)
    })
}

shinyApp(ui, server)
