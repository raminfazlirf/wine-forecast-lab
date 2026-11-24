# Australian Wine Forecast Lab (ADS 506, Week 5)

**Live app:** https://raminfazli.shinyapps.io/wine-forecast-lab/

Shiny app that compares TSLM, ETS, ARIMA and an equal-weight ensemble for each varietal in the Australian wine sales data (monthly, 1980 to 1994).

## Run locally

```r
install.packages(c("shiny", "bslib", "DT", "markdown", "dplyr", "tidyr", "ggplot2",
                   "lubridate", "tsibble", "fable", "fabletools", "feasts", "urca"))
shiny::runApp()   # from this folder
```

## Deploy to shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<account>", token = "<token>", secret = "<secret>")
rsconnect::deployApp(appName = "wine-forecast-lab")
```

## Structure

| Path | Purpose |
|---|---|
| `app.R` | UI and server |
| `R/helpers.R` | Data loading, model fitting, tables and plots (shared with the report) |
| `data/AustralianWines.csv` | Dataset (project-relative path) |
| `ABOUT.md` | Text for the About tab |
| `report/Week5_report.qmd` | Two-page submission; recomputes every number with `R/helpers.R` |
| `report/screenshots/` | App screenshots used as Figures 1 and 2 |
| `make_report.R` | Captures app screenshots (shinytest2) and renders the report |
| `render_only.R` | Re-renders the report PDF with Quarto |
| `check_models.R` | Prints specs and accuracy for all six varietals |
| `verify_live.R` | Checks every output of the deployed app |

## Acknowledgement

Code drafted with help from Claude (Anthropic), an AI assistant, and reviewed and tested by the author.
