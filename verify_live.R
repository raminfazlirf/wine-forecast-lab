# Opens the deployed app headlessly, visits every tab and counts output errors on the page.
app <- shinytest2::AppDriver$new("https://raminfazli.shinyapps.io/wine-forecast-lab/",
                                 width = 1500, height = 1400, load_timeout = 120000, timeout = 120000)
app$wait_for_idle(timeout = 120000)
errs <- c()
for (tab in c("1. Explore", "2. Forecasts", "3. Model specs", "4. Accuracy")) {
  app$set_inputs(tabs = tab); app$wait_for_idle(timeout = 120000); Sys.sleep(2)
  errs[tab] <- app$get_js("document.querySelectorAll('.shiny-output-error:not(.shiny-output-error-validation)').length")
}
app$set_inputs(tabs = "2. Forecasts"); app$wait_for_idle(timeout = 120000); Sys.sleep(2)
unlink("live_check.png"); app$get_screenshot("live_check.png", selector = "#fc_plot")
app$stop()
cat("\nLIVE APP ERRORS PER TAB:\n"); print(errs)
