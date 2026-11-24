# 1) Capture app screenshots for the report, 2) render the PDF.
need <- c("shinytest2", "chromote", "tinytex")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss, type = "binary")

dir.create(file.path("report", "screenshots"), showWarnings = FALSE)
unlink(file.path("report", "screenshots", c("fig1.png", "fig2.png")))
app <- shinytest2::AppDriver$new(".", width = 1500, height = 2200,
                                 load_timeout = 90000, timeout = 90000)
app$wait_for_idle(timeout = 90000)
app$get_screenshot(file.path("report", "screenshots", "fig1.png"), selector = "#seasonal")
app$set_inputs(tabs = "2. Forecasts")
app$wait_for_idle(timeout = 90000)
Sys.sleep(3)
app$get_screenshot(file.path("report", "screenshots", "fig2.png"), selector = "#fc_plot")
app$stop()
cat("SCREENSHOTS DONE\n")

if (!tinytex::is_tinytex()) tinytex::install_tinytex(force = TRUE)
q <- Sys.which("quarto")
if (!nzchar(q)) q <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto"
status <- system2(q, c("render", file.path("report", "Week5_report.qmd")))
cat("RENDER STATUS:", status, "\n")
