q <- Sys.which("quarto")
if (!nzchar(q)) q <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto"
cat("RENDER STATUS:", system2(q, c("render", file.path("report", "Week5_report.qmd"))), "\n")
