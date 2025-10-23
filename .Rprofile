source("renv/activate.R")

# Work around Windows + Quarto env passing issue during R CMD check/rendering.
# Force rmarkdown to use pandoc instead of the Quarto CLI on Windows.
try({
  if (tolower(Sys.info()[["sysname"]]) == "windows") {
    Sys.setenv(R_MARKDOWN_USE_QUARTO = "false")
  }
}, silent = TRUE)
