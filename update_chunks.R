# Script to update all remaining ```r chunks in col-chart.Rmd
content <- readLines("vignettes/col-chart.Rmd")

# Find all ```r chunks that don't already have options
chunk_count <- 0
for (i in 1:length(content)) {
  if (content[i] == "```r") {
    chunk_count <- chunk_count + 1
    # Create a unique chunk name
    chunk_name <- paste0("chunk-", chunk_count)
    content[i] <- paste0("```{r ", chunk_name, ", fig.width=8, fig.height=6, eval=TRUE, echo=TRUE}")
  }
}

writeLines(content, "vignettes/col-chart.Rmd")
cat("Updated", chunk_count, "chunks in col-chart.Rmd\n")
