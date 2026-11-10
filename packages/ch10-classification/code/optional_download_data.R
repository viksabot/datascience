# Downloads the data used in Chapter 10 and saves dated snapshots in data/.
# The book reads the snapshots, so it still builds if a source changes or is offline.
# Run from the coursebook folder: Rscript data-raw/fetch_ch10.R
# regression_example_data.xlsx and trade_cars_2022.xlsx are course files.

fred <- function(id) {
  d <- read.csv(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id),
                na.strings = c("", "."))
  names(d) <- c("date", id)
  d
}

y <- Reduce(function(a, b) merge(a, b, by = "date"),
            list(fred("GS10"), fred("TB3MS"), fred("USREC")))
y$spread <- y$GS10 - y$TB3MS
n <- nrow(y)
# recession 12 months from now
y$rec_12 <- c(y$USREC[13:n], rep(NA, 12))
write.csv(y[, c("date", "GS10", "TB3MS", "spread", "USREC", "rec_12")],
          "data/us_yield_recession.csv", row.names = FALSE)
cat("Saved", n, "months on", format(Sys.Date()), "\n")
