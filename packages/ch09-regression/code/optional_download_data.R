# Downloads the data used in Chapter 9 and saves dated snapshots in data/.
# The book reads the snapshots, so it still builds if a source changes or is offline.
# Run from the coursebook folder: Rscript data-raw/fetch_ch09.R
# regression_example_data.xlsx and gravity_data.xlsx are course files.

fred <- function(id) {
  d <- read.csv(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id),
                na.strings = c("", "."))
  names(d) <- c("date", id)
  d
}

gdp <- fred("GDPCA")                     # real GDP, annual
gdp$year <- as.integer(substr(gdp$date, 1, 4))

une <- fred("UNRATE")                    # unemployment rate, monthly
une$year <- as.integer(substr(une$date, 1, 4))
une <- aggregate(UNRATE ~ year, une, mean)

okun <- merge(gdp[, c("year", "GDPCA")], une, by = "year")
okun <- okun[okun$year <= 2025, ]
okun$growth <- c(NA, 100 * diff(log(okun$GDPCA)))
okun$d_unemp <- c(NA, diff(okun$UNRATE))
okun <- okun[!is.na(okun$growth), ]
write.csv(okun, "data/us_okun.csv", row.names = FALSE)
cat("Saved", nrow(okun), "years on", format(Sys.Date()), "\n")
