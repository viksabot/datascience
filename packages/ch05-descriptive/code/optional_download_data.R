# Downloads the data used in Chapter 5 and saves dated snapshots in data/.
# The book reads the snapshots, so it still builds if a source changes or is offline.
# Run from the coursebook folder: Rscript data-raw/fetch_ch05.R

base <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT"

get_eurostat <- function(dataset, filters) {
  query <- paste0("c[", names(filters), "]=", filters, collapse = "&")
  url <- paste0(base, "/", dataset, "/1.0?format=csvdata&compress=false&", query)
  read.csv(url, stringsAsFactors = FALSE)
}

aggregates <- c("EA", "EA12", "EA18", "EA19", "EA20", "EA21", "EU", "EU15",
                "EU27_2007", "EU27_2020", "EU28")

# 1) Mean and median equivalised net income, by country, 2023 (PPS)
income <- get_eurostat("ilc_di03", list(
  TIME_PERIOD = 2023, age = "TOTAL", sex = "T", unit = "PPS",
  statinfo = "MEAN_EI,MED_EI"
))
income <- income[!income$geo %in% aggregates, c("geo", "statinfo", "OBS_VALUE")]
income <- reshape(income, idvar = "geo", timevar = "statinfo", direction = "wide")
names(income) <- c("geo", "mean_income", "median_income")
write.csv(income, "data/income_europe_2023.csv", row.names = FALSE)

# 2) Real GDP growth, by country, 2015 (% change on previous year)
growth <- get_eurostat("nama_10_gdp", list(
  TIME_PERIOD = 2015, na_item = "B1GQ", unit = "CLV_PCH_PRE"
))
growth <- growth[!growth$geo %in% aggregates, c("geo", "OBS_VALUE")]
names(growth) <- c("geo", "gdp_growth")
write.csv(growth, "data/gdp_growth_europe_2015.csv", row.names = FALSE)

# 3) NASDAQ Composite daily close, from FRED
nasdaq <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=NASDAQCOM",
                   na.strings = c("", "."))
names(nasdaq) <- c("date", "close")
nasdaq <- nasdaq[!is.na(nasdaq$close), ]
write.csv(nasdaq, "data/nasdaq_daily.csv", row.names = FALSE)

cat("Saved", nrow(income), "income rows,", nrow(growth), "growth rows,",
    nrow(nasdaq), "NASDAQ rows on", format(Sys.Date()), "\n")
