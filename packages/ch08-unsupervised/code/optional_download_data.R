# Downloads the data used in Chapter 8 and saves dated snapshots in data/.
# The book reads the snapshots, so it still builds if a source changes or is offline.
# Run from the coursebook folder: Rscript data-raw/fetch_ch08.R
# The World Bank table comes from fetch_ch06.R (data/wdi_hdi_clean_2022.csv).

fred <- function(id) {
  d <- read.csv(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id),
                na.strings = c("", "."))
  names(d) <- c("date", id)
  d
}

# 1) US Treasury constant maturity yields, monthly
ids <- c("GS1", "GS2", "GS3", "GS5", "GS7", "GS10")
yields <- Reduce(function(a, b) merge(a, b, by = "date"), lapply(ids, fred))
yields <- yields[complete.cases(yields), ]
write.csv(yields, "data/us_treasury_yields.csv", row.names = FALSE)

# 2) Chicago Fed National Activity Index and US recession dates
cfnai <- merge(fred("CFNAI"), fred("CFNAIMA3"), by = "date", all.x = TRUE)
cfnai <- merge(cfnai, fred("USREC"), by = "date")
write.csv(cfnai, "data/cfnai.csv", row.names = FALSE)

# 3) Eurostat regional indicators, NUTS 2, 2022
base <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT"
get_eurostat <- function(dataset, filters) {
  query <- paste0("c[", names(filters), "]=", filters, collapse = "&")
  url <- paste0(base, "/", dataset, "/1.0?format=csvdata&compress=false&", query)
  read.csv(url, stringsAsFactors = FALSE)
}
pick <- function(d, v) {
  d <- d[nchar(d$geo) == 4, c("geo", "OBS_VALUE")]
  names(d) <- c("geo", v)
  d
}
gdp <- get_eurostat("nama_10r_2gdp", list(TIME_PERIOD = 2022, unit = "PPS_EU27_2020_HAB"))
emp <- get_eurostat("lfst_r_lfe2emprt", list(TIME_PERIOD = 2022, sex = "T", age = "Y20-64", unit = "PC"))
uni <- get_eurostat("edat_lfse_04", list(TIME_PERIOD = 2022, sex = "T", age = "Y25-64",
                                         isced11 = "ED5-8", unit = "PC"))
une <- get_eurostat("lfst_r_lfu3rt", list(TIME_PERIOD = 2022, sex = "T", age = "Y15-74",
                                          isced11 = "TOTAL", unit = "PC"))
regions <- Reduce(function(a, b) merge(a, b, by = "geo"),
                  list(pick(gdp, "gdp_pps"), pick(emp, "employment"),
                       pick(uni, "tertiary"), pick(une, "unemployment")))
n_all <- nrow(regions)
regions <- regions[complete.cases(regions), ]
regions <- regions[!grepl("ZZ$", regions$geo), ]
meta <- jsonlite::fromJSON(paste0(
  "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nama_10r_2gdp",
  "?format=JSON&lang=EN&time=2022&unit=PPS_EU27_2020_HAB"))
geo_label <- unlist(meta$dimension$geo$category$label)
regions$name <- geo_label[regions$geo]
write.csv(regions, "data/eurostat_regions_2022.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Saved", nrow(yields), "yield rows,", nrow(cfnai), "CFNAI rows,",
    nrow(regions), "of", n_all, "regions on", format(Sys.Date()), "\n")
