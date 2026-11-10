# Downloads the data used in Chapter 1 and saves dated snapshots in data/.
# Run from the coursebook folder: Rscript data-raw/fetch_ch01.R
# Trade data: CEPII BACI, version 202601, HS 2022 (Etalab 2.0 open licence, cite Gaulier and
# Zignago 2010). The download is 300 MB and BACI reports values in thousands of US dollars
# and quantities in metric tons. Uses the data.table package to read the large files.

library(data.table)

baci_zip <- file.path(Sys.getenv("TEMP"), "BACI_HS22_V202601.zip")
if (!file.exists(baci_zip)) {
  options(timeout = 1800)
  download.file("https://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS22_V202601.zip",
                baci_zip, mode = "wb")
}
baci_dir <- tempfile("baci-")
unzip(baci_zip, exdir = baci_dir)
read_baci <- function(year) {
  fread(file.path(baci_dir, sprintf("BACI_HS22_Y%d_V202601.csv", year)),
        colClasses = c(k = "character", q = "character"))[, q := suppressWarnings(as.numeric(q))]
}

# 1) UK annual imports of solar panels (HS 854143, exists from 2022)
solar <- rbindlist(lapply(2022:2024, function(y) {
  b <- read_baci(y)[j == 826 & k == "854143"]
  data.table(year = y, value_usd = sum(b$v) * 1000, tons = sum(b$q, na.rm = TRUE),
             flows = nrow(b), flows_without_quantity = sum(is.na(b$q)))
}))
write.csv(solar, "data/uk_solar_imports_annual.csv", row.names = FALSE)

# 2) North Macedonia's exports by HS chapter, 2024
b <- read_baci(2024)[i == 807]
exports <- b[, .(value_usd = sum(v) * 1000), by = .(hs2 = substr(k, 1, 2))][order(hs2)]
write.csv(exports, "data/mk_exports_hs2.csv", row.names = FALSE)
unlink(baci_dir, recursive = TRUE)
# ---- end of BACI ----

# 3) Greek general government balance, % of GDP (Eurostat)
url <- paste0("https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT/",
              "gov_10dd_edpt1/1.0?format=csvdata&compress=false",
              "&c[geo]=EL&c[unit]=PC_GDP&c[sector]=S13&c[na_item]=B9")
greece <- read.csv(url)[, c("TIME_PERIOD", "OBS_VALUE")]
names(greece) <- c("year", "balance_pct_gdp")
write.csv(greece, "data/greece_balance.csv", row.names = FALSE)

cat("Saved", nrow(solar), "solar years,", nrow(exports), "export rows,",
    nrow(greece), "Greek years on", format(Sys.Date()), "\n")
