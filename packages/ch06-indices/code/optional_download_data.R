# Downloads the data used in Chapter 6 and saves dated snapshots in data/.
# The book reads the snapshots, so it still builds if a source changes or is offline.
# Run from the coursebook folder: Rscript data-raw/fetch_ch06.R

library(WDI)
library(dplyr)
library(countrycode)

# 1) World Bank: the three indicators of the HDI-like index, 2020 and 2022
indicators <- c(
  "LifeExp"    = "SP.DYN.LE00.IN",
  "GDPperCap"  = "NY.GDP.PCAP.CD",
  "SecEnroll"  = "SE.SEC.ENRR"
)
for (yr in c(2020, 2022)) {
  raw <- WDI(country = "all", indicator = indicators, start = yr, end = yr)
  write.csv(raw, sprintf("data/wdi_hdi_like_%d.csv", yr), row.names = FALSE)
  cat(yr, nrow(raw), "rows; complete:", sum(complete.cases(raw[, names(indicators)])), "\n")
}

# The cleaned 2022 table from the chapter. countrycode() removes the regional
# aggregates, and it also removes Kosovo, whose code XK has no ISO3 match.
df_raw <- read.csv("data/wdi_hdi_like_2022.csv", stringsAsFactors = FALSE)
df_raw$iso2c[df_raw$country == "Namibia"] <- "NA"  # read.csv reads "NA" as missing
df_clean <- df_raw %>%
  mutate(iso3c = countrycode(iso2c, origin = "iso2c", destination = "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  filter(!country %in% c("World", "High income", "Low income", "Euro area",
                         "European Union", "IDA & IBRD total", "OECD members"),
         !is.na(LifeExp), !is.na(GDPperCap), !is.na(SecEnroll),
         LifeExp > 0, GDPperCap > 0, SecEnroll > 0) %>%
  select(country, iso3c, LifeExp, GDPperCap, SecEnroll)
write.csv(df_clean, "data/wdi_hdi_clean_2022.csv", row.names = FALSE)
cat("Clean countries:", nrow(df_clean), "\n")

# World Governance Indicators: rule of law, 2018 (for the Doing Business box)
library(jsonlite)
wgi <- fromJSON(paste0("https://api.worldbank.org/v2/country/all/indicator/GOV_WGI_RL.EST",
                       "?source=3&date=2018&format=json&per_page=500"))[[2]]
wgi <- data.frame(iso3c = wgi$countryiso3code, country = wgi$country$value, rule_of_law = wgi$value)
wgi <- wgi[!is.na(wgi$rule_of_law) & wgi$iso3c != "", ]
write.csv(wgi, "data/wgi_rule_of_law_2018.csv", row.names = FALSE)
cat("WGI countries:", nrow(wgi), "\n")

# 2) UNDP: official HDI and its components, full time series
url <- "https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Composite_indices_complete_time_series.csv"
undp <- read.csv(url, check.names = FALSE, fileEncoding = "latin1")
keep <- c("iso3", "country", "hdicode", "region", "hdi_rank_2023", "hdi_2023", "le_2023",
          "eys_2023", "mys_2023", "gnipc_2023", "hdi_1990", "hdi_2010")
undp <- undp[, keep]
write.csv(undp, "data/undp_hdi_2023.csv", row.names = FALSE)
cat("UNDP rows:", nrow(undp), "\n")

# 3) Eurostat: three labour market indicators, 2023
base <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT"
get_eurostat <- function(dataset, filters) {
  query <- paste0("c[", names(filters), "]=", filters, collapse = "&")
  url <- paste0(base, "/", dataset, "/1.0?format=csvdata&compress=false&", query)
  read.csv(url, stringsAsFactors = FALSE)
}
aggregates <- c("EA", "EA12", "EA18", "EA19", "EA20", "EA21", "EU", "EU15",
                "EU27_2007", "EU27_2020", "EU28")

unemp <- get_eurostat("une_rt_a", list(
  TIME_PERIOD = 2023, age = "Y15-74", sex = "T", unit = "PC_ACT"
))
emp <- get_eurostat("lfsi_emp_a", list(
  TIME_PERIOD = 2023, age = "Y20-64", sex = "T", unit = "PC_POP", indic_em = "EMP_LFS"
))
neet <- get_eurostat("edat_lfse_20", list(
  TIME_PERIOD = 2023, age = "Y15-24", sex = "T", unit = "PC", training = "NO_FE_NO_NFE", wstatus = "NEMP"
))
pick <- function(d, v) {
  d <- d[!d$geo %in% aggregates, c("geo", "OBS_VALUE")]
  names(d) <- c("geo", v)
  d
}
labour <- Reduce(function(a, b) merge(a, b, by = "geo"),
                 list(pick(emp, "employment"), pick(unemp, "unemployment"), pick(neet, "neet")))
labour <- labour[complete.cases(labour), ]
write.csv(labour, "data/eurostat_labour_2023.csv", row.names = FALSE)
cat("Eurostat labour rows:", nrow(labour), "on", format(Sys.Date()), "\n")
