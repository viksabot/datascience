# Downloads the data used in Chapter 3 and saves snapshots in data/.
# The book shows this download code but reads the snapshots, so it can be
# built offline. Run from the coursebook folder: Rscript data-raw/fetch_ch03.R
# Needs: httr, jsonlite, dplyr, WDI, eurostat, countrycode.

library(httr)
library(jsonlite)
library(dplyr)
library(WDI)
library(eurostat)
library(countrycode)

# 1) World Bank API: raw JSON text for GDP and population, 2022
url_gdp <- "https://api.worldbank.org/v2/country/all/indicator/NY.GDP.MKTP.CD?format=json&date=2022:2022&per_page=30000"
url_pop <- "https://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL?format=json&date=2022:2022&per_page=30000"
writeLines(content(GET(url_gdp), as = "text", encoding = "UTF-8"), "data/wb_gdp_2022.json", useBytes = TRUE)
writeLines(content(GET(url_pop), as = "text", encoding = "UTF-8"), "data/wb_pop_2022.json", useBytes = TRUE)

# 2) World Bank through the WDI package
wdi_2022 <- WDI(
  country = "all",
  indicator = c("GDP" = "NY.GDP.MKTP.CD", "Pop" = "SP.POP.TOTL"),
  start = 2022,
  end = 2022
)
write.csv(wdi_2022, "data/wdi_2022.csv", row.names = FALSE)

# 3) MakStat: average monthly net wage by sector (PX-Web API)
url_wages <- paste0("https://makstat.stat.gov.mk/PXWeb/api/v1/en/MakStat/",
                    "PazarNaTrud/Plati/MesecnaBrutoNeto/175_PazTrud_Mk_neto_ml.px")
query <- '{"query": [], "response": {"format": "csv"}}'
res_wages <- POST(url_wages, body = query, content_type_json())
wages_raw <- read.csv(text = content(res_wages, as = "text", encoding = "latin1"),
                      check.names = FALSE)
write.csv(wages_raw, "data/mk_net_wages_wide.csv", row.names = FALSE)

# 4) FRED: US unemployment rate by direct link
unrate <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=UNRATE")
write.csv(unrate, "data/fred_unrate.csv", row.names = FALSE)

# 5) Eurostat through the eurostat package: the code shown in Chapter 3
unemp_raw <- get_eurostat("tps00203", time_format = "num")
infl_raw <- get_eurostat("prc_hicp_aind", time_format = "num")

YEAR <- 2022
UNEMP_UNIT <- "PC_ACT"
INFL_UNIT <- "RCH_A_AVG"

unemp_2022 <- unemp_raw %>%
  filter(TIME_PERIOD == YEAR, unit == UNEMP_UNIT) %>%
  select(geo, unemployment_rate = values)

infl_2022 <- infl_raw %>%
  filter(TIME_PERIOD == YEAR, unit == INFL_UNIT, coicop == "CP00") %>%
  select(geo, inflation_rate = values)

fix_geo <- function(x) {
  recode(as.character(x), EL = "GR", UK = "GB", .default = as.character(x))
}

unemp_2022 <- unemp_2022 %>%
  mutate(iso3c = countrycode(fix_geo(geo), "iso2c", "iso3c", warn = FALSE))
infl_2022 <- infl_2022 %>%
  mutate(iso3c = countrycode(fix_geo(geo), "iso2c", "iso3c", warn = FALSE))

europe_iso3 <- c(
  "ALB", "AUT", "BEL", "BGR", "BIH", "BLR", "CHE", "CYP", "CZE", "DEU",
  "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IRL",
  "ISL", "ITA", "LTU", "LUX", "LVA", "MDA", "MKD", "MLT", "MNE", "NLD",
  "NOR", "POL", "PRT", "ROU", "RUS", "SRB", "SVK", "SVN", "SWE", "TUR",
  "UKR"
)

eurostat_2022 <- unemp_2022 %>%
  filter(iso3c %in% europe_iso3) %>%
  inner_join(
    infl_2022 %>% filter(iso3c %in% europe_iso3) %>%
      select(iso3c, inflation_rate),
    by = "iso3c"
  ) %>%
  mutate(year = YEAR) %>%
  select(iso3c, year, unemployment_rate, inflation_rate) %>%
  arrange(iso3c)
write.csv(eurostat_2022, "data/eurostat_2022_tidy.csv", row.names = FALSE)

cat("wdi", nrow(wdi_2022), "| wages", dim(wages_raw), "| unrate", nrow(unrate),
    "| eurostat", nrow(eurostat_2022), "| units", sort(unique(as.character(unemp_raw$unit))),
    "|", sort(unique(as.character(infl_raw$unit))), "on", format(Sys.Date()), "\n")
