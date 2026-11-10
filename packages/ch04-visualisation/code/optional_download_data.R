# Downloads the data used in Chapter 4 and saves snapshots in data/.
# Run from the coursebook folder: Rscript data-raw/fetch_ch04.R
# Needs: dplyr, ggplot2, WDI, maps, countrycode.

library(dplyr)
library(ggplot2)
library(WDI)
library(maps)
library(countrycode)

europe_iso3 <- c(
  "ALB", "AUT", "BEL", "BGR", "BIH", "BLR", "CHE", "CYP", "CZE", "DEU",
  "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IRL",
  "ISL", "ITA", "LTU", "LUX", "LVA", "MDA", "MKD", "MLT", "MNE", "NLD",
  "NOR", "POL", "PRT", "ROU", "RUS", "SRB", "SVK", "SVN", "SWE", "TUR",
  "UKR"
)

# 1) The cleaned table from Chapter 3: GDP and population, 2022, 41 countries
wdi_2022 <- read.csv("data/wdi_2022.csv")
df_2022 <- wdi_2022 %>%
  filter(iso3c %in% europe_iso3) %>%
  select(country, iso2c, iso3c, year, GDP, Pop)
write.csv(df_2022, "data/wb_cleaned_data.csv", row.names = FALSE)

# 2) GDP of three Western Balkan countries (current US$, the example in the text)
df_countries <- WDI(
  country = c("MKD", "ALB", "SRB"),
  indicator = c("GDP" = "NY.GDP.MKTP.CD"),
  start = 2002,
  end = 2022
)
write.csv(df_countries, "data/gdp_balkans.csv", row.names = FALSE)

# 3) North Macedonia: GDP in current dollars and in constant local currency
mk <- WDI(
  country = "MKD",
  indicator = c("current_usd" = "NY.GDP.MKTP.CD", "constant_lcu" = "NY.GDP.MKTP.KN"),
  start = 2002,
  end = 2023
)
write.csv(mk, "data/mk_gdp_nominal_real.csv", row.names = FALSE)

# 4) Ten countries, four indicators, twenty years: the overloaded chart
ten <- WDI(
  country = c("DEU", "FRA", "ITA", "ESP", "POL", "GRC", "PRT", "SWE", "ROU", "BGR"),
  indicator = c("GDP growth" = "NY.GDP.MKTP.KD.ZG", "Inflation" = "FP.CPI.TOTL.ZG",
                "Unemployment" = "SL.UEM.TOTL.ZS", "Exports (% GDP)" = "NE.EXP.GNFS.ZS"),
  start = 2003,
  end = 2022
)
write.csv(ten, "data/ten_countries_panel.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 5) Map polygons for Europe
world_map_data <- map_data("world")
world_map_data$iso3c <- countrycode(world_map_data$region,
                                    origin = "country.name",
                                    destination = "iso3c", warn = FALSE)
world_map_data$continent <- countrycode(world_map_data$iso3c,
                                        origin = "iso3c",
                                        destination = "continent", warn = FALSE)
world_map_data_europe <- world_map_data %>%
  filter(continent == "Europe", long > -30, long < 60, lat > 30) %>%
  select(long, lat, group, order, region, iso3c)
write.csv(world_map_data_europe, "data/europe_map.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 6) United States: unemployment and CPI inflation for the Phillips curve story
fred <- function(id) {
  x <- read.csv(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id))
  names(x) <- c("date", "value")
  x$value <- as.numeric(x$value)
  x$year <- as.integer(substr(x$date, 1, 4))
  aggregate(value ~ year, x, mean)
}
u <- fred("UNRATE"); names(u)[2] <- "unemployment"
p <- fred("CPIAUCSL"); names(p)[2] <- "cpi"
p$inflation <- 100 * (p$cpi / c(NA, head(p$cpi, -1)) - 1)
us <- merge(u, p[, c("year", "inflation")])
write.csv(us, "data/us_phillips.csv", row.names = FALSE)

cat("df_2022", nrow(df_2022), "| balkans", nrow(df_countries), "| ten", nrow(ten),
    "| map", nrow(world_map_data_europe), "| regions", length(unique(world_map_data_europe$region)),
    "| us", nrow(us), "on", format(Sys.Date()), "\n")
