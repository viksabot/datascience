# Chapter 3: Data and Working with Data
# The code of the chapter, in the order of the book.
# Lines that begin with ## were not run in the book, usually because they
# need the internet or a package that is not always installed.

# ---- fig-greece-2009 ----
estimates <- data.frame(
  when = c("Spring 2009", "Before Oct 2009\nelection", "Oct 2009",
           "Apr 2010", "Nov 2010"),
  deficit = c(3.7, 6.0, 12.5, 13.6, 15.4)
)
estimates$when <- factor(estimates$when, levels = estimates$when)
ggplot(estimates, aes(when, deficit)) +
  geom_col(fill = book_blue, width = 0.62) +
  geom_text(aes(label = paste0(deficit, "%")), vjust = -0.45, size = 3.1) +
  geom_hline(yintercept = 3, linetype = "dashed", color = book_orange) +
  annotate("text", x = 5.42, y = 4.3, label = "EU limit\n3%", hjust = 0,
           color = "#5A6675", size = 2.8, lineheight = 0.9) +
  scale_x_discrete(expand = expansion(add = c(0.6, 0.95))) +
  scale_y_continuous(limits = c(0, 17.5), expand = c(0, 0)) +
  labs(x = NULL, y = "Deficit, % of GDP")


# ---- tbl-cross ----
library(tidyr)
wages_wide <- read.csv("data/mk_net_wages_wide.csv", check.names = FALSE)
wages_long <- wages_wide %>%
  pivot_longer(-Sector, names_to = "month", values_to = "net_wage")
wages_long %>%
  filter(month == "2024M09", !grepl("Total", Sector)) %>%
  select(Sector, `Net wage` = net_wage) %>%
  head(6) %>%
  kable()


# ---- tbl-ts ----
wages_long %>%
  filter(grepl("Total", Sector)) %>%
  select(Month = month, `Net wage` = net_wage) %>%
  head(6) %>%
  kable()


# ---- tbl-panel ----
wages_long %>%
  filter(grepl("^\\((A|B|C)\\)", Sector), month %in% c("2005M01", "2005M02", "2005M03")) %>%
  select(Sector, Month = month, `Net wage` = net_wage) %>%
  kable()


# ---- d-setup-general ----
# ##############################################
# ## Example of setting the working directory
# ## and loading the required packages
# ##############################################
# 
# # 1) Set the working directory
# # Adjust this to the location of your files
# setwd("/path/to/your/folder")  # e.g. setwd("C:/Users/Ime/Documents/Data")
# 
# # Check the current working directory
# getwd()
# 
# # 2) Install the required packages (if not already installed)
# # Uncomment these lines the first time you use the packages:
# # install.packages("readxl")
# # install.packages("dplyr")


# ---- d-setup-load ----
# 3) Load the required packages
library(readxl)  # for reading Excel files
library(dplyr)   # for working with and manipulating data


# ---- d-file-names ----
# Define the file names
gdp_file <- "data/GDP.xls"  # GDP file
pop_file <- "data/POP.xls"  # population file


# ---- d-read-excel ----
# Read the Excel files
df_gdp <- read_excel(gdp_file, sheet = 1, skip = 3)  # skip first 3 rows
df_pop <- read_excel(pop_file, sheet = 1, skip = 3)  # skip first 3 rows


# ---- d-iso-codes ----
# Define a vector of ISO3 codes for European countries
europe_iso3 <- c(
  "ALB", "AUT", "BEL", "BGR", "BIH", "BLR", "CHE", "CYP", "CZE",
  "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV",
  "HUN", "IRL", "ISL", "ITA", "LTU", "LUX", "LVA", "MDA", "MKD",
  "MLT", "MNE", "NLD", "NOR", "POL", "PRT", "ROU", "RUS", "SRB",
  "SVK", "SVN", "SWE", "TUR", "UKR"
)


# ---- d-names ----
# Check the column names
names(df_gdp)


# ---- d-filter ----
# Filter the GDP data
df_gdp_eu <- df_gdp %>%
  filter(`Country Code` %in% europe_iso3)

# Filter the population data
df_pop_eu <- df_pop %>%
  filter(`Country Code` %in% europe_iso3)


# ---- d-setdiff ----
setdiff(c(europe_iso3, "MVA"), df_gdp$`Country Code`)
nrow(df_gdp_eu)


# ---- d-select ----
# Select and rename the GDP columns
df_gdp_2022 <- df_gdp_eu %>%
  select(`Country Code`, `Country Name`, `2022`) %>%
  rename(GDP_2022 = `2022`)

# Select and rename the population columns
df_pop_2022 <- df_pop_eu %>%
  select(`Country Code`, `Country Name`, `2022`) %>%
  rename(Pop_2022 = `2022`)


# ---- d-join ----
# Merge the GDP and population data
df_2022 <- left_join(
  df_gdp_2022,
  df_pop_2022,
  by = c("Country Code", "Country Name")
)


# ---- d-head ----
# Preview the final data
head(df_2022)


# ---- d-write ----
# Save the final table to a CSV file
write.csv(df_2022, "wb_cleaned_data.csv", row.names = FALSE)


# ---- d-fred-show ----
# unrate <- read.csv(
#   "https://fred.stlouisfed.org/graph/fredgraph.csv?id=UNRATE"
# )


# ---- d-fred ----
unrate <- read.csv("data/fred_unrate.csv")


# ---- d-fred-tail ----
tail(unrate, 3)


# ---- d-api-libs ----
# Install the required packages (if not already installed)
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("dplyr")

# Load the libraries
library(httr)
library(jsonlite)
library(dplyr)


# ---- d-url-gdp ----
url_gdp <- paste0(
  "https://api.worldbank.org/v2/country/all/indicator/NY.GDP.MKTP.CD",
  "?format=json&date=2022:2022&per_page=30000"
)


# ---- d-get-gdp ----
# res_gdp <- GET(url_gdp)


# ---- d-content-gdp ----
# json_gdp <- content(res_gdp, as = "text")


# ---- d-load-json-gdp ----
json_gdp <- paste(readLines("data/wb_gdp_2022.json", warn = FALSE, encoding = "UTF-8"), collapse = "")


# ---- d-fromjson-gdp ----
list_gdp <- fromJSON(json_gdp)


# ---- d-df-gdp ----
df_gdp_raw <- as.data.frame(list_gdp[[2]])


# ---- d-api-filter ----
df_gdp_eur <- df_gdp_raw %>%
  filter(countryiso3code %in% europe_iso3)


# ---- d-api-rename ----
df_gdp_eur <- df_gdp_eur %>%
  rename(
    iso3c = countryiso3code,
    GDP_2022 = value
  ) %>%
  select(iso3c, date, GDP_2022)


# ---- d-get-pop ----
# # Download the population data
# url_pop <- paste0(
#   "https://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL",
#   "?format=json&date=2022:2022&per_page=30000"
# )
# res_pop <- GET(url_pop)
# 
# # Convert the response to JSON text
# json_pop <- content(res_pop, as = "text")


# ---- d-load-json-pop ----
json_pop <- paste(readLines("data/wb_pop_2022.json", warn = FALSE, encoding = "UTF-8"), collapse = "")


# ---- d-api-pop ----
list_pop   <- fromJSON(json_pop)
df_pop_raw <- as.data.frame(list_pop[[2]])

# Filter for European countries
df_pop_eur <- df_pop_raw %>%
  filter(countryiso3code %in% europe_iso3) %>%
  rename(
    iso3c    = countryiso3code,
    Pop_2022 = value
  ) %>%
  select(iso3c, date, Pop_2022)


# ---- d-api-join ----
# Combine the data
df_2022 <- left_join(df_gdp_eur, df_pop_eur, by = c("iso3c", "date"))


# ---- d-api-head ----
# Preview the data
head(df_2022)


# ---- d-api-write ----
# Save the processed data
write.csv(df_2022, "wb_api_cleaned_data.csv", row.names = FALSE)


# ---- d-wdi-install ----
# install.packages("WDI")
# install.packages("countrycode")
# install.packages("dplyr")


# ---- d-wdi-libs ----
# library(WDI)
# library(countrycode)
# library(dplyr)


# ---- d-wdi-call ----
# df_2022 <- WDI(
#   country = "all",  # data for all countries
#   indicator = c("GDP" = "NY.GDP.MKTP.CD",  # GDP
#                 "Pop" = "SP.POP.TOTL"),    # population
#   start = 2022,     # first year
#   end = 2022        # last year
# )


# ---- d-wdi-load ----
df_2022 <- read.csv("data/wdi_2022.csv")


# ---- d-countrycode ----
# df <- df %>%
#   mutate(iso3c = countrycode(iso2c, origin = "iso2c",
#                              destination = "iso3c"))


# ---- d-wdi-filter ----
df_2022 <- df_2022 %>%
  filter(iso3c %in% europe_iso3)

head(df_2022)


# ---- d-eu-libs ----
# # install.packages("eurostat")
# # install.packages("dplyr")
# # install.packages("countrycode")
# 
# library(eurostat)
# library(dplyr)
# library(countrycode)


# ---- d-eu-toc ----
# toc <- get_eurostat_toc()
# 
# # Which tables are about unemployment?
# toc_unemp <- toc %>%
#   filter(grepl("unemployment", title, ignore.case = TRUE))
# head(toc_unemp)


# ---- d-eu-get ----
# unemp_raw <- get_eurostat("tps00203", time_format = "num")
# 
# toc %>% filter(grepl("HICP", title, ignore.case = TRUE)) %>% head()
# infl_raw <- get_eurostat("prc_hicp_aind", time_format = "num")


# ---- d-eu-units ----
# sort(unique(unemp_raw$unit))
# #> "PC_ACT"  "PC_POP"  "THS_PER"
# sort(unique(infl_raw$unit))
# #> "CID_EA"  "INX_A_AVG"  "RCH_A_AVG"


# ---- d-eu-filter ----
# YEAR <- 2022
# UNEMP_UNIT <- "PC_ACT"
# INFL_UNIT <- "RCH_A_AVG"
# 
# unemp_2022 <- unemp_raw %>%
#   filter(TIME_PERIOD == YEAR, unit == UNEMP_UNIT) %>%
#   select(geo, unemployment_rate = values)
# 
# infl_2022 <- infl_raw %>%
#   filter(TIME_PERIOD == YEAR, unit == INFL_UNIT, coicop == "CP00") %>%
#   select(geo, inflation_rate = values)


# ---- d-eu-geo ----
# fix_geo <- function(x) {
#   recode(as.character(x), EL = "GR", UK = "GB",
#          .default = as.character(x))
# }
# 
# unemp_2022 <- unemp_2022 %>%
#   mutate(iso3c = countrycode(fix_geo(geo), "iso2c", "iso3c",
#                              warn = FALSE))
# 
# infl_2022 <- infl_2022 %>%
#   mutate(iso3c = countrycode(fix_geo(geo), "iso2c", "iso3c",
#                              warn = FALSE))


# ---- d-eu-join ----
# unemp_2022_eu <- unemp_2022 %>% filter(iso3c %in% europe_iso3)
# infl_2022_eu  <- infl_2022  %>% filter(iso3c %in% europe_iso3)
# 
# df_2022 <- unemp_2022_eu %>%
#   inner_join(infl_2022_eu %>% select(iso3c, inflation_rate),
#              by = "iso3c") %>%
#   mutate(year = 2022) %>%
#   select(iso3c, year, unemployment_rate, inflation_rate) %>%
#   arrange(iso3c)


# ---- d-eu-load ----
df_2022 <- read.csv("data/eurostat_2022_tidy.csv")


# ---- d-eu-head ----
head(df_2022)
nrow(df_2022)


# ---- d-mak-get ----
# url_wages <- paste0(
#   "https://makstat.stat.gov.mk/PXWeb/api/v1/en/MakStat/",
#   "PazarNaTrud/Plati/MesecnaBrutoNeto/175_PazTrud_Mk_neto_ml.px"
# )
# query <- '{"query": [], "response": {"format": "csv"}}'
# res_wages <- POST(url_wages, body = query, content_type_json())
# 
# wages_raw <- read.csv(
#   text = content(res_wages, as = "text", encoding = "latin1"),
#   check.names = FALSE
# )


# ---- d-mak-load ----
wages_raw <- read.csv("data/mk_net_wages_wide.csv", check.names = FALSE)


# ---- d-mak-dim ----
dim(wages_raw)
names(wages_raw)[1:4]


# ---- d-mak-pivot ----
library(tidyr)

wages <- wages_raw %>%
  pivot_longer(
    cols = -Sector,            # every column except Sector
    names_to = "month",        # column names go into "month"
    values_to = "net_wage"     # cell values go into "net_wage"
  )

dim(wages)
head(wages, 3)


# ---- fig-wages ----
total <- wages %>%
  filter(grepl("Total", Sector)) %>%
  mutate(date = as.Date(paste0(sub("M", "-", month), "-01")))

ggplot(total, aes(date, net_wage)) +
  geom_line(color = "#1F66A8", linewidth = 0.7) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  labs(x = NULL, y = "Denars per month")


# ---- repro-run-all ----
# # run_all.R: reproduces every table and figure in this project
# set.seed(123)   # fixes any random numbers used later
# 
# source("code/01_download.R")
# source("code/02_clean.R")
# source("code/03_analysis.R")
# source("code/04_figures.R")
# 
# # Record the exact software used
# writeLines(capture.output(sessionInfo()), "output/session_info.txt")


# ---- repro-versions ----
R.version.string
packageVersion("ggplot2")


# ---- repro-renv ----
# install.packages("renv")
# renv::init()      # gives the project its own package library
# renv::snapshot()  # records the versions in the file renv.lock
# renv::restore()   # on another computer: installs those versions


# ---- repro-example-check ----
# my_table <- read.csv("output/wb_cleaned_data.csv")
# nrow(my_table)
# my_table[my_table$Country.Code == "MKD", ]

