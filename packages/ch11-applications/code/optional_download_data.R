# Prepares the data used in Chapter 11 and saves snapshots in data/.
# The book reads the snapshots, so it builds without the EconGeo package.
# To rerun: install.packages("EconGeo"), then, from the coursebook folder,
# Rscript data-raw/fetch_ch11.R
# The four Excel files in data/ are course files.
# trade_matrix_data_2021.xlsx holds export data from the Observatory of Economic Complexity.

library(readxl)
library(EconGeo)

as_matrix <- function(file) {
  d <- as.data.frame(read_excel(file))
  m <- as.matrix(d[, -1])
  rownames(m) <- d[[1]]
  m
}
proximity_of <- function(M) {
  MtM <- t(M) %*% M
  diagonal <- diag(MtM)
  max_matrix <- outer(diagonal, diagonal, pmax)
  p <- MtM / max_matrix
  p[is.nan(p)] <- 0
  diag(p) <- 0
  p
}

# 1) Fictional regional economy
data <- as_matrix("data/specialization_example.xlsx")
R <- location_quotient(data, binary = FALSE)
M <- location_quotient(data, binary = TRUE)
fict <- list(RCA = R, M = M, Mc = diversity(M), Mp = ubiquity(M),
             ECIc = morc(M), HHI = herfindahl(data))

# 2) Fictional export example
ex <- as_matrix("data/export_example.xlsx")
Me <- location_quotient(ex, binary = TRUE)
pe <- proximity_of(Me)
exp_ex <- list(M = Me, PCI = mort(Me), RR = relatedness_density(Me, pe),
               RR_ext = relatedness_density_ext(Me, pe))

# 3) Real exports, 2021
tr <- as_matrix("data/trade_matrix_data_2021.xlsx")
tr[is.na(tr)] <- 0
M21 <- location_quotient(tr, binary = TRUE)
p21 <- proximity_of(M21)
# EconGeo's relatedness_density_ext() rounds to whole numbers, so use the formula.
RR21 <- 100 * t(t(M21 %*% p21) / colSums(p21))
RR21[M21 == 1] <- NA
PCI <- mort(M21)
ECI <- morc(M21)
countries <- c("MKD", "AGO", "ROU", "SGP")
keep <- RR21[countries, , drop = FALSE]
trade <- list(PCI = PCI, ECI = ECI, RR = keep, products = colnames(M21),
              diversity = rowSums(M21),
              mk_exports = tr["MKD", ])
saveRDS(list(fict = fict, exp_ex = exp_ex, trade = trade), "data/econgeo_results.rds")

# 4) Eurostat: employment by sector in NUTS 2 regions, 2022 (thousands)
base <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/3.0/data/dataflow/ESTAT"
url <- paste0(base, "/nama_10r_3empers/1.0?format=csvdata&compress=false&",
              "c[TIME_PERIOD]=2022&c[wstatus]=EMP&c[unit]=THS")
emp <- read.csv(url, stringsAsFactors = FALSE)
sectors <- c("A", "B-E", "F", "G-I", "J", "K", "L", "M_N", "O-Q", "R-U")
emp <- emp[nchar(emp$geo) == 4 & emp$nace_r2 %in% sectors & !grepl("ZZ$", emp$geo),
           c("geo", "nace_r2", "OBS_VALUE")]
wide <- reshape(emp, idvar = "geo", timevar = "nace_r2", direction = "wide")
names(wide) <- sub("OBS_VALUE.", "", names(wide), fixed = TRUE)
wide <- wide[complete.cases(wide), ]
meta <- jsonlite::fromJSON(paste0(
  "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nama_10r_2gdp",
  "?format=JSON&lang=EN&time=2022&unit=PPS_EU27_2020_HAB"))
wide$name <- unlist(meta$dimension$geo$category$label)[wide$geo]
write.csv(wide, "data/eurostat_employment_nuts2.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Saved", nrow(wide), "regions on", format(Sys.Date()), "\n")
