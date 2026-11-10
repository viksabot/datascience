# Downloads the data used in Chapter 12 and saves dated snapshots in data/.
# Run from the coursebook folder: Rscript data-raw/fetch_ch12.R

# 1) Kenneth French Data Library: monthly returns, percent
french <- function(file) {
  url <- paste0("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/",
                file, "_CSV.zip")
  zipped <- tempfile(fileext = ".zip")
  download.file(url, zipped, mode = "wb", quiet = TRUE)
  name <- unzip(zipped, list = TRUE)$Name[1]
  readLines(unz(zipped, name), warn = FALSE)
}
# Keeps the first monthly table in a file: rows that start with YYYYMM
first_table <- function(lines, header_pattern) {
  head_at <- grep(header_pattern, lines)[1]
  rows <- lines[(head_at + 1):length(lines)]
  stop_at <- which(!grepl("^\\s*[0-9]{6},", rows))[1]
  rows <- rows[seq_len(stop_at - 1)]
  d <- read.csv(text = c(lines[head_at], rows), check.names = FALSE,
                strip.white = TRUE)
  names(d)[1] <- "month"
  d
}
ind <- first_table(french("48_Industry_Portfolios"), "^\\s*,\\s*Agric")
ff <- first_table(french("F-F_Research_Data_Factors"), "^\\s*,\\s*Mkt-RF")
ind <- ind[ind$month >= 200601 & ind$month <= 202512, ]
ff <- ff[ff$month >= 200601 & ff$month <= 202512, c("month", "Mkt-RF", "RF")]
names(ff) <- c("month", "market_excess", "riskfree")
complete <- sapply(ind[-1], function(x) all(x > -99))
ind <- ind[, c(TRUE, complete)]
write.csv(merge(ff, ind, by = "month"), "data/french_industries_monthly.csv",
          row.names = FALSE)

# 2) OECD share price indices through FRED, and the VIX
fred <- function(id) {
  d <- read.csv(paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id))
  names(d) <- c("date", "value")
  d$value <- suppressWarnings(as.numeric(d$value))
  d
}
countries <- c(Greece = "GR", Turkiye = "TR", Hungary = "HU", Slovenia = "SI",
               Poland = "PL", Czechia = "CZ", Germany = "DE")
prices <- NULL
for (n in names(countries)) {
  d <- fred(paste0("SPASTT01", countries[[n]], "M661N"))
  names(d)[2] <- n
  prices <- if (is.null(prices)) d else merge(prices, d, by = "date")
}
vix <- fred("VIXCLS")
vix <- vix[!is.na(vix$value), ]
vix$date <- paste0(substr(vix$date, 1, 7), "-01")
prices <- merge(prices, aggregate(value ~ date, vix, mean), by = "date")
names(prices)[names(prices) == "value"] <- "VIX"
write.csv(prices, "data/oecd_share_prices.csv", row.names = FALSE)

# 3) Boston HMDA data (Munnell et al. 1996), as distributed in the AER package
hmda <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/AER/HMDA.csv")
hmda$rownames <- NULL
write.csv(hmda, "data/hmda_boston.csv", row.names = FALSE)

# 4) World Bank indicators: latest value in 2019-2023 for each country
wb <- function(code) {
  url <- paste0("https://api.worldbank.org/v2/country/all/indicator/", code,
                "?format=json&per_page=20000&date=2019:2023")
  x <- jsonlite::fromJSON(url)[[2]]
  x <- x[!is.na(x$value), c("countryiso3code", "date", "value")]
  x <- x[order(x$countryiso3code, x$date, decreasing = TRUE), ]
  x <- x[!duplicated(x$countryiso3code), c("countryiso3code", "value")]
  names(x) <- c("iso3c", code)
  x
}
codes <- c("FS.AST.PRVT.GD.ZS", "FD.AST.PRVT.GD.ZS", "FX.OWN.TOTL.ZS", "FB.ATM.TOTL.P5",
           "FB.CBK.BRCH.P5", "FB.AST.NPER.ZS", "NY.GDP.PCAP.PP.KD")
fd <- Reduce(function(a, b) merge(a, b, by = "iso3c", all = TRUE), lapply(codes, wb))
meta <- jsonlite::fromJSON("https://api.worldbank.org/v2/country?format=json&per_page=400")[[2]]
meta <- meta[meta$region$value != "Aggregates",
             c("id", "name")]
meta$region <- jsonlite::fromJSON("https://api.worldbank.org/v2/country?format=json&per_page=400")[[2]]$region$value[
  match(meta$id, jsonlite::fromJSON("https://api.worldbank.org/v2/country?format=json&per_page=400")[[2]]$id)]
names(meta) <- c("iso3c", "country", "region")
fd <- merge(meta, fd, by = "iso3c")
names(fd) <- c("iso3c", "country", "region", "credit", "bank_credit", "accounts", "atms",
               "branches", "npl", "gdp_pc")
write.csv(fd, "data/wb_financial_development.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Saved on", format(Sys.Date()), "\n")
