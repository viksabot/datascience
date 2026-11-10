# Chapter 3: Data and Working with Data

Reproducibility package for *Data Science for Economists* by Viktor Stojkoski. Part I. Foundations.

## What it reproduces

Every table, number and chart that R produces in this chapter. Running the package creates:

- `output/results.txt`: the code and everything it prints
- `output/figures.pdf`: the charts, in order
- `output/session_info.txt`: the exact software used

## How to run it

1. Open `ch03-data.Rproj` in RStudio.
2. The first time, run `install_packages.R`, which installs the R packages listed below.
3. Run `run_all.R`. It ends with the message `Done. See the output folder.`

## Software

Written and tested with R version 4.5.0 (2025-04-11 ucrt). Packages: `dplyr`, `ggplot2`, `httr`, `jsonlite`, `knitr`, `moments`, `readxl`, `scales`, `tidyr`.

## Data

| File | Source | Type | What it contains |
|---|---|---|---|
| `GDP.xls` | World Bank | Public download | GDP in current US dollars (NY.GDP.MKTP.CD), Excel download |
| `POP.xls` | World Bank | Public download | Total population (SP.POP.TOTL), Excel download |
| `eurostat_2022_tidy.csv` | Eurostat | Public download | Tidy table combining tps00203 and prc_hicp_aind for 2022 |
| `fred_unrate.csv` | FRED | Public download | US unemployment rate (UNRATE) |
| `mk_net_wages_wide.csv` | State Statistical Office of North Macedonia | Public download | Net wages (MakStat table 175) |
| `wb_gdp_2022.json` | World Bank API | Public download | GDP, 2022, as returned by the API |
| `wb_pop_2022.json` | World Bank API | Public download | Population, 2022, as returned by the API |
| `wdi_2022.csv` | World Bank | Public download | GDP and population for all countries through the WDI package |

Data from outside sources remain subject to the terms of those sources.

## Files

- `run_all.R`: runs everything, in order
- `install_packages.R`: installs the R packages that this chapter uses
- `code/00_setup.R`: packages, chart theme and colours
- `code/01_chapter.R`: the chapter's code, in book order
- `code/optional_download_data.R`: how the data were downloaded. It needs the internet, and it is written to be run from the book's project, so it is not part of `run_all.R`
- `data/`: dated copies of the data, never edited by hand
- `output/`: created when you run the package
