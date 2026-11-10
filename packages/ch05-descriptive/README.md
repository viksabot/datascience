# Chapter 5: Descriptive Analysis

Reproducibility package for *Data Science for Economists* by Viktor Stojkoski. Part II. Describing the economy.

## What it reproduces

Every table, number and chart that R produces in this chapter. Running the package creates:

- `output/results.txt`: the code and everything it prints
- `output/figures.pdf`: the charts, in order
- `output/session_info.txt`: the exact software used

## How to run it

1. Open `ch05-descriptive.Rproj` in RStudio.
2. The first time, run `install_packages.R`, which installs the R packages listed below.
3. Run `run_all.R`. It ends with the message `Done. See the output folder.`

## Software

Written and tested with R version 4.5.0 (2025-04-11 ucrt). Packages: `dplyr`, `ggplot2`, `knitr`, `moments`, `openxlsx`, `scales`.

## Data

| File | Source | Type | What it contains |
|---|---|---|---|
| `gdp_growth_europe_2015.csv` | Eurostat | Public download | GDP growth in Europe, 2015 (nama_10_gdp) |
| `income_europe_2023.csv` | Eurostat | Public download | Mean and median household income, 2023 (ilc_di03) |
| `nasdaq_daily.csv` | FRED | Public download | NASDAQ Composite daily (NASDAQCOM) |
| `story_ireland_growth.csv` | World Bank | Public download | GDP growth in Ireland and the EU (NY.GDP.MKTP.KD.ZG) |
| `wb_cleaned_data.csv` | World Bank | Derived | Cleaned table of 41 countries built from wdi_2022.csv |

Data from outside sources remain subject to the terms of those sources.

## Files

- `run_all.R`: runs everything, in order
- `install_packages.R`: installs the R packages that this chapter uses
- `code/00_setup.R`: packages, chart theme and colours
- `code/01_chapter.R`: the chapter's code, in book order
- `code/optional_download_data.R`: how the data were downloaded. It needs the internet, and it is written to be run from the book's project, so it is not part of `run_all.R`
- `data/`: dated copies of the data, never edited by hand
- `output/`: created when you run the package
