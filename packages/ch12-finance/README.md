# Chapter 12: Applications: Finance

Reproducibility package for *Data Science for Economists* by Viktor Stojkoski. Part IV. Applications.

## What it reproduces

Every table, number and chart that R produces in this chapter. Running the package creates:

- `output/results.txt`: the code and everything it prints
- `output/figures.pdf`: the charts, in order
- `output/session_info.txt`: the exact software used

## How to run it

1. Open `ch12-finance.Rproj` in RStudio.
2. The first time, run `install_packages.R`, which installs the R packages listed below.
3. Run `run_all.R`. It ends with the message `Done. See the output folder.`

## Software

Written and tested with R version 4.5.0 (2025-04-11 ucrt). Packages: `dplyr`, `ggplot2`, `ggrepel`, `knitr`, `moments`, `scales`.

## Data

| File | Source | Type | What it contains |
|---|---|---|---|
| `french_industries_monthly.csv` | Kenneth French Data Library | Public download | Monthly returns on 48 US industries and Fama/French factors, 2006 to 2025 |
| `hmda_boston.csv` | R package AER | Public download | Boston mortgage applications (Munnell et al. 1996) |
| `oecd_share_prices.csv` | OECD through FRED | Public download | Monthly share price indices and the average VIX |
| `story_account_ownership.csv` | World Bank Global Findex | Public download | Adults with an account in Kenya and Sub-Saharan Africa (FX.OWN.TOTL.ZS) |
| `story_asian_exchange_rates.csv` | FRED | Public download | Baht, won and ringgit against the dollar, 1997 and 1998 (DEXTHUS, DEXKOUS, DEXMAUS) |
| `wb_financial_development.csv` | World Bank | Public download | Credit, accounts, ATMs, branches, non-performing loans and GDP per person, latest for 2019 to 2023 |

Data from outside sources remain subject to the terms of those sources.

## Files

- `run_all.R`: runs everything, in order
- `install_packages.R`: installs the R packages that this chapter uses
- `code/00_setup.R`: packages, chart theme and colours
- `code/01_chapter.R`: the chapter's code, in book order
- `code/optional_download_data.R`: how the data were downloaded. It needs the internet, and it is written to be run from the book's project, so it is not part of `run_all.R`
- `data/`: dated copies of the data, never edited by hand
- `output/`: created when you run the package
