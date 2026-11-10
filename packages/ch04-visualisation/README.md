# Chapter 4: Visualising Economic Data

Reproducibility package for *Data Science for Economists* by Viktor Stojkoski. Part II. Describing the economy.

## What it reproduces

Every table, number and chart that R produces in this chapter. Running the package creates:

- `output/results.txt`: the code and everything it prints
- `output/figures.pdf`: the charts, in order
- `output/session_info.txt`: the exact software used

## How to run it

1. Open `ch04-visualisation.Rproj` in RStudio.
2. The first time, run `install_packages.R`, which installs the R packages listed below.
3. Run `run_all.R`. It ends with the message `Done. See the output folder.`

## Software

Written and tested with R version 4.5.0 (2025-04-11 ucrt). Packages: `dplyr`, `ggplot2`, `ggrepel`, `knitr`, `moments`, `readxl`, `scales`, `tidyr`, `treemapify`.

## Data

| File | Source | Type | What it contains |
|---|---|---|---|
| `europe_map.csv` | R package maps | Derived | Map outlines of Europe prepared for Chapter 4 |
| `gdp_balkans.csv` | World Bank | Public download | GDP of Balkan economies from WDI |
| `mk_gdp_nominal_real.csv` | World Bank | Public download | North Macedonia's GDP in current dollars and constant denars |
| `ten_countries_panel.csv` | World Bank | Public download | Panel of ten countries from WDI |
| `us_phillips.csv` | FRED | Public download | US unemployment and inflation (UNRATE and CPIAUCSL) |
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
