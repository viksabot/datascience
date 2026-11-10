# Chapter 1: Why Data Science for Economists?

Reproducibility package for *Data Science for Economists* by Viktor Stojkoski. Part I. Foundations.

## What it reproduces

Every table, number and chart that R produces in this chapter. Running the package creates:

- `output/results.txt`: the code and everything it prints
- `output/figures.pdf`: the charts, in order
- `output/session_info.txt`: the exact software used

## How to run it

1. Open `ch01-why.Rproj` in RStudio.
2. The first time, run `install_packages.R`, which installs the R packages listed below.
3. Run `run_all.R`. It ends with the message `Done. See the output folder.`

## Software

Written and tested with R version 4.5.0 (2025-04-11 ucrt). Packages: `dplyr`, `ggplot2`, `knitr`, `moments`, `scales`, `treemapify`.

## Data

| File | Source | Type | What it contains |
|---|---|---|---|
| `mk_exports_hs2.csv` | CEPII BACI | Public download | North Macedonia's exports by HS two-digit group, 2024 (BACI HS22, version 202601) |
| `uk_solar_imports_annual.csv` | CEPII BACI | Public download | UK imports of solar panels by value and weight, 2022 to 2024 (HS 854143; BACI HS22, version 202601) |

Data from outside sources remain subject to the terms of those sources.

## Files

- `run_all.R`: runs everything, in order
- `install_packages.R`: installs the R packages that this chapter uses
- `code/00_setup.R`: packages, chart theme and colours
- `code/01_chapter.R`: the chapter's code, in book order
- `code/optional_download_data.R`: how the data were downloaded. It needs the internet, and it is written to be run from the book's project, so it is not part of `run_all.R`
- `data/`: dated copies of the data, never edited by hand
- `output/`: created when you run the package
