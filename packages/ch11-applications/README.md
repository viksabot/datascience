# Chapter 11: Applications: Economic Structure and Trade

Reproducibility package for *Data Science for Economists* by Viktor Stojkoski. Part IV. Applications.

## What it reproduces

Every table, number and chart that R produces in this chapter. Running the package creates:

- `output/results.txt`: the code and everything it prints
- `output/figures.pdf`: the charts, in order
- `output/session_info.txt`: the exact software used

## How to run it

1. Open `ch11-applications.Rproj` in RStudio.
2. The first time, run `install_packages.R`, which installs the R packages listed below.
3. Run `run_all.R`. It ends with the message `Done. See the output folder.`

## Software

Written and tested with R version 4.5.0 (2025-04-11 ucrt). Packages: `dplyr`, `ggplot2`, `ggrepel`, `knitr`, `moments`, `readxl`, `scales`, `tidyverse`.

## Data

| File | Source | Type | What it contains |
|---|---|---|---|
| `data-for-real-regression.xlsx` | Course file | Course file | Firm revenue per employee, ECI and HHI for North Macedonia's municipalities, 2015 and 2019. Original source to be documented |
| `econgeo_results.rds` | This book | Derived | Saved results from the EconGeo package, computed from the course files and OEC exports (data-raw/fetch_ch11.R) |
| `eurostat_employment_nuts2.csv` | Eurostat | Public download | Employment by sector in NUTS 2 regions, 2022 (nama_10r_3empers) |
| `export_example.xlsx` | Course file | Fictional | Fictional country and product export table |
| `specialization_example.xlsx` | Course file | Fictional | Fictional regions and industries |
| `trade_matrix_data_2021.xlsx` | Observatory of Economic Complexity | Course file | Exports of 144 countries in 1169 products |

The original source of `data-for-real-regression.xlsx` still has to be documented.

Data from outside sources remain subject to the terms of those sources.

## Files

- `run_all.R`: runs everything, in order
- `install_packages.R`: installs the R packages that this chapter uses
- `code/00_setup.R`: packages, chart theme and colours
- `code/01_chapter.R`: the chapter's code, in book order
- `code/optional_download_data.R`: how the data were downloaded. It needs the internet, and it is written to be run from the book's project, so it is not part of `run_all.R`
- `data/`: dated copies of the data, never edited by hand
- `output/`: created when you run the package
