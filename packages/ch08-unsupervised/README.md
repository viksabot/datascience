# Chapter 8: Unsupervised Learning

Reproducibility package for *Data Science for Economists* by Viktor Stojkoski. Part III. Statistical learning and policy prediction.

## What it reproduces

Every table, number and chart that R produces in this chapter. Running the package creates:

- `output/results.txt`: the code and everything it prints
- `output/figures.pdf`: the charts, in order
- `output/session_info.txt`: the exact software used

## How to run it

1. Open `ch08-unsupervised.Rproj` in RStudio.
2. The first time, run `install_packages.R`, which installs the R packages listed below.
3. Run `run_all.R`. It ends with the message `Done. See the output folder.`

## Software

Written and tested with R version 4.5.0 (2025-04-11 ucrt). Packages: `dplyr`, `ggplot2`, `ggrepel`, `knitr`, `moments`, `readxl`, `scales`, `tidyr`, `treemapify`.

## Data

| File | Source | Type | What it contains |
|---|---|---|---|
| `cfnai.csv` | FRED | Public download | Chicago Fed National Activity Index and US recession dates (CFNAI, CFNAIMA3, USREC) |
| `eurostat_regions_2022.csv` | Eurostat | Public download | NUTS 2 regional indicators, 2022 (nama_10r_2gdp, lfst_r_lfe2emprt, edat_lfse_04, lfst_r_lfu3rt) |
| `us_treasury_yields.csv` | FRED | Public download | US Treasury yields (GS1, GS2, GS3, GS5, GS7, GS10) |
| `usa_imports_machinery_2023.xlsx` | Course file | Course file | US imports of machinery and transport products. Original source to be documented |
| `wdi_hdi_clean_2022.csv` | World Bank | Derived | Cleaned indicators for an HDI-like index |

The original source of `usa_imports_machinery_2023.xlsx` still has to be documented.

Data from outside sources remain subject to the terms of those sources.

## Files

- `run_all.R`: runs everything, in order
- `install_packages.R`: installs the R packages that this chapter uses
- `code/00_setup.R`: packages, chart theme and colours
- `code/01_chapter.R`: the chapter's code, in book order
- `code/optional_download_data.R`: how the data were downloaded. It needs the internet, and it is written to be run from the book's project, so it is not part of `run_all.R`
- `data/`: dated copies of the data, never edited by hand
- `output/`: created when you run the package
