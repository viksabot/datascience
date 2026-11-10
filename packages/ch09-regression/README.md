# Chapter 9: Linear Regression

Reproducibility package for *Data Science for Economists* by Viktor Stojkoski. Part III. Statistical learning and policy prediction.

## What it reproduces

Every table, number and chart that R produces in this chapter. Running the package creates:

- `output/results.txt`: the code and everything it prints
- `output/figures.pdf`: the charts, in order
- `output/session_info.txt`: the exact software used

## How to run it

1. Open `ch09-regression.Rproj` in RStudio.
2. The first time, run `install_packages.R`, which installs the R packages listed below.
3. Run `run_all.R`. It ends with the message `Done. See the output folder.`

## Software

Written and tested with R version 4.5.0 (2025-04-11 ucrt). Packages: `boot`, `dplyr`, `ggplot2`, `knitr`, `moments`, `readxl`, `scales`, `tidyverse`.

## Data

| File | Source | Type | What it contains |
|---|---|---|---|
| `gravity_data.xlsx` | Course file | Course file | Bilateral trade with GDP and distance. Original source to be documented |
| `regression_example_data.xlsx` | Course file | Course file | Country indicators for the regression and classification examples. Original source to be documented |
| `story_bordeaux_wine.csv` | Ashenfelter et al. (1995) | Public download | Weather and prices of Bordeaux vintages |
| `us_okun.csv` | FRED | Public download | US GDP and unemployment (GDPCA and UNRATE) |

The original source of `gravity_data.xlsx`, `regression_example_data.xlsx` still has to be documented.

Data from outside sources remain subject to the terms of those sources.

## Files

- `run_all.R`: runs everything, in order
- `install_packages.R`: installs the R packages that this chapter uses
- `code/00_setup.R`: packages, chart theme and colours
- `code/01_chapter.R`: the chapter's code, in book order
- `code/optional_download_data.R`: how the data were downloaded. It needs the internet, and it is written to be run from the book's project, so it is not part of `run_all.R`
- `data/`: dated copies of the data, never edited by hand
- `output/`: created when you run the package
