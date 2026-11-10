# Companion to Data Science for Economists

Open `dashboard.html` for an overview of everything here.

- `data/`: every dataset used in the book, with `data_catalogue.csv` saying where each came from
- `packages/`: a reproducibility package for each chapter
- `downloads/`: the same packages as zip files, which the dashboard links to
- `install_packages.R`: installs the R packages that the chapters use
- `reproduction_report.csv`: the result of running each package from scratch

To reproduce a chapter, open the `.Rproj` file in its folder in RStudio and run `run_all.R`.

To regenerate this folder, run `Rscript companion-tools/build_companion.R` from the coursebook folder.
