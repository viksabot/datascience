# Installs every package used in the reproducibility packages.
needed <- c("boot", "dplyr", "ggplot2", "ggrepel", "httr", "jsonlite", "knitr", "moments", "openxlsx", "readxl", "scales", "tidyr", "tidyverse", "treemapify")
missing <- setdiff(needed, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
