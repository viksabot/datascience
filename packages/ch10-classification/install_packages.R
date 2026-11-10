# Installs the R packages that this chapter uses. Run it once.
needed <- c("boot", "dplyr", "ggplot2", "knitr", "moments", "readxl", "scales", "tidyverse")
missing <- setdiff(needed, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
cat("All packages are installed.\n")
