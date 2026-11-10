# Installs the R packages that this chapter uses. Run it once.
needed <- c("dplyr", "ggplot2", "httr", "jsonlite", "knitr", "moments", "readxl", "scales", "tidyr")
missing <- setdiff(needed, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
cat("All packages are installed.\n")
