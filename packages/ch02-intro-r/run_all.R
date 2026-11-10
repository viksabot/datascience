# Reproduces the results of Chapter 2: Introduction to R
# Open the .Rproj file in RStudio and then run this script.
if (!file.exists("code/01_chapter.R")) stop("Open the .Rproj file in this folder first.")
dir.create("output", showWarnings = FALSE)
source("code/00_setup.R", encoding = "UTF-8")
pdf("output/figures.pdf", width = 7, height = 5)
results <- file("output/results.txt", open = "wt", encoding = "UTF-8")
sink(results)
# If the chapter's code stops with an error, the Console still returns to normal.
tryCatch(
  source("code/01_chapter.R", echo = TRUE, print.eval = TRUE, keep.source = TRUE,
         max.deparse.length = Inf, encoding = "UTF-8"),
  finally = { sink(); close(results); invisible(dev.off()) }
)
writeLines(capture.output(sessionInfo()), "output/session_info.txt")
# Files that the chapter's code saves are moved to the output folder.
saved <- list.files(pattern = "\\.(xlsx|png|csv)$")
file.rename(saved, file.path("output", saved))
cat("Done. See the output folder.\n")
