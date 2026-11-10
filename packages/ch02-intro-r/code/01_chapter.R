# Chapter 2: Introduction to R
# The code of the chapter, in the order of the book.
# Lines that begin with ## were not run in the book, usually because they
# need the internet or a package that is not always installed.

# ---- getwd ----
# getwd()


# ---- setwd-general ----
# setwd("path_to_the_folder")


# ---- setwd-example ----
# setwd("C:/Users/Ime/Documents/MyData")


# ---- rproj-relative ----
# population <- read.csv("data/population.csv")


# ---- install-package ----
# install.packages("ggplot2")


# ---- load-package ----
library(ggplot2)


# ---- ops-number ----
3


# ---- ops-add ----
3 + 4
3 - 5


# ---- ops-multiply ----
2 * 6
5 / 3


# ---- assign-x ----
x <- 4


# ---- print-x ----
x


# ---- assign-y ----
y <- 5


# ---- assign-z ----
z <- x + y
z


# ---- census ----
pop_2002 <- 2022547
pop_2021 <- 1836713

change <- pop_2021 - pop_2002
change

percent_change <- change / pop_2002 * 100
percent_change


# ---- census-annual ----
annual_rate <- ((pop_2021 / pop_2002)^(1 / 19) - 1) * 100
annual_rate


# ---- fn-mean ----
mean(c(3, 5, 7))


# ---- fn-others ----
sum(c(3, 5, 7))
sqrt(16)
log(10)
exp(1)


# ---- fn-na ----
mean(c(3, 5, NA))
mean(c(3, 5, NA), na.rm = TRUE)


# ---- vector ----
inflation <- c(2.1, 3.4, 14.2, 9.4)
inflation / 100
mean(inflation)


# ---- data-frame ----
balkans <- data.frame(
  country = c("Albania", "Montenegro", "North Macedonia", "Serbia"),
  median_income = c(6174, 9167, 7609, 7752)
)
balkans
balkans$median_income
mean(balkans$median_income)

