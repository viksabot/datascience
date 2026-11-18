# Chapter 5: Descriptive Analysis
# The code of the chapter, in the order of the book.
# Lines that begin with ## were not run in the book, usually because they
# need the internet or a package that is not always installed.

# ---- read-population ----
library(dplyr)
library(ggplot2)

population <- read.csv("data/wb_cleaned_data.csv")


# ---- population-millions ----
population$Pop_millions <- population$Pop / 1e6
x <- population$Pop_millions


# ---- sturges ----
num_bins <- nclass.Sturges(x)
num_bins


# ---- cut-bins ----
bins <- cut(x, breaks = num_bins, include.lowest = TRUE, right = FALSE)


# ---- frequency-table ----
frequency_table <- population %>%
  mutate(bin = cut(Pop_millions, breaks = num_bins,
                   include.lowest = TRUE, right = FALSE)) %>%
  count(bin) %>%
  mutate(relative_freq = round(n / sum(n), 3),
         percentage = relative_freq * 100)

frequency_table


# ---- stat-mean ----
mean_x <- mean(x, na.rm = TRUE)
mean_x


# ---- stat-median ----
median_x <- median(x, na.rm = TRUE)
median_x


# ---- stat-quantile ----
q75 <- quantile(x, probs = 0.75, na.rm = TRUE)
q75


# ---- stat-minmax ----
min_x <- min(x, na.rm = TRUE)
max_x <- max(x, na.rm = TRUE)
min_x
max_x


# ---- mode-pitfall ----
get_mode <- function(v) {
  v_no_na <- v[!is.na(v)]                     # remove missing values
  freq <- table(v_no_na)                      # table of frequencies
  mode_value <- names(freq)[which.max(freq)]  # most frequent value
  as.numeric(mode_value)                      # convert to a number
}
get_mode(x)


# ---- fig-income-ratio ----
names_lookup <- c(
  AL = "Albania", AT = "Austria", BE = "Belgium", BG = "Bulgaria",
  CH = "Switzerland", CY = "Cyprus", CZ = "Czechia", DE = "Germany",
  DK = "Denmark", EE = "Estonia", EL = "Greece", ES = "Spain",
  FI = "Finland", FR = "France", HR = "Croatia", HU = "Hungary",
  IE = "Ireland", IT = "Italy", LT = "Lithuania", LU = "Luxembourg",
  LV = "Latvia", ME = "Montenegro", MK = "North Macedonia", MT = "Malta",
  NL = "Netherlands", NO = "Norway", PL = "Poland", PT = "Portugal",
  RO = "Romania", RS = "Serbia", SE = "Sweden", SI = "Slovenia",
  SK = "Slovakia", TR = "Türkiye"
)
income <- read.csv("data/income_europe_2023.csv") %>%
  mutate(ratio = mean_income / median_income,
         country = names_lookup[geo],
         balkan = geo %in% c("MK", "AL", "RS", "ME"))
ggplot(income, aes(x = ratio, y = reorder(country, ratio), color = balkan)) +
  geom_segment(aes(x = 1, xend = ratio, yend = reorder(country, ratio)),
               linewidth = 0.5) +
  geom_point(size = 1.9) +
  scale_color_manual(values = c(`FALSE` = book_gray, `TRUE` = book_orange),
                     guide = "none") +
  scale_x_continuous(limits = c(1, 1.5), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Mean income divided by median income", y = NULL) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#E6E9EE"))


# ---- disp-range ----
range_x <- max_x - min_x
range_x


# ---- disp-iqr ----
iqr_x <- IQR(x, na.rm = TRUE)
iqr_x


# ---- disp-sd ----
sd_x <- sd(x, na.rm = TRUE)
sd_x


# ---- disp-var ----
var_x <- var(x, na.rm = TRUE)
var_x


# ---- disp-cv ----
cv_x <- sd_x / mean_x
cv_x


# ---- fig-normal ----
z <- seq(-4, 4, length.out = 401)
curve <- data.frame(z = z, y = dnorm(z))
p <- ggplot(curve, aes(z, y))
for (k in 3:1) {
  p <- p + geom_ribbon(data = subset(curve, abs(z) <= k),
                       aes(ymin = 0, ymax = y), fill = book_blue, alpha = 0.16)
}
brackets <- data.frame(k = 1:3, y = c(0.43, 0.48, 0.53),
                       lab = c("68%", "95%", "99.7%"))
p + geom_line(color = book_blue, linewidth = 0.6) +
  geom_segment(data = brackets, aes(x = -k, xend = k, y = y, yend = y),
               inherit.aes = FALSE, color = "gray35", linewidth = 0.35) +
  geom_text(data = brackets, aes(x = k + 0.15, y = y, label = lab),
            inherit.aes = FALSE, hjust = 0, size = 2.6, color = "gray20") +
  annotate("text", x = -3.95, y = 0.26, hjust = 0, size = 2.8, color = "gray15", parse = TRUE,
           label = "f(x) == frac(1, sigma * sqrt(2 * pi)) * e^{-frac((x - mu)^2, 2 * sigma^2)}") +
  scale_x_continuous(breaks = -3:3, limits = c(-4, 4.2)) +
  scale_y_continuous(limits = c(0, 0.56), expand = c(0, 0)) +
  labs(x = "Number of standard deviations from the mean", y = NULL) +
  theme(axis.text.y = element_blank(), panel.grid = element_blank())


# ---- fig-skew ----
grid <- seq(0, 1, length.out = 400)
shapes <- bind_rows(
  data.frame(x = grid, y = dbeta(grid, 6, 2),
             shape = "Left-skewed (negative)"),
  data.frame(x = grid, y = dbeta(grid, 4, 4), shape = "Symmetric (zero)"),
  data.frame(x = grid, y = dbeta(grid, 2, 6),
             shape = "Right-skewed (positive)")
)
shapes$shape <- factor(shapes$shape, levels = unique(shapes$shape))
ggplot(shapes, aes(x, y)) +
  geom_ribbon(aes(ymin = 0, ymax = y), fill = book_blue, alpha = 0.18) +
  geom_line(color = book_blue, linewidth = 0.6) +
  facet_wrap(~shape) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(), panel.grid = element_blank())


# ---- fig-kurt ----
z <- seq(-4.5, 4.5, length.out = 500)
tails <- bind_rows(
  data.frame(z = z, y = ifelse(abs(z) < 2.2,
                               dbeta((z + 2.2) / 4.4, 1.8, 1.8) / 4.4, 0),
             shape = "Lighter tails (below 3)"),
  data.frame(z = z, y = dnorm(z), shape = "Normal (equal to 3)"),
  data.frame(z = z, y = sqrt(2) / 2 * exp(-sqrt(2) * abs(z)),
             shape = "Heavier tails (above 3)")
)
tails$shape <- factor(tails$shape, levels = unique(tails$shape))
ggplot(tails, aes(z, y)) +
  geom_ribbon(aes(ymin = 0, ymax = y), fill = book_blue, alpha = 0.18) +
  geom_line(color = book_blue, linewidth = 0.6) +
  facet_wrap(~shape) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(), panel.grid = element_blank())


# ---- skew-kurt ----
# install.packages("moments")   # only once on each computer
library(moments)

skew_x <- skewness(x, na.rm = TRUE)
kurtosis_x <- kurtosis(x, na.rm = TRUE)
skew_x
kurtosis_x


# ---- fig-nasdaq ----
nasdaq <- read.csv("data/nasdaq_daily.csv")
nasdaq$ret <- 100 * (nasdaq$close / lag(nasdaq$close) - 1)
returns <- nasdaq$ret[!is.na(nasdaq$ret)]
ggplot(data.frame(ret = returns[abs(returns) <= 6]), aes(ret)) +
  geom_histogram(aes(y = after_stat(count) / length(returns) / 0.2),
                 binwidth = 0.2, fill = book_blue, color = "white",
                 linewidth = 0.1) +
  stat_function(fun = dnorm, args = list(mean = mean(returns), sd = sd(returns)),
                color = book_orange, linewidth = 0.7, xlim = c(-6, 6)) +
  annotate("text", x = 2.4, y = 0.28, label = "Normal curve", hjust = 0,
           color = book_orange, size = 3.2) +
  labs(x = "Daily return (percent)", y = "Density")


# ---- stats-df ----
stats_df <- data.frame(
  Statistic = c("Mean", "Median", "Min", "Max", "SD",
                "Variance", "Skewness", "Kurtosis"),
  Value = c(mean_x, median_x, min_x, max_x, sd_x,
            var_x, skew_x, kurtosis_x)
)
stats_df

# install.packages("openxlsx")  # only once on each computer
library(openxlsx)
write.xlsx(stats_df, file = "descriptive_stats.xlsx", rowNames = FALSE)


# ---- tbl-summary ----
library(knitr)   # for kable()

describe <- function(v) {
  v <- v[!is.na(v)]
  c(N = length(v), Mean = mean(v), Median = median(v), SD = sd(v),
    Min = min(v), Max = max(v),
    Skewness = skewness(v), Kurtosis = kurtosis(v))
}

income <- read.csv("data/income_europe_2023.csv")
growth <- read.csv("data/gdp_growth_europe_2015.csv")
nasdaq <- read.csv("data/nasdaq_daily.csv")
nasdaq$return <- 100 * (nasdaq$close / lag(nasdaq$close) - 1)

summary_table <- rbind(
  "Population (m)"    = describe(x),
  "Median income"     = describe(income$median_income / 1000),
  "GDP growth (%)"    = describe(growth$gdp_growth),
  "NASDAQ return (%)" = describe(nasdaq$return)
)
kable(round(summary_table, 2))


# ---- fig-pop-hist ----
edges <- seq(min(x), max(x), length.out = num_bins + 1)

ggplot(population, aes(x = Pop_millions)) +
  geom_histogram(breaks = edges, fill = "#1F66A8", color = "white") +
  labs(x = "Population (millions)", y = "Number of countries")


# ---- fig-box-anatomy ----
ggplot() +
  annotate("segment", x = c(8, 60), xend = c(30, 98), y = 0, yend = 0,
           color = "#3A4654", linewidth = 0.5) +
  annotate("segment", x = c(8, 98), xend = c(8, 98), y = -0.09, yend = 0.09,
           color = "#3A4654", linewidth = 0.5) +
  annotate("rect", xmin = 30, xmax = 60, ymin = -0.18, ymax = 0.18,
           fill = "#BBD3EA", color = "#3A4654", linewidth = 0.4) +
  annotate("segment", x = 42, xend = 42, y = -0.18, yend = 0.18,
           color = "#3A4654", linewidth = 1) +
  annotate("point", x = c(122, 136), y = 0, color = book_orange, size = 2) +
  annotate("text", x = c(30, 42, 60), y = 0.4, size = 2.8,
           label = c("Q1\n(25th percentile)", "Median",
                     "Q3\n(75th percentile)"),
           hjust = c(1, 0.5, 0), lineheight = 0.9) +
  annotate("text", x = c(8, 98), y = -0.36, size = 2.8,
           label = c("Adjacent value", "Adjacent value")) +
  annotate("text", x = 129, y = 0.28, size = 2.8, label = "Extreme values") +
  annotate("segment", x = 30, xend = 60, y = -0.3, yend = -0.3,
           arrow = arrow(ends = "both", length = unit(0.06, "in")),
           color = "#5A6675", linewidth = 0.3) +
  annotate("text", x = 45, y = -0.42, size = 2.8, label = "IQR") +
  scale_y_continuous(limits = c(-0.55, 0.62)) +
  scale_x_continuous(limits = c(-8, 145)) +
  theme_void()


# ---- fig-pop-box ----
ggplot(population, aes(x = Pop_millions)) +
  geom_boxplot(fill = "#BBD3EA", outlier.color = "#C2571A",
               outlier.shape = 16, width = 0.5) +
  labs(x = "Population (millions)", y = NULL) +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())


# ---- fig-growth-box ----
ggplot(growth, aes(x = gdp_growth)) +
  geom_boxplot(fill = "#BBD3EA", width = 0.5,
               outlier.color = "#C2571A", outlier.size = 2) +
  annotate("text", x = c(-9.8, 9.6, 24.6), y = 0.14,
           label = c("Ukraine", "Malta", "Ireland"), size = 3) +
  labs(x = "Real GDP growth (percent)", y = NULL) +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())

