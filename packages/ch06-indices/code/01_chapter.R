# Chapter 6: Composite Indices
# The code of the chapter, in the order of the book.
# Lines that begin with ## were not run in the book, usually because they
# need the internet or a package that is not always installed.

# ---- i-download ----
# # 0) Load the required packages
# # install.packages("WDI")
# # install.packages("dplyr")
# # install.packages("countrycode")
# library(WDI)
# library(dplyr)
# library(countrycode)
# 
# # 1) Define indicators similar to those of the HDI
# #    - Life expectancy (SP.DYN.LE00.IN)
# #    - GDP per person (NY.GDP.PCAP.CD)
# #    - Secondary school enrolment (SE.SEC.ENRR)
# indicators <- c(
#   "LifeExp"    = "SP.DYN.LE00.IN",  # Life expectancy at birth
#   "GDPperCap"  = "NY.GDP.PCAP.CD",  # GDP per person (current US$)
#   "SecEnroll"  = "SE.SEC.ENRR"      # Secondary enrolment, % (gross)
# )
# 
# # 2) Download data for all countries for 2022
# df_raw <- WDI(
#   country   = "all",
#   indicator = indicators,
#   start     = 2022,
#   end       = 2022
# )
# 
# # 3) For easier processing we use the ISO3 country codes
# df_raw <- df_raw %>%
#   mutate(iso3c = countrycode(iso2c, origin = "iso2c",
#                              destination = "iso3c"))
# 
# # 4) Filter out aggregates, regions and the like
# #    We keep only the data for countries:
# df_clean <- df_raw %>%
#   filter(!is.na(iso3c)) %>%
#   # Remove "countries" such as "World" and "High income"
#   filter(
#     !country %in% c("World", "High income", "Low income", "Euro area",
#                     "European Union", "IDA & IBRD total",
#                     "OECD members"),
#     !is.na(LifeExp),
#     !is.na(GDPperCap),
#     !is.na(SecEnroll),
#     LifeExp > 0,
#     GDPperCap > 0,
#     SecEnroll > 0
#   )


# ---- i-load ----
df_clean <- read.csv("data/wdi_hdi_clean_2022.csv")


# ---- i-scatter-code ----
# Data
df_analysis <- df_clean %>%
  select(LifeExp, GDPperCap, SecEnroll) %>%
  filter(!is.na(LifeExp), !is.na(GDPperCap), !is.na(SecEnroll))

# Function for drawing scatter plots
scatter_plot <- function(x, y, x_label, y_label) {
  ggplot(df_analysis, aes(x = !!sym(x), y = !!sym(y))) +
    geom_point(color = "#1F66A8", alpha = 0.6) +
    labs(x = x_label, y = y_label) +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, face = "bold"))
}

# Create all three scatter plots
plot1 <- scatter_plot("GDPperCap", "LifeExp",
                      "GDP per person (USD)",
                      "Life expectancy at birth (years)")
plot2 <- scatter_plot("SecEnroll", "LifeExp",
                      "Secondary enrolment (%)",
                      "Life expectancy at birth (years)")
plot3 <- scatter_plot("GDPperCap", "SecEnroll",
                      "GDP per person (USD)",
                      "Secondary enrolment (%)")


# ---- i-grid-code ----
# # install.packages("gridExtra")
# library(gridExtra)
# # Save as a high-quality PNG image
# png("scatter_plots.png", width = 3000, height = 1500, res = 300)
# grid.arrange(plot1, plot2, plot3, ncol = 3)
# dev.off()


# ---- fig-i-scatters ----
small <- theme(axis.title = element_text(size = 6.3),
               axis.text = element_text(size = 5.8),
               plot.margin = margin(2, 5, 2, 2))
k_axis <- scale_x_continuous(breaks = c(0, 50000, 100000),
                             labels = c("0", "50,000", "100,000"))
library(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 3)))
print(plot1 + k_axis + small, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2 + small, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(plot3 + k_axis + small, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))


# ---- fig-i-strong ----
set.seed(6)
a <- runif(70)
sim <- rbind(
  data.frame(a = a, b = a + rnorm(70, 0, 0.05), kind = "Strong positive"),
  data.frame(a = a, b = 1 - a + rnorm(70, 0, 0.05), kind = "Strong negative")
)
sim$kind <- factor(sim$kind, levels = c("Strong positive", "Strong negative"))
ggplot(sim, aes(a, b)) +
  geom_point(color = book_blue, alpha = 0.6, size = 1) +
  facet_wrap(~kind) +
  labs(x = "Indicator A", y = "Indicator B") +
  theme(axis.text = element_blank())


# ---- i-cor ----
# Calculate the correlations
cor1 <- cor(df_analysis$GDPperCap, df_analysis$LifeExp,
            method = "pearson", use = "complete.obs")
cor2 <- cor(df_analysis$SecEnroll, df_analysis$LifeExp,
            method = "pearson", use = "complete.obs")
cor3 <- cor(df_analysis$GDPperCap, df_analysis$SecEnroll,
            method = "pearson", use = "complete.obs")

# Results
cat("GDP per person and life expectancy:", round(cor1, 3), "\n")
cat("Enrolment and life expectancy:", round(cor2, 3), "\n")
cat("GDP per person and enrolment:", round(cor3, 3), "\n")


# ---- i-cor-log ----
cor(log(df_analysis$GDPperCap), df_analysis$LifeExp)


# ---- i-rank ----
df_clean <- df_clean %>%
  mutate(
    LifeExp_rank = rank(LifeExp),
    GDPperCap_rank = rank(GDPperCap),
    SecEnroll_rank = rank(SecEnroll)
  )


# ---- i-zscore ----
df_clean <- df_clean %>%
  mutate(
    LifeExp_z = (LifeExp - mean(LifeExp, na.rm = TRUE)) /
      sd(LifeExp, na.rm = TRUE),
    GDPperCap_z = (GDPperCap - mean(GDPperCap, na.rm = TRUE)) /
      sd(GDPperCap, na.rm = TRUE),
    SecEnroll_z = (SecEnroll - mean(SecEnroll, na.rm = TRUE)) /
      sd(SecEnroll, na.rm = TRUE)
  )


# ---- i-minmax ----
df_clean <- df_clean %>%
  mutate(
    LifeExp_minmax = (LifeExp - min(LifeExp, na.rm = TRUE)) /
      (max(LifeExp, na.rm = TRUE) - min(LifeExp, na.rm = TRUE)),
    GDPperCap_minmax = (GDPperCap - min(GDPperCap, na.rm = TRUE)) /
      (max(GDPperCap, na.rm = TRUE) - min(GDPperCap, na.rm = TRUE)),
    SecEnroll_minmax = (SecEnroll - min(SecEnroll, na.rm = TRUE)) /
      (max(SecEnroll, na.rm = TRUE) - min(SecEnroll, na.rm = TRUE))
  )


# ---- i-ref ----
df_clean <- df_clean %>%
  mutate(
    LifeExp_ref = LifeExp / max(LifeExp, na.rm = TRUE),
    GDPperCap_ref = GDPperCap / max(GDPperCap, na.rm = TRUE),
    SecEnroll_ref = SecEnroll / max(SecEnroll, na.rm = TRUE)
  )


# ---- tbl-i-norm ----
tab <- df_clean %>%
  filter(country %in% c("Luxembourg", "Slovenia", "North Macedonia", "Malawi")) %>%
  arrange(desc(GDPperCap)) %>%
  transmute(Country = country, `GDP per person (US$)` = round(GDPperCap),
            Rank = GDPperCap_rank, `z-score` = round(GDPperCap_z, 2),
            `Min-max` = round(GDPperCap_minmax, 3),
            `Share of leader` = round(GDPperCap_ref, 3))
kable(tab, row.names = FALSE)


# ---- i-linear ----
df_clean <- df_clean %>%
  mutate(
    HDI_linear = 1/3 * LifeExp_minmax + 1/3 * GDPperCap_minmax +
      1/3 * SecEnroll_minmax
  )


# ---- i-geometric ----
df_clean <- df_clean %>%
  mutate(
    HDI_geometric = (LifeExp_minmax ^ (1/3)) *
      (GDPperCap_minmax ^ (1/3)) * (SecEnroll_minmax ^ (1/3))
  )


# ---- i-zeros ----
df_clean %>%
  filter(HDI_geometric == 0) %>%
  select(country, LifeExp_minmax, GDPperCap_minmax, SecEnroll_minmax)


# ---- i-undp ----
undp <- read.csv("data/undp_hdi_2023.csv") %>%
  filter(!is.na(hdi_rank_2023), !is.na(hdi_2023))

undp <- undp %>%
  mutate(
    health = pmin((le_2023 - 20) / (85 - 20), 1),
    educ = (pmin(eys_2023 / 18, 1) + pmin(mys_2023 / 15, 1)) / 2,
    income = pmin((log(gnipc_2023) - log(100)) /
                    (log(75000) - log(100)), 1),
    hdi_calc = (health * educ * income)^(1/3)
  )

max(abs(undp$hdi_calc - undp$hdi_2023))


# ---- i-log-index ----
df_clean <- df_clean %>%
  mutate(
    GDP_log_minmax = (log(GDPperCap) - min(log(GDPperCap))) /
      (max(log(GDPperCap)) - min(log(GDPperCap))),
    HDI_linear_log = (LifeExp_minmax + GDP_log_minmax +
                        SecEnroll_minmax) / 3
  )

compare <- df_clean %>%
  inner_join(select(undp, iso3, hdi_2023),
             by = c("iso3c" = "iso3"))

c(raw_income = cor(compare$HDI_linear, compare$hdi_2023),
  log_income = cor(compare$HDI_linear_log, compare$hdi_2023))


# ---- fig-i-official ----
mk <- compare %>% filter(iso3c == "MKD")
ggplot(compare, aes(HDI_linear_log, hdi_2023)) +
  geom_point(color = book_gray, size = 1.5) +
  geom_point(data = mk, color = book_orange, size = 2.4) +
  annotate("segment", x = mk$HDI_linear_log, y = mk$hdi_2023,
           xend = mk$HDI_linear_log + 0.08, yend = mk$hdi_2023 - 0.13,
           color = "#5A6675", linewidth = 0.3) +
  annotate("text", x = mk$HDI_linear_log + 0.09, y = mk$hdi_2023 - 0.14,
           label = "North Macedonia", hjust = 0, size = 3) +
  labs(x = "HDI-like index, our calculation", y = "Official HDI")


# ---- i-table ----
library(dplyr)
library(openxlsx)

df_table <- df_clean %>%
  select(country, HDI_geometric, GDPperCap) %>%
  arrange(desc(HDI_geometric))

# Save the table to Excel
write.xlsx(df_table, "HDI_Geometric_Table.xlsx")

head(df_table, 5)


# ---- fig-i-bars ----
library(ggplot2)

df_bar <- df_table %>%
  slice(c(1:10, (n() - 9):n()))

ggplot(df_bar, aes(y = reorder(country, HDI_geometric),
                   x = HDI_geometric)) +
  geom_bar(stat = "identity", fill = "#1F66A8") +
  labs(y = NULL, x = "HDI-like index (geometric mean)")


# ---- fig-i-hdi-gdp ----
ggplot(df_table, aes(x = GDPperCap, y = HDI_geometric)) +
  geom_point(color = "#2F8F6B", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", color = "#C2571A", se = FALSE) +
  labs(x = "GDP per person (USD)",
       y = "HDI-like index (geometric mean)")


# ---- i-weights ----
rank_with <- function(w) {
  score <- undp$health^w[1] * undp$educ^w[2] * undp$income^w[3]
  rank(-score, ties.method = "min")
}

undp$r_equal  <- rank_with(c(1/3, 1/3, 1/3))
undp$r_health <- rank_with(c(0.50, 0.25, 0.25))
undp$r_educ   <- rank_with(c(0.25, 0.50, 0.25))
undp$r_income <- rank_with(c(0.25, 0.25, 0.50))

undp$r_linear <- rank(-(undp$health + undp$educ + undp$income),
                      ties.method = "min")


# ---- fig-i-sensitivity ----
keep <- c(MKD = "North Macedonia", ALB = "Albania", SRB = "Serbia",
          MNE = "Montenegro", BIH = "Bosnia and Herz.", BGR = "Bulgaria",
          HRV = "Croatia", GRC = "Greece", SVN = "Slovenia")
s <- undp %>%
  filter(iso3 %in% names(keep)) %>%
  mutate(name = keep[iso3],
         lo = pmin(r_equal, r_health, r_educ, r_income),
         hi = pmax(r_equal, r_health, r_educ, r_income))
ggplot(s, aes(y = reorder(name, -r_equal))) +
  geom_segment(aes(x = lo, xend = hi, yend = reorder(name, -r_equal)),
               color = book_gray, linewidth = 1.6) +
  geom_point(aes(x = r_equal), color = book_blue, size = 2) +
  scale_x_reverse() +
  labs(x = "HDI rank (higher up the ranking to the right)", y = NULL) +
  theme(panel.grid.major.y = element_blank())


# ---- i-labour ----
labour <- read.csv("data/eurostat_labour_2023.csv")
nrow(labour)
round(cor(labour[, c("employment", "unemployment", "neet")]), 2)


# ---- i-labour-index ----
minmax <- function(x) (x - min(x)) / (max(x) - min(x))

labour <- labour %>%
  mutate(
    careless = (minmax(employment) + minmax(unemployment) +
                  minmax(neet)) / 3,
    correct  = (minmax(employment) + (1 - minmax(unemployment)) +
                  (1 - minmax(neet))) / 3,
    rank_careless = rank(-careless),
    rank_correct  = rank(-correct)
  )

labour %>%
  arrange(rank_careless) %>%
  select(geo, careless, rank_careless, rank_correct) %>%
  head(3)


# ---- fig-i-labour ----
geo_names <- c(
  AT = "Austria", BA = "Bosnia and Herzegovina", BE = "Belgium",
  BG = "Bulgaria", CH = "Switzerland", CY = "Cyprus", CZ = "Czechia",
  DE = "Germany", DK = "Denmark", EE = "Estonia", EL = "Greece",
  ES = "Spain", FI = "Finland", FR = "France", HR = "Croatia",
  HU = "Hungary", IE = "Ireland", IS = "Iceland", IT = "Italy",
  LT = "Lithuania", LU = "Luxembourg", LV = "Latvia",
  MK = "North Macedonia", MT = "Malta", NL = "Netherlands", NO = "Norway",
  PL = "Poland", PT = "Portugal", RO = "Romania", RS = "Serbia",
  SE = "Sweden", SI = "Slovenia", SK = "Slovakia", TR = "Türkiye"
)
labour %>%
  mutate(name = geo_names[geo], balkan = geo %in% c("MK", "RS", "BA")) %>%
  ggplot(aes(x = correct, y = reorder(name, correct), color = balkan)) +
  geom_segment(aes(x = 0, xend = correct, yend = reorder(name, correct)),
               linewidth = 0.5) +
  geom_point(size = 1.9) +
  scale_color_manual(values = c(`FALSE` = book_gray, `TRUE` = book_orange),
                     guide = "none") +
  scale_x_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Labour market index (0 = worst, 1 = best)", y = NULL) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#E6E9EE"))

