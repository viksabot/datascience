# Chapter 8: Unsupervised Learning
# The code of the chapter, in the order of the book.
# Lines that begin with ## were not run in the book, usually because they
# need the internet or a package that is not always installed.

# ---- fig-u-cfnai ----
cf <- read.csv("data/cfnai.csv")
cf$date <- as.Date(cf$date)
run <- rle(cf$USREC)
ends <- cumsum(run$lengths)
starts <- ends - run$lengths + 1
rec <- data.frame(start = cf$date[starts[run$values == 1]],
                  end = cf$date[ends[run$values == 1]])
ggplot(cf, aes(date, CFNAIMA3)) +
  geom_rect(data = rec, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#E3E6EB") +
  geom_hline(yintercept = -0.7, color = book_orange, linewidth = 0.4) +
  geom_line(color = book_blue, linewidth = 0.4) +
  annotate("text", x = as.Date("1969-01-01"), y = -1.3, label = "-0.70",
           color = book_orange, size = 2.8, hjust = 0) +
  coord_cartesian(ylim = c(-6, 2)) +
  labs(x = NULL, y = "CFNAI, 3-month average")


# ---- fig-u-pca-idea ----
set.seed(11)
z1 <- rnorm(120)
pts <- data.frame(a = as.numeric(scale(z1 + rnorm(120, 0, 0.55))),
                  b = as.numeric(scale(z1 + rnorm(120, 0, 0.55))))
ggplot(pts, aes(a, b)) +
  geom_point(color = book_gray, size = 1.2) +
  annotate("segment", x = -2.2, xend = 2.2, y = -2.2, yend = 2.2,
           color = book_blue, linewidth = 0.8,
           arrow = arrow(length = unit(0.08, "in"))) +
  annotate("segment", x = 0.75, xend = -0.75, y = -0.75, yend = 0.75,
           color = book_orange, linewidth = 0.8,
           arrow = arrow(length = unit(0.08, "in"))) +
  annotate("text", x = 2.3, y = 1.85, label = "PC1", color = book_blue,
           size = 3.2, hjust = 0) +
  annotate("text", x = -1.2, y = 0.95, label = "PC2", color = book_orange,
           size = 3.2, hjust = 0) +
  coord_fixed(xlim = c(-3.2, 3.2), ylim = c(-2.8, 2.8)) +
  labs(x = "Variable A (standardised)", y = "Variable B (standardised)")


# ---- u-download ----
# library(WDI)
# library(dplyr)
# library(countrycode)
# library(ggplot2)
# 
# indicators <- c(
#   "LifeExp"    = "SP.DYN.LE00.IN",  # Life expectancy at birth
#   "GDPperCap"  = "NY.GDP.PCAP.CD",  # GDP per person (USD)
#   "SecEnroll"  = "SE.SEC.ENRR"      # Secondary enrolment (%)
# )
# 
# df_raw <- WDI(country = "all", indicator = indicators,
#               start = 2022, end = 2022)
# 
# df_clean <- df_raw %>%
#   mutate(iso3c = countrycode(iso2c, origin = "iso2c",
#                              destination = "iso3c")) %>%
#   filter(!is.na(iso3c)) %>%
#   filter(
#     !country %in% c("World", "High income", "Low income", "Euro area",
#                     "European Union", "IDA & IBRD total",
#                     "OECD members"),
#     !is.na(LifeExp), !is.na(GDPperCap), !is.na(SecEnroll),
#     LifeExp > 0, GDPperCap > 0, SecEnroll > 0
#   )


# ---- u-load ----
df_clean <- read.csv("data/wdi_hdi_clean_2022.csv")


# ---- u-scale ----
df_scaled <- df_clean %>%
  select(LifeExp, GDPperCap, SecEnroll) %>%
  scale()


# ---- u-pca ----
pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)


# ---- u-rotation ----
pca_result$rotation


# ---- u-weights ----
weights_pc1 <- abs(pca_result$rotation[, 1])
normalized_weights <- weights_pc1 / sum(weights_pc1)
round(normalized_weights, 2)


# ---- u-index ----
# Calculate the index: a weighted sum of the standardised indicators
df_clean$HDI_PCA_weighted <- as.numeric(df_scaled %*%
                                          normalized_weights)


# ---- u-minmax ----
df_minmax <- df_clean %>%
  mutate(
    LifeExp_std   = (LifeExp - min(LifeExp)) /
      (max(LifeExp) - min(LifeExp)),
    GDPperCap_std = (GDPperCap - min(GDPperCap)) /
      (max(GDPperCap) - min(GDPperCap)),
    SecEnroll_std = (SecEnroll - min(SecEnroll)) /
      (max(SecEnroll) - min(SecEnroll)),
    HDI_equal = (LifeExp_std + GDPperCap_std + SecEnroll_std) / 3,
    HDI_PCA = normalized_weights["LifeExp"] * LifeExp_std +
      normalized_weights["GDPperCap"] * GDPperCap_std +
      normalized_weights["SecEnroll"] * SecEnroll_std
  )


# ---- fig-u-compare ----
library(ggrepel)

ggplot(df_minmax, aes(x = HDI_equal, y = HDI_PCA, label = iso3c)) +
  geom_abline(slope = 1, intercept = 0, color = "#AEB6C1") +
  geom_point(color = "#2F8F6B", size = 1.5) +
  geom_text_repel(size = 2, max.overlaps = 12) +
  labs(x = "HDI with equal weights", y = "HDI with PCA weights")


# ---- u-redundant ----
# Experiment 1: add a second measure of income
s2 <- df_clean %>%
  transmute(LifeExp, GDPperCap, SecEnroll,
            LogGDP = log(GDPperCap)) %>%
  scale()
w2 <- abs(prcomp(s2)$rotation[, 1])
round(w2 / sum(w2), 2)

# Experiment 2: add an indicator of pure random noise
set.seed(7)
noise <- as.numeric(scale(rnorm(nrow(df_scaled))))
s3 <- cbind(df_scaled, Noise = noise)
w3 <- abs(prcomp(s3)$rotation[, 1])
round(w3 / sum(w3), 2)


# ---- u-scores ----
head(round(pca_result$x, 3), 4)


# ---- fig-u-pc12 ----
df_minmax$PC1 <- pca_result$x[, 1]
df_minmax$PC2 <- pca_result$x[, 2]

plot_components <- ggplot(df_minmax, aes(x = PC1, y = PC2,
                                         label = iso3c)) +
  geom_point(color = "#1F66A8", size = 1.5) +
  geom_text_repel(size = 2, max.overlaps = 14) +
  labs(x = "First principal component",
       y = "Second principal component")
plot_components

ggsave("HDI_PC1_PC2.png", plot = plot_components,
       width = 8, height = 6, dpi = 300)


# ---- u-yields ----
yields <- read.csv("data/us_treasury_yields.csv")
changes <- apply(yields[, -1], 2, diff)   # monthly changes
pca_yields <- prcomp(changes, center = TRUE, scale. = TRUE)
round(summary(pca_yields)$importance[, 1:4], 3)


# ---- fig-u-yields ----
ld <- as.data.frame(pca_yields$rotation[, 1:3])
ld$maturity <- c(1, 2, 3, 5, 7, 10)
ld <- tidyr::pivot_longer(ld, -maturity, names_to = "pc", values_to = "loading")
ld$pc <- factor(ld$pc, labels = c("PC1: level", "PC2: slope", "PC3: curvature"))
ends <- ld[ld$maturity == 10, ]
ends$label <- as.character(ends$pc)
ggplot(ld, aes(maturity, loading, color = pc)) +
  geom_hline(yintercept = 0, color = "#AEB6C1", linewidth = 0.3) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  geom_text(data = ends, aes(x = 10.2, label = label), hjust = 0, size = 2.8) +
  scale_color_manual(values = c(book_blue, book_orange, "#2F8F6B"), guide = "none") +
  scale_x_continuous(breaks = c(1, 2, 3, 5, 7, 10), limits = c(1, 13.2)) +
  labs(x = "Maturity (years)", y = "Loading")


# ---- u-kmeans ----
set.seed(123)
kmeans_result <- kmeans(df_scaled, centers = 3, nstart = 25)


# ---- u-cluster-summary ----
df_clean$Cluster <- as.factor(kmeans_result$cluster)
df_minmax$Cluster <- as.factor(kmeans_result$cluster)

cluster_counts <- df_clean %>%
  count(Cluster, name = "Countries")
cluster_counts

cluster_summary <- df_clean %>%
  group_by(Cluster) %>%
  summarise(
    Mean_LifeExp = mean(LifeExp, na.rm = TRUE),
    Mean_GDPperCap = mean(GDPperCap, na.rm = TRUE),
    Mean_SecEnroll = mean(SecEnroll, na.rm = TRUE)
  )
cluster_summary


# ---- fig-u-kmeans ----
kmeans_plot <- ggplot(df_minmax, aes(x = PC1, y = PC2,
                                     color = Cluster, label = iso3c)) +
  geom_point(size = 1.5) +
  geom_text_repel(size = 2, max.overlaps = 14, show.legend = FALSE) +
  scale_color_manual(values = c("#C2571A", "#1F66A8", "#2F8F6B")) +
  labs(x = "First principal component",
       y = "Second principal component", color = "Cluster")
kmeans_plot


# ---- u-kmeans-log ----
df_log_scaled <- df_clean %>%
  transmute(LifeExp, LogGDP = log(GDPperCap), SecEnroll) %>%
  scale()

set.seed(123)
kmeans_log <- kmeans(df_log_scaled, centers = 3, nstart = 25)
table(original = kmeans_result$cluster, log_income = kmeans_log$cluster)


# ---- fig-u-elbow ----
wss <- numeric(10)
set.seed(123)
for (k in 1:10) {
  kmeans_model <- kmeans(df_scaled, centers = k, nstart = 20)
  wss[k] <- kmeans_model$tot.withinss
}

elbow_df <- data.frame(K = 1:10, WSS = wss)
ggplot(elbow_df, aes(x = K, y = WSS)) +
  geom_line(color = "#1F66A8") +
  geom_point(size = 2, color = "#1F66A8") +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Number of clusters (K)",
       y = "Total within-cluster variation (WSS)")


# ---- fig-u-noise ----
set.seed(2026)
R <- scale(cbind(runif(136), runif(136), runif(136)))
rw <- numeric(10)
for (k in 1:10) rw[k] <- kmeans(R, centers = k, nstart = 20)$tot.withinss
nd <- rbind(data.frame(K = 1:10, share = wss / wss[1], src = "136 countries"),
            data.frame(K = 1:10, share = rw / rw[1], src = "Random numbers"))
lab <- nd[nd$K == 10, ]
ggplot(nd, aes(K, share, color = src)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.6) +
  geom_text(data = lab, aes(x = 10.2, label = src), hjust = 0, size = 2.8) +
  scale_color_manual(values = c(book_blue, book_orange), guide = "none") +
  scale_x_continuous(breaks = 1:10, limits = c(1, 12.6)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Number of clusters (K)", y = "Share of total variation left")


# ---- u-regions ----
regions <- read.csv("data/eurostat_regions_2022.csv")
region_vars <- c("gdp_pps", "employment", "tertiary", "unemployment")
regions_scaled <- scale(regions[, region_vars])

set.seed(123)
km_regions <- kmeans(regions_scaled, centers = 4, nstart = 25)
regions$cluster <- km_regions$cluster
nrow(regions)


# ---- tbl-u-regions ----
tab <- regions %>%
  group_by(cluster) %>%
  summarise(Regions = n(), `GDP per head (PPS)` = round(mean(gdp_pps), -2),
            `Employment (%)` = round(mean(employment), 1),
            `Tertiary (%)` = round(mean(tertiary), 1),
            `Unempl. (%)` = round(mean(unemployment), 1), .groups = "drop") %>%
  arrange(`GDP per head (PPS)`)
tab <- cbind(Group = c("Low employment", "High employment, few graduates",
                       "Many graduates", "Rich, mostly metropolitan"),
             tab[, -1])
kable(tab, row.names = FALSE)


# ---- u-machinery ----
library(readxl)

df <- read_excel("data/usa_imports_machinery_2023.xlsx")
exporters <- df$exporter

df_log <- df %>%
  select(-exporter) %>%
  mutate(across(everything(), ~ log1p(.)))

pca_machines <- prcomp(df_log, scale. = TRUE)
pca_scores <- as.data.frame(pca_machines$x)
pca_scores$exporter <- exporters

set.seed(123)
kmeans_machines <- kmeans(df_log, centers = 4, nstart = 25)
pca_scores$cluster <- factor(kmeans_machines$cluster)

# Which cluster does North Macedonia belong to?
mkd_cluster <- pca_scores %>%
  filter(exporter == "North Macedonia") %>%
  pull(cluster)

same_cluster <- pca_scores %>%
  filter(cluster == mkd_cluster) %>%
  arrange(exporter)
nrow(same_cluster)


# ---- fig-u-machinery ----
ggplot(pca_scores, aes(x = PC1, y = PC2,
                       color = cluster, label = exporter)) +
  geom_point(size = 1.4) +
  geom_text_repel(size = 2, max.overlaps = 9, show.legend = FALSE) +
  scale_color_manual(values = c("#1F66A8", "#C2571A", "#2F8F6B",
                                "#7B4FA3")) +
  labs(x = "Principal component 1", y = "Principal component 2",
       color = "Cluster")


# ---- u-pc1-meaning ----
n_products <- rowSums(select(df, -exporter) > 0)
cor(pca_machines$x[, 1], n_products)


# ---- fig-u-treemap ----
library(tidyr)
library(treemapify)

selected <- c("North Macedonia", "Latvia", "Bosnia and Herzegovina")

df_long <- df %>%
  filter(exporter %in% selected) %>%
  pivot_longer(-exporter, names_to = "Product",
               values_to = "Exports") %>%
  filter(Exports > 0) %>%
  mutate(Product = sub(" *\\(.*", "", Product))

ggplot(df_long, aes(area = Exports, label = Product)) +
  geom_treemap(fill = "#1F66A8", color = "white") +
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE,
                    min.size = 3.5, size = 7) +
  facet_wrap(~ exporter, nrow = 1)

