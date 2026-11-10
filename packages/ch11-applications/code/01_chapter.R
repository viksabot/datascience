# Chapter 11: Applications: Economic Structure and Trade
# The code of the chapter, in the order of the book.
# Lines that begin with ## were not run in the book, usually because they
# need the internet or a package that is not always installed.

# ---- a-read ----
# Load necessary libraries
# install.packages("EconGeo")
library(readxl)
library(tidyverse)

# Read the dataset from the Excel file
data <- as.data.frame(read_excel("data/specialization_example.xlsx"))
# Set the first column as row names
xx <- data[[1]]
# Remove the first column from the data frame
data <- data[, -1]
rownames(data) <- xx
data


# ---- a-load-cached ----
res <- readRDS("data/econgeo_results.rds")
RCA <- res$fict$RCA
M <- res$fict$M
Mc <- res$fict$Mc
Mp <- res$fict$Mp
ECIc <- res$fict$ECIc
HHI <- res$fict$HHI


# ---- a-region ----
region_GDP <- rowSums(data, na.rm = TRUE)
region_GDP


# ---- a-industry ----
industry_GDP <- colSums(data, na.rm = TRUE)
industry_GDP


# ---- a-gdp ----
GDP <- sum(data, na.rm = TRUE)
GDP


# ---- a-w ----
W <- data / region_GDP
round(W, 3)


# ---- a-wp ----
Wp <- industry_GDP / GDP
round(Wp, 3)


# ---- a-rca-code ----
# library(EconGeo)
# RCA <- location_quotient(as.matrix(data), binary = FALSE)
# round(RCA, 2)


# ---- a-rca-out ----
round(RCA, 2)


# ---- a-m-code ----
# M <- location_quotient(as.matrix(data), binary = TRUE)
# M


# ---- a-m-out ----
M


# ---- a-div-code ----
# Mc <- diversity(M)
# Mc


# ---- a-div-out ----
Mc


# ---- a-ubi-code ----
# Mp <- ubiquity(M)
# Mp


# ---- a-ubi-out ----
Mp


# ---- a-eci-code ----
# ECIc <- morc(M)
# ECIc


# ---- a-eci-out ----
ECIc


# ---- a-hhi-code ----
# HHI <- herfindahl(as.matrix(data))
# HHI


# ---- a-hhi-out ----
round(HHI, 4)


# ---- fig-a-hhi ----
hhi_plot <- data.frame(region = rownames(data), HHI = as.vector(HHI),
                       Diversification = as.vector(Mc))

ggplot(hhi_plot, aes(HHI, Diversification, label = region)) +
  geom_point(color = "#1F66A8", size = 2.2) +
  ggrepel::geom_text_repel(size = 3, seed = 1) +
  scale_y_continuous(breaks = 1:4, limits = c(0.5, 4.5))


# ---- a-eurostat ----
emp <- read.csv("data/eurostat_employment_nuts2.csv")
sectors <- c("A", "B.E", "F", "G.I", "J", "K", "L", "M_N", "O.Q", "R.U")
X <- as.matrix(emp[, sectors])
rownames(X) <- emp$name

share_region <- X / rowSums(X)
share_all <- colSums(X) / sum(X)
rca <- t(t(share_region) / share_all)
hhi <- rowSums(share_region^2)

round(rca["Severna Makedonija", ], 2)


# ---- a-eurostat-hhi ----
round(sort(hhi, decreasing = TRUE)[1:5], 3)


# ---- a-muni ----
data <- as.data.frame(read_excel("data/data-for-real-regression.xlsx"))
head(data, 3)


# ---- a-muni-model ----
# Standardise the ECI within each year
data$eci_start_z <- as.vector(scale(data$eci_start))
data$eci_end_z <- as.vector(scale(data$eci_end))
# Transform income_start to its logarithm
data$log_income_start <- log(data$income_start)
# Fit the linear model
model <- lm(log_income_start ~ eci_start_z + hhi_start, data = data)
# Summarize the model to see the coefficients
summary(model)


# ---- a-muni-mse ----
mean((predict(model) - data$log_income_start)^2)


# ---- a-muni-predict ----
# Prepare data for prediction (using eci_end and hhi_end)
data_to_predict <- data.frame(eci_start_z = data$eci_end_z,
                              hhi_start = data$hhi_end)
# Make predictions
predictions_log_income <- predict(model, newdata = data_to_predict)
# Convert log predictions back to the original scale
predictions_income <- exp(predictions_log_income)

log_income_end <- log(data$income_end)
mean((predictions_log_income - log_income_end)^2)


# ---- fig-a-muni ----
latin <- c("ИЛИНДЕН" = "Ilinden", "АРАЧИНОВО" = "Aračinovo",
           "САРАЈ" = "Saraj", "БУТЕЛ" = "Butel", "ЖЕЛИНО" = "Želino",
           "ПЛАСНИЦА" = "Plasnica", "НОВАЦИ" = "Novaci",
           "МАКЕДОНСКИ БРОД" = "Makedonski Brod",
           "ДЕЛЧЕВО" = "Delčevo", "КРАТОВО" = "Kratovo",
           "КРИВА ПАЛАНКА" = "Kriva Palanka", "КРУШЕВО" = "Kruševo")

data$Predicted_income <- predictions_income
data$ratio <- data$income_end / data$Predicted_income

extremes <- data %>%
  arrange(desc(ratio)) %>%
  slice(c(1:6, (n() - 5):n())) %>%
  mutate(Region = factor(latin[municipality],
                         levels = rev(latin[municipality])))

plot_data <- bind_rows(
  data.frame(Region = extremes$Region, Revenue = extremes$income_end,
             Type = "Actual revenue"),
  data.frame(Region = extremes$Region,
             Revenue = extremes$Predicted_income,
             Type = "Predicted revenue")
)

ggplot(plot_data, aes(x = Revenue, y = Region, color = Type)) +
  geom_line(aes(group = Region), color = "#AEB6C1") +
  geom_point(size = 2.2) +
  scale_x_log10(breaks = c(1, 2, 5, 10, 20) * 1e6,
                labels = c(1, 2, 5, 10, 20)) +
  scale_color_manual(values = c("Actual revenue" = "#1F66A8",
                                "Predicted revenue" = "#C2571A")) +
  labs(x = "Revenue per employee (million denars)", y = NULL,
       color = NULL) +
  theme(legend.position = "top")


# ---- a-muni-test ----
gap_2015 <- residuals(model)
growth <- log(data$income_end) - log(data$income_start)
round(coef(summary(lm(growth ~ gap_2015))), 3)
summary(lm(growth ~ gap_2015))$r.squared


# ---- a-export-read ----
options(scipen = 999)
data <- as.data.frame(read_excel("data/export_example.xlsx"))
xx <- data[[1]]
data <- data[, -1]
rownames(data) <- xx
data


# ---- a-export-cached ----
M <- res$exp_ex$M
PCI <- res$exp_ex$PCI
RR <- res$exp_ex$RR
RR_ext <- res$exp_ex$RR_ext


# ---- a-export-m ----
# ## Calculate Specialization matrix
# M <- location_quotient(as.matrix(data), binary = TRUE)
# M


# ---- a-export-m-out ----
M


# ---- a-pci ----
# ## Calculate Product Complexity Index
# PCI <- mort(M)
# PCI


# ---- a-pci-out ----
round(PCI, 2)


# ---- a-prox ----
# # Calculate product co-occurrence matrix
# MtM <- t(M) %*% M
# # Get the diagonal elements (which are M_ii)
# diagonal <- diag(MtM)
# # Create a matrix of maximum values between M_ii and M_jj
# max_matrix <- outer(diagonal, diagonal, pmax)
# # Divide the product matrix by the matrix of maximum values
# proximity <- MtM / max_matrix
# diag(proximity) <- 0
# 
# ## Calculate Relatedness density
# RR <- relatedness_density(M, proximity)
# RR


# ---- a-rd-out ----
round(RR, 1)


# ---- a-rd-ext-out ----
RR_ext


# ---- fig-a-frontier-idea ----
q <- data.frame(x = c(1, 3, 1, 3), y = c(3, 3, 1, 1),
                label = c("Hard to reach,\nhigh reward", "Best targets:\nlow risk, high reward",
                          "Poor choice:\nhigh risk, low reward", "Easy to reach,\nlow reward"))
ggplot(q, aes(x, y, label = label)) +
  annotate("rect", xmin = 2, xmax = 4, ymin = 2, ymax = 4, fill = book_blue, alpha = 0.12) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "#5A6675") +
  geom_vline(xintercept = 2, linetype = "dashed", color = "#5A6675") +
  geom_text(size = 3) +
  scale_x_continuous(limits = c(0, 4), breaks = 2, labels = "Mean relatedness",
                     expand = c(0, 0), name = "Relatedness density (feasibility)") +
  scale_y_continuous(limits = c(0, 4), breaks = 2, labels = "Country's\ncomplexity",
                     expand = c(0, 0), name = "Product complexity (reward)") +
  theme(panel.grid = element_blank(), axis.ticks = element_blank())


# ---- fig-a-shapes ----
tr <- res$trade
shape_names <- c(AGO = "Angola", ROU = "Romania", SGP = "Singapore")
shape_data <- bind_rows(lapply(names(shape_names), function(cc) {
  data.frame(country = shape_names[[cc]],
             rd = as.vector(tr$RR[cc, ]), pci = tr$PCI)
}))
shape_data <- shape_data[!is.na(shape_data$rd), ]
shape_data$country <- factor(shape_data$country, levels = shape_names)
ggplot(shape_data, aes(rd, pci)) +
  geom_point(color = book_blue, alpha = 0.35, size = 0.6, stroke = 0) +
  geom_smooth(method = "lm", se = FALSE, color = book_orange, linewidth = 0.7) +
  facet_wrap(~country, scales = "free_x") +
  scale_x_continuous(n.breaks = 4) +
  labs(x = "Relatedness density", y = "Product complexity") +
  theme(panel.spacing.x = unit(1.4, "lines"))


# ---- a-trade-read ----
data_2021 <- as.data.frame(
  read_excel("data/trade_matrix_data_2021.xlsx"))
dim(data_2021)


# ---- a-trade-code ----
# # Set the first column as row names
# xx <- data_2021[[1]]
# rownames(data_2021) <- xx
# # Remove the first column from the data frame
# data_2021 <- as.matrix(data_2021[, -1])
# # calculate M
# M_2021 <- location_quotient(data_2021, binary = TRUE)
# PCI <- mort(M_2021)
# 
# # Calculate the product matrix
# MtM <- t(M_2021) %*% M_2021
# # Get the diagonal elements (which are M_ii)
# diagonal <- diag(MtM)
# # Create a matrix of maximum values between M_ii and M_jj
# max_matrix <- outer(diagonal, diagonal, pmax)
# # Divide the product matrix by the matrix of maximum values
# proximity <- MtM / max_matrix
# diag(proximity) <- 0
# # Relatedness density, leaving out products already exported with RCA
# RR <- 100 * t(t(M_2021 %*% proximity) / colSums(proximity))
# RR[M_2021 == 1] <- NA
# 
# # Values for North Macedonia
# index_of_mkd <- which(xx == "MKD")
# RR_mkd <- RR[index_of_mkd, , drop = FALSE]
# rr_mkd_value <- as.vector(RR_mkd)
# pci_value <- as.vector(PCI)
# 
# # The horizontal line: North Macedonia's ECI
# ECI <- morc(M_2021)
# ECI_mkd <- ECI[index_of_mkd]


# ---- a-trade-cached ----
pci_value <- as.vector(tr$PCI)
rr_mkd_value <- as.vector(tr$RR["MKD", ])
ECI_mkd <- tr$ECI["MKD"]
product_names <- tr$products


# ---- fig-a-frontier ----
mean_rr_mkd <- mean(rr_mkd_value, na.rm = TRUE)

frontier <- data.frame(rd = rr_mkd_value, pci = pci_value)
frontier <- frontier[!is.na(frontier$rd), ]
frontier$candidate <- frontier$rd >= mean_rr_mkd &
  frontier$pci >= ECI_mkd

ggplot(frontier, aes(rd, pci, color = candidate)) +
  geom_point(size = 0.9, alpha = 0.6, stroke = 0) +
  geom_hline(yintercept = ECI_mkd, linetype = "dashed") +
  geom_vline(xintercept = mean_rr_mkd, linetype = "dashed") +
  annotate("text", x = Inf, y = Inf, label = "Best targets",
           hjust = 1.1, vjust = 1.6, size = 3, color = "#1F66A8") +
  scale_color_manual(values = c("#AEB6C1", "#1F66A8"),
                     labels = c("Other products", "Candidates")) +
  guides(color = guide_legend(override.aes = list(size = 2.5,
                                                  alpha = 1))) +
  labs(x = "Relatedness density", y = "Product complexity index",
       color = NULL) +
  theme(legend.position = "top")


# ---- a-suggestions ----
# Create a data frame
mkd_suggestions <- data.frame(
  ProductNames = product_names,
  Relatedness = rr_mkd_value,
  PCI = pci_value
)
# Create the table for suggestions
mkd_suggestions <- mkd_suggestions[
  !is.na(mkd_suggestions$Relatedness) &
    mkd_suggestions$PCI >= ECI_mkd &
    mkd_suggestions$Relatedness >= mean_rr_mkd, ]
nrow(mkd_suggestions)
top <- head(mkd_suggestions[order(-mkd_suggestions$Relatedness), ], 8)
top$Relatedness <- round(top$Relatedness, 1)
top$PCI <- round(top$PCI, 1)
top


# ---- a-mk-exports ----
mk <- tr$mk_exports
round(sort(mk / sum(mk), decreasing = TRUE)[1:4], 3)
c(total_billion = round(sum(mk) / 1e9, 2),
  hhi = round(sum((mk / sum(mk))^2), 3))

