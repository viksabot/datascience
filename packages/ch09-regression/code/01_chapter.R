# Chapter 9: Linear Regression
# The code of the chapter, in the order of the book.
# Lines that begin with ## were not run in the book, usually because they
# need the internet or a package that is not always installed.

# ---- fig-r-line ----
line_data <- data.frame(GDP = c(15, 20, 25, 30, 40, 45, 50, 60))
line_data$Exports <- -2 + 0.2 * line_data$GDP
ggplot(line_data, aes(GDP, Exports)) +
  geom_abline(intercept = -2, slope = 0.2, color = book_gray) +
  geom_point(color = book_blue, size = 2) +
  labs(x = "GDP (billion dollars)", y = "Exports (billion dollars)")


# ---- fig-r-residuals ----
set.seed(21)
res_data <- data.frame(GDP = c(15, 19, 24, 28, 33, 38, 43, 48, 54, 60))
res_data$Exports <- -2 + 0.2 * res_data$GDP + rnorm(10, 0, 1.2)
fit_res <- lm(Exports ~ GDP, res_data)
res_data$fitted <- fitted(fit_res)
ggplot(res_data, aes(GDP, Exports)) +
  geom_abline(intercept = coef(fit_res)[1], slope = coef(fit_res)[2],
              color = book_gray) +
  geom_segment(aes(xend = GDP, yend = fitted), linetype = "dashed",
               color = book_orange, linewidth = 0.4) +
  geom_point(color = book_blue, size = 2) +
  labs(x = "GDP (billion dollars)", y = "Exports (billion dollars)")


# ---- r-read ----
# Load necessary libraries
library(readxl)
library(tidyverse)
# setwd("YourDirectory")
# Read the dataset from the Excel file
data <- read_excel("data/regression_example_data.xlsx")
# View the first few rows of the data to ensure it's loaded correctly
head(data, 3)


# ---- r-cyrillic ----
charToRaw(substr(names(data)[7], 1, 1))


# ---- r-fix-name ----
names(data)[7] <- "electricity_access"


# ---- fig-r-scatter ----
plot(internet_penetration ~ mean_years_schooling, data = data)


# ---- fig-r-abline ----
model <- lm(internet_penetration ~ mean_years_schooling, data = data)
plot(internet_penetration ~ mean_years_schooling, data = data)
abline(model)


# ---- r-summary1 ----
summary(model)


# ---- r-predict1 ----
predict(model, newdata = data.frame(mean_years_schooling = 13))


# ---- r-confint ----
confint(model)


# ---- r-multi ----
model <- lm(internet_penetration ~ mean_years_schooling +
              electricity_access, data = data)
summary(model)


# ---- r-predict2 ----
predict(model, newdata = data.frame(mean_years_schooling = 13,
                                    electricity_access = 90))


# ---- r-okun ----
okun <- read.csv("data/us_okun.csv")
okun_model <- lm(d_unemp ~ growth, data = okun)
round(coef(summary(okun_model)), 3)
summary(okun_model)$r.squared


# ---- fig-r-okun ----
lab_years <- okun[okun$year %in% c(1950, 1982, 2009, 2020, 2021), ]
ggplot(okun, aes(growth, d_unemp)) +
  geom_hline(yintercept = 0, color = "#D5DAE1") +
  geom_smooth(method = "lm", se = FALSE, color = book_orange, linewidth = 0.7) +
  geom_point(color = book_blue, size = 1.6) +
  geom_text(data = lab_years, aes(label = year), size = 2.7, nudge_y = 0.35) +
  labs(x = "Real GDP growth (percent)",
       y = "Change in unemployment rate\n(percentage points)")


# ---- r-six ----
model <- lm(internet_penetration ~ life_expectancy +
              expected_years_schooling + mean_years_schooling +
              income + electricity_access + urban_population,
            data = data)
summary(model)


# ---- r-overfit ----
set.seed(8)
noise <- as.data.frame(matrix(rnorm(60 * 25), ncol = 25))
noise$y <- rnorm(60)

fit <- lm(y ~ ., data = noise[1:30, ])
summary(fit)$r.squared
c(train = mean(residuals(fit)^2),
  test = mean((predict(fit, noise[31:60, ]) - noise$y[31:60])^2))


# ---- r-split ----
set.seed(1)
train.indices <- sample(1:nrow(data), nrow(data) / 2)
training <- data[train.indices, ]
testing <- data[-train.indices, ]

model <- lm(internet_penetration ~ life_expectancy +
              expected_years_schooling + mean_years_schooling +
              income + electricity_access + urban_population,
            data = training)
round(coef(summary(model)), 4)


# ---- r-mse-train ----
mean((predict(model) - training$internet_penetration)^2)


# ---- r-mse-test ----
mean((predict(model, newdata = testing) -
        testing$internet_penetration)^2)


# ---- r-simpler ----
simpler.model <- lm(internet_penetration ~ expected_years_schooling +
                      electricity_access + urban_population + income,
                    data = training)
mean((predict(simpler.model) - training$internet_penetration)^2)
mean((predict(simpler.model, newdata = testing) -
        testing$internet_penetration)^2)


# ---- fig-r-kfold ----
folds <- expand.grid(round = 1:5, group = 1:5)
folds$role <- ifelse(folds$round == 6 - folds$group, "Test", "Train")
ggplot(folds, aes(group, round, fill = role)) +
  geom_tile(color = "white", linewidth = 1.2, height = 0.86) +
  scale_fill_manual(values = c(Test = book_orange, Train = "#BBD3EA"), name = NULL) +
  scale_x_continuous(breaks = 1:5, labels = c("a", "b", "c", "d", "e"), name = "Group") +
  scale_y_continuous(breaks = 1:5, labels = paste("Round", 5:1), name = NULL) +
  theme(panel.grid = element_blank())


# ---- r-cv ----
library(boot)
set.seed(1)

bad.model <- glm(internet_penetration ~ life_expectancy +
                   expected_years_schooling + mean_years_schooling +
                   income + electricity_access + urban_population,
                 data = data)
better.model <- glm(internet_penetration ~ expected_years_schooling +
                      mean_years_schooling + income +
                      electricity_access + urban_population,
                    data = data)

bad.cv.err <- cv.glm(data, bad.model, K = 5)
# the cross-validated MSE estimate we will be using
# is a bias-corrected one stored as the second element
# in the 'delta' vector of the cv.err object
bad.cv.err$delta[2]

better.cv.err <- cv.glm(data, better.model, K = 5)
better.cv.err$delta[2]


# ---- r-hdi ----
hdi.model <- glm(internet_penetration ~ HDI + electricity_access +
                   urban_population, data = data)
hdi.cv.err <- cv.glm(data, hdi.model, K = 5)
hdi.cv.err$delta[2]


# ---- r-gravity-read ----
data <- read_excel("data/gravity_data.xlsx")
data


# ---- r-gravity-model ----
data$log_trade <- log(data$trade)
data$log_gdp_o <- log(data$gdp_o)
data$log_gdp_d <- log(data$gdp_d)
data$log_distance <- log(data$distance)

model <- lm(log_trade ~ log_gdp_o + log_gdp_d + log_distance,
            data = data)
summary(model)


# ---- r-gravity-predict ----
data$predicted_log_trade <- predict(model, newdata = data)
data$predicted_trade <- exp(data$predicted_log_trade)


# ---- r-gravity-cv ----
# Set seed for reproducibility
set.seed(123)
# Define your full gravity model
gravity_model <- glm(log_trade ~ log_gdp_o + log_gdp_d + log_distance,
                     data = data)
# Perform 10-fold cross-validation
cv_results <- cv.glm(data, gravity_model, K = 10)
# Obtain cross-validated Mean Squared Error (MSE)
cv_mse_gravity <- cv_results$delta[1]
# Define baseline model with only distance
baseline_model <- glm(log_trade ~ log_distance, data = data)
# Cross-validation for baseline model
cv_baseline_results <- cv.glm(data, baseline_model, K = 10)
# Obtain cross-validated MSE for baseline model
cv_mse_baseline <- cv_baseline_results$delta[1]
# Print results
cat("Cross-validated MSE (Gravity Model):", cv_mse_gravity, "\n")
cat("Cross-validated MSE (Baseline - Distance only):",
    cv_mse_baseline, "\n")


# ---- r-gravity-potential ----
data$export_potential <- ifelse(
  data$predicted_trade - data$trade > 0,
  data$predicted_trade - data$trade, 0)


# ---- fig-r-potential ----
index_of_mkd <- which(data$origin == "North Macedonia")
export_potential_mkd <- data.frame(
  destination = data$destination[index_of_mkd],
  export_potential = data$export_potential[index_of_mkd] / 10^6
)
export_potential_mkd <- export_potential_mkd[
  order(-export_potential_mkd$export_potential), ]

# Get top 10 destinations
top_10_data <- export_potential_mkd %>%
  arrange(desc(export_potential)) %>%
  slice_head(n = 10)

# Plotting with ggplot2
ggplot(top_10_data, aes(x = reorder(destination, export_potential),
                        y = export_potential)) +
  geom_col(fill = "#1F66A8") +
  coord_flip() +
  labs(x = NULL, y = "Export potential (million dollars)")


# ---- r-gravity-check ----
mk <- data[index_of_mkd, ]
round(c(actual_total = sum(mk$trade) / 1e6,
        predicted_total = sum(mk$predicted_trade) / 1e6,
        total_potential = sum(mk$export_potential) / 1e6))

egypt <- data[data$origin == "North Macedonia" &
                data$destination == "Egypt", ]
round(exp(predict(model, newdata = egypt,
                  interval = "prediction")) / 1e6, 2)

