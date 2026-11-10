# Chapter 10: Classification
# The code of the chapter, in the order of the book.
# Lines that begin with ## were not run in the book, usually because they
# need the internet or a package that is not always installed.

# ---- fig-k-logistic ----
curve_data <- data.frame(y = seq(-7, 7, length.out = 200))
curve_data$p <- 1 / (1 + exp(-curve_data$y))
ggplot(curve_data, aes(y, p)) +
  geom_hline(yintercept = c(0, 1), color = "#D5DAE1") +
  geom_line(color = book_blue, linewidth = 0.8) +
  labs(x = "Prediction from the linear part", y = "Probability")


# ---- k-read ----
# Load necessary libraries
library(readxl)
library(tidyverse)
# setwd("YourDirectory")
# Read the dataset from the Excel file
data <- read_excel("data/regression_example_data.xlsx")
names(data)[7] <- "electricity_access"   # see Chapter 9
# View the first few rows of the data to ensure it's loaded correctly
head(data, 3)


# ---- k-binary ----
data <- data %>%
  mutate(high_internet_penetration =
           ifelse(internet_penetration > 75, 1, 0))


# ---- k-model ----
model <- glm(high_internet_penetration ~ mean_years_schooling +
               electricity_access, data = data,
             family = binomial(logit))
summary(model)


# ---- k-predict ----
predict(model, newdata = data.frame(mean_years_schooling = 13,
                                    electricity_access = 90),
        type = "response")


# ---- k-class-one ----
new_country <- data.frame(mean_years_schooling = 13,
                          electricity_access = 90)
predicted_probs <- predict(model, newdata = new_country,
                           type = "response")
predicted_class <- ifelse(predicted_probs > 0.5, 1, 0)
predicted_class


# ---- k-class-all ----
# Calculate predicted probabilities
predicted_probs <- predict(model, newdata = data, type = "response")
# Convert probabilities to binary classification (1 or 0)
# based on a threshold of 0.5
data$predicted_high_internet_penetration <-
  ifelse(predicted_probs > 0.5, 1, 0)

data %>%
  select(Country, high_internet_penetration,
         predicted_high_internet_penetration) %>%
  head(4)


# ---- tbl-k-confusion ----
cm <- data.frame(
  ` ` = c("Actual positive (P)", "Actual negative (N)"),
  `Predicted positive` = c("True positive (TP)", "False positive (FP)"),
  `Predicted negative` = c("False negative (FN)", "True negative (TN)"),
  check.names = FALSE
)
kable(cm, row.names = FALSE)


# ---- k-confusion ----
predicted_class_factor <- factor(
  data$predicted_high_internet_penetration, levels = c(1, 0))
actual_class_factor <- factor(
  data$high_internet_penetration, levels = c(1, 0))
# Create the confusion matrix with the specified order
conf_matrix <- table(Predicted = predicted_class_factor,
                     Actual = actual_class_factor)
conf_matrix


# ---- k-accuracy ----
accuracy <- sum(data$high_internet_penetration ==
                  data$predicted_high_internet_penetration) /
  length(data$high_internet_penetration)
accuracy


# ---- k-metrics ----
metrics <- function(actual, prob, threshold) {
  pred <- ifelse(prob > threshold, 1, 0)
  tp <- sum(pred == 1 & actual == 1)
  fp <- sum(pred == 1 & actual == 0)
  fn <- sum(pred == 0 & actual == 1)
  tn <- sum(pred == 0 & actual == 0)
  round(c(threshold = threshold, accuracy = (tp + tn) / length(actual),
          precision = tp / max(tp + fp, 1), recall = tp / (tp + fn)), 3)
}

t(sapply(c(0.2, 0.5, 0.8), metrics,
         actual = data$high_internet_penetration,
         prob = predicted_probs))


# ---- k-cost ----
cost <- function(r, pi = 0) mean(abs(r - pi) > 0.5)


# ---- k-cv ----
library(boot)
set.seed(42)
cv.err <- cv.glm(data, model, cost = cost, K = 5)
100 * (1 - cv.err$delta[1])


# ---- k-recession ----
yc <- read.csv("data/us_yield_recession.csv")
yc <- yc[!is.na(yc$rec_12) & !is.na(yc$spread), ]

rec_model <- glm(rec_12 ~ spread, data = yc, family = binomial)
round(coef(summary(rec_model)), 3)

predict(rec_model, newdata = data.frame(spread = c(-1, 0, 2)),
        type = "response")


# ---- k-recession-acc ----
rec_prob <- predict(rec_model, type = "response")

c(always_no = mean(yc$rec_12 == 0))
t(sapply(c(0.15, 0.3, 0.5), metrics, actual = yc$rec_12,
         prob = rec_prob))


# ---- fig-k-recession ----
yc$date <- as.Date(yc$date)
yc$prob <- rec_prob
run <- rle(yc$USREC)
ends <- cumsum(run$lengths)
starts <- ends - run$lengths + 1
rec <- data.frame(start = yc$date[starts[run$values == 1]],
                  end = yc$date[ends[run$values == 1]])
ggplot(yc, aes(date, prob)) +
  geom_rect(data = rec, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#E3E6EB") +
  geom_line(color = book_blue, linewidth = 0.4) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Probability of recession\nin 12 months")


# ---- k-cars ----
df <- read_excel("data/trade_cars_2022.xlsx")

car_model <- glm(is_exporter ~ fdi + gdp + capital,
                 data = df, family = binomial)
round(coef(summary(car_model)), 4)

mkd_data <- df %>% filter(Exporter == "MKD")
predict(car_model, newdata = mkd_data, type = "response")


# ---- k-leak ----
leaky_model <- glm(is_exporter ~ fdi + gdp + pop + capital + exports_pc,
                   data = df, family = binomial)
set.seed(123)
100 * (1 - cv.glm(df, car_model, cost = cost, K = 5)$delta[1])
100 * (1 - cv.glm(df, leaky_model, cost = cost, K = 5)$delta[1])
predict(leaky_model, newdata = mkd_data, type = "response")


# ---- k-leak-check ----
all(df$is_exporter == (df$exports_pc > median(df$exports_pc)))

