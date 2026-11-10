# Chapter 12: Applications: Finance
# The code of the chapter, in the order of the book.
# Lines that begin with ## were not run in the book, usually because they
# need the internet or a package that is not always installed.

# ---- f-read ----
fin <- read.csv("data/french_industries_monthly.csv",
                check.names = FALSE)
dim(fin)
fin[1:3, 1:7]


# ---- f-matrix ----
r <- as.matrix(fin[, -(1:3)])


# ---- f-summary ----
risk_return <- data.frame(
  industry = colnames(r),
  mean = colMeans(r),
  sd = apply(r, 2, sd)
)
risk_return$sharpe <-
  (risk_return$mean - mean(fin$riskfree)) / risk_return$sd
rownames(risk_return) <- NULL

top <- risk_return[order(-risk_return$sharpe), ]
head(top, 3)
tail(top, 3)
cor(risk_return$mean, risk_return$sd)


# ---- fig-f-riskreturn ----
show_names <- c("Food", "Util", "Chips", "Autos", "Coal", "Gold", "Banks", "Books")
ggplot(risk_return, aes(sd, mean)) +
  geom_point(color = book_blue, size = 1.8) +
  ggrepel::geom_text_repel(
    data = risk_return[risk_return$industry %in% show_names, ],
    aes(label = industry), size = 2.9, seed = 1) +
  labs(x = "Standard deviation of monthly return (%)",
       y = "Mean monthly return (%)")


# ---- f-market ----
market <- fin$market_excess + fin$riskfree
c(mean = mean(market), sd = sd(market),
  skewness = skewness(market), kurtosis = kurtosis(market))

z <- (market - mean(market)) / sd(market)
c(beyond_2sd = sum(abs(z) > 2), expected = 240 * 2 * pnorm(-2))
c(beyond_3sd = sum(abs(z) > 3), expected = 240 * 2 * pnorm(-3))


# ---- fig-f-hist ----
ggplot(data.frame(market), aes(market)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1.5,
                 fill = book_blue, color = "white", linewidth = 0.2) +
  stat_function(fun = dnorm, args = list(mean = mean(market), sd = sd(market)),
                color = book_orange, linewidth = 0.8) +
  labs(x = "Monthly return (%)", y = "Density")


# ---- f-corr ----
cors <- cor(r)
pairs <- cors[upper.tri(cors)]
c(mean = mean(pairs), min = min(pairs), max = max(pairs))
round(cors["Banks", "Gold"], 2)


# ---- f-two ----
banks <- r[, "Banks"]
gold <- r[, "Gold"]
w <- seq(0, 1, by = 0.01)
sd_p <- sqrt(w^2 * var(banks) + (1 - w)^2 * var(gold) +
               2 * w * (1 - w) * cov(banks, gold))
c(sd_banks = sd(banks), sd_gold = sd(gold))
c(lowest_risk = min(sd_p), share_in_banks = w[which.min(sd_p)])


# ---- f-curve ----
set.seed(123)
sizes <- c(1, 2, 3, 5, 10, 20, 30, 48)
risk <- sapply(sizes, function(n) {
  mean(replicate(1000, {
    pick <- sample(ncol(r), n)
    sd(rowMeans(r[, pick, drop = FALSE]))
  }))
})
round(setNames(risk, sizes), 2)


# ---- fig-f-curve ----
limit <- sqrt(mean(cov(r)[upper.tri(cov(r))]))
ggplot(data.frame(n = sizes, risk), aes(n, risk)) +
  geom_hline(yintercept = limit, linetype = "dashed", color = "#5A6675") +
  geom_line(color = book_blue, linewidth = 0.8) +
  geom_point(color = book_blue, size = 2) +
  annotate("text", x = 48, y = limit - 0.1, hjust = 1, size = 2.9,
           label = "Risk that diversification cannot remove", color = "#5A6675") +
  scale_y_continuous(limits = c(4.5, 7)) +
  labs(x = "Number of industries in the portfolio",
       y = "Standard deviation (%)")


# ---- f-cluster ----
train <- fin$month <= 201512
returns_scaled <- t(scale(r[train, ]))

set.seed(123)
wss <- sapply(1:10, function(k) {
  kmeans(returns_scaled, centers = k, nstart = 25)$tot.withinss
})
km <- kmeans(returns_scaled, centers = 6, nstart = 25)
split(names(km$cluster), km$cluster)


# ---- f-test ----
test <- r[!train, ]
groups <- split(names(km$cluster), km$cluster)
draw_one <- function(x) x[sample(length(x), 1)]
largest <- groups[[which.max(lengths(groups))]]

test_risk <- function(pick) sd(rowMeans(test[, pick]))

set.seed(123)
one_each <- replicate(2000, test_risk(sapply(groups, draw_one)))
same_cluster <- replicate(2000, test_risk(sample(largest, 6)))
at_random <- replicate(2000,
                       test_risk(sample(colnames(test), 6)))
round(c(one_each = mean(one_each), same_cluster = mean(same_cluster),
        at_random = mean(at_random)), 2)


# ---- f-beta ----
excess <- r - fin$riskfree
market_excess <- fin$market_excess

banks_model <- lm(excess[, "Banks"] ~ market_excess)
round(coef(summary(banks_model)), 3)
summary(banks_model)$r.squared


# ---- f-betas ----
get_beta <- function(y) coef(lm(y ~ market_excess))[2]
betas <- apply(excess, 2, get_beta)
round(sort(betas)[c(1:3, 46:48)], 2)


# ---- f-sml ----
mean_excess <- colMeans(excess)
sml <- lm(mean_excess ~ betas)
round(coef(summary(sml)), 3)
summary(sml)$r.squared
mean(market_excess)


# ---- fig-f-sml ----
sml_data <- data.frame(industry = names(betas), beta = betas, mean = mean_excess)
ggplot(sml_data, aes(beta, mean)) +
  geom_abline(intercept = 0, slope = mean(market_excess),
              linetype = "dashed", color = "#5A6675") +
  geom_smooth(method = "lm", se = FALSE, color = book_orange, linewidth = 0.8) +
  geom_point(color = book_blue, size = 1.8) +
  ggrepel::geom_text_repel(
    data = sml_data[sml_data$industry %in% c("Gold", "Food", "Autos", "Chips", "Coal", "Banks"), ],
    aes(label = industry), size = 2.9, seed = 1) +
  labs(x = "Beta", y = "Mean excess return (% per month)")


# ---- f-prices ----
prices <- read.csv("data/oecd_share_prices.csv")
prices <- prices[prices$date >= "2000-01-01", ]
markets <- c("Greece", "Turkiye", "Hungary", "Slovenia",
             "Poland", "Czechia")

returns <- 100 * diff(log(as.matrix(prices[, c(markets, "Germany")])))
months <- prices$date[-1]
vix <- prices$VIX[-1]
germany <- returns[, "Germany"]
nrow(returns)


# ---- f-coex ----
cutoff <- apply(returns[, markets], 2, quantile, probs = 0.05)
round(cutoff, 1)

extreme <- sweep(returns[, markets], 2, cutoff, "<=")
coexceed <- rowSums(extreme)
table(coexceed)


# ---- f-indep ----
c(actual = mean(coexceed >= 2),
  if_independent = 1 - dbinom(0, 6, 0.05) - dbinom(1, 6, 0.05))
months[coexceed >= 4]


# ---- fig-f-coex ----
coex_plot <- rbind(
  data.frame(k = 1:6, n = as.vector(table(factor(coexceed, levels = 1:6))), Type = "Actual"),
  data.frame(k = 1:6, n = dbinom(1:6, 6, 0.05) * length(coexceed), Type = "If independent")
)
ggplot(coex_plot, aes(factor(k), n, fill = Type)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  scale_fill_manual(values = c("Actual" = book_blue, "If independent" = book_gray)) +
  labs(x = "Number of markets with an extreme fall in the same month",
       y = "Months", fill = NULL) +
  theme(legend.position = "top")


# ---- f-logit ----
joint_fall <- as.integer(coexceed >= 2)
coex_model <- glm(joint_fall ~ germany + vix, family = binomial)
round(coef(summary(coex_model)), 3)

predict(coex_model, type = "response",
        newdata = data.frame(germany = c(0, -10), vix = c(15, 30)))

coex_prob <- predict(coex_model, type = "response")
table(Predicted = coex_prob > 0.5, Actual = joint_fall)


# ---- f-hmda ----
hmda <- read.csv("data/hmda_boston.csv", stringsAsFactors = TRUE)
hmda$denied <- as.integer(hmda$deny == "yes")
tapply(hmda$denied, hmda$afam, mean)
table(hmda$afam)


# ---- f-denial ----
denial_model <- glm(denied ~ pirat + hirat + lvrat + chist + mhist +
                      phist + selfemp + insurance + afam,
                    data = hmda, family = binomial)
round(coef(summary(denial_model)), 3)

# Predicted denial for everyone, with afam set to each value
as_black <- as_other <- hmda
as_black$afam[] <- "yes"
as_other$afam[] <- "no"
gap <- predict(denial_model, as_black, type = "response") -
  predict(denial_model, as_other, type = "response")
round(c(all = mean(gap), black_applicants = mean(gap[hmda$afam == "yes"])), 3)


# ---- f-fair-fit ----
set.seed(123)
in_train <- sample(nrow(hmda), round(0.7 * nrow(hmda)))
loan_train <- hmda[in_train, ]
loan_test <- hmda[-in_train, ]

blind_model <- glm(denied ~ pirat + hirat + lvrat + chist + mhist +
                     phist + selfemp + insurance,
                   data = loan_train, family = binomial)
loan_test$prob <- predict(blind_model, loan_test, type = "response")

table(Flagged = loan_test$prob > 0.5, Denied = loan_test$denied)
c(accuracy = mean((loan_test$prob > 0.5) == loan_test$denied),
  base_rate = mean(loan_test$denied == 0))


# ---- f-fair ----
fairness <- function(g, threshold = 0.2) {
  s <- loan_test[loan_test$afam == g, ]
  flagged <- s$prob > threshold
  c(applicants = nrow(s),
    denied_rate = mean(s$denied),
    flagged_rate = mean(flagged),
    false_flag = mean(flagged[s$denied == 0]),
    missed = mean(!flagged[s$denied == 1]))
}
round(sapply(c(other = "no", black = "yes"), fairness), 3)


# ---- f-fd ----
fd <- read.csv("data/wb_financial_development.csv")
fd <- fd[complete.cases(fd[, c("credit", "accounts", "atms",
                               "gdp_pc")]), ]
fd <- fd[fd$atms > 0 & fd$credit > 0, ]
nrow(fd)


# ---- f-index ----
minmax <- function(x) 100 * (x - min(x)) / (max(x) - min(x))

fd$depth <- minmax(log(fd$credit))
fd$access <- minmax(fd$accounts)
fd$reach <- minmax(log(fd$atms))
fd$index <- (fd$depth + fd$access + fd$reach) / 3

fd <- fd[order(-fd$index), ]
fd$rank <- seq_len(nrow(fd))
fd[1:4, c("country", "index", "rank")]
balkans <- c("MKD", "SRB", "BIH", "XKX", "ALB")
fd[fd$iso3c %in% balkans,
   c("country", "depth", "access", "reach", "index", "rank")]


# ---- f-gap ----
gap_model <- lm(index ~ log(gdp_pc), data = fd)
summary(gap_model)$r.squared
fd$gap <- residuals(gap_model)
fd[fd$iso3c %in% balkans, c("country", "gap")]


# ---- fig-f-index ----
fd$balkan <- fd$iso3c %in% balkans
fd$short <- c(MKD = "North Macedonia", SRB = "Serbia", BIH = "Bosnia and Herz.",
              XKX = "Kosovo", ALB = "Albania")[fd$iso3c]
ggplot(fd, aes(gdp_pc, index)) +
  geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE,
              color = book_gray, linewidth = 0.8) +
  geom_point(data = fd[!fd$balkan, ], color = "#9DBAD6", size = 1.5) +
  geom_point(data = fd[fd$balkan, ], color = book_orange, size = 2.2) +
  ggrepel::geom_text_repel(data = fd[fd$balkan, ], aes(label = short),
                           size = 2.9, seed = 4, color = book_orange,
                           nudge_y = -24, direction = "both", box.padding = 0.5,
                           segment.color = book_orange, segment.size = 0.25,
                           min.segment.length = 0, max.overlaps = Inf) +
  scale_x_log10(labels = label_comma()) +
  labs(x = "GDP per person (PPP, international dollars, log scale)",
       y = "Financial development index")


# ---- f-fd-kmeans ----
set.seed(123)
fd_km <- kmeans(scale(fd[, c("depth", "access", "reach")]),
                centers = 3, nstart = 25)
fd$cluster <- fd_km$cluster
aggregate(cbind(depth, access, reach, gdp_pc) ~ cluster,
          data = fd, FUN = function(x) round(mean(x)))
fd[fd$iso3c %in% balkans, c("country", "cluster")]

