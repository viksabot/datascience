# Chapter 1: Why Data Science for Economists?
# The code of the chapter, in the order of the book.
# Lines that begin with ## were not run in the book, usually because they
# need the internet or a package that is not always installed.

# ---- fig-kuznets ----
kuznets <- data.frame(year = 1929:1932,
                      income = c(83.032, 70.345, 54.643, 39.365))
ggplot(kuznets, aes(factor(year), income)) +
  geom_col(fill = book_blue, width = 0.62) +
  geom_text(aes(label = sprintf("%.1f", income)), vjust = -0.45, size = 3.1) +
  scale_y_continuous(limits = c(0, 95), expand = c(0, 0)) +
  labs(x = NULL, y = "US$ billions")


# ---- fig-uk-solar ----
solar <- read.csv("data/uk_solar_imports_annual.csv")
measures <- c("Value (US$ millions)", "Weight (thousand tonnes)")
panels <- rbind(
  data.frame(year = solar$year, measure = measures[1], amount = solar$value_usd / 1e6),
  data.frame(year = solar$year, measure = measures[2], amount = solar$tons / 1e3)
)
panels$measure <- factor(panels$measure, levels = measures)
ggplot(panels, aes(factor(year), amount, fill = measure)) +
  geom_col(width = 0.62, show.legend = FALSE) +
  geom_text(aes(label = round(amount)), vjust = -0.4, size = 2.7) +
  facet_wrap(~measure, scales = "free_y") +
  scale_fill_manual(values = c(book_blue, book_orange)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.14))) +
  labs(x = NULL, y = NULL)


# ---- fig-mk-exports ----
library(treemapify)
hs_names <- c(
  "38" = "Chemical products", "85" = "Electrical machinery",
  "94" = "Furniture", "72" = "Iron and steel", "84" = "Machinery",
  "87" = "Vehicles and parts", "62" = "Clothing", "27" = "Mineral fuels",
  "26" = "Ores", "73" = "Iron and steel articles"
)
mk <- read.csv("data/mk_exports_hs2.csv") %>%
  mutate(group = ifelse(as.character(hs2) %in% names(hs_names),
                        hs_names[as.character(hs2)], "All other products")) %>%
  group_by(group) %>%
  summarise(value = sum(value_usd)) %>%
  mutate(share = 100 * value / sum(value),
         label = paste0(group, "\n", round(share, 1), "%"),
         other = group == "All other products")
ggplot(mk, aes(area = value, fill = other, label = label)) +
  geom_treemap(color = "white", size = 1.5) +
  geom_treemap_text(color = "white", size = 8, place = "centre", reflow = TRUE) +
  scale_fill_manual(values = c(`FALSE` = book_blue, `TRUE` = "#7D8896"),
                    guide = "none")

