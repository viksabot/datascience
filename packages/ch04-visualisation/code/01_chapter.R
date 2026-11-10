# Chapter 4: Visualising Economic Data
# The code of the chapter, in the order of the book.
# Lines that begin with ## were not run in the book, usually because they
# need the internet or a package that is not always installed.

# ---- fig-us-phillips ----
us <- read.csv("data/us_phillips.csv") %>%
  filter(year >= 1961, year <= 1983) %>%
  mutate(period = ifelse(year <= 1969, "1961 to 1969", "1970 to 1983"))
ggplot(us, aes(unemployment, inflation, color = period)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6) +
  annotate("text", x = 3.1, y = 7.7, label = "1961 to 1969", color = "#1F66A8", size = 3.1, hjust = 0) +
  annotate("text", x = 8.4, y = 13.2, label = "1970 to 1983", color = "#C2571A", size = 3.1, hjust = 0) +
  scale_color_manual(values = c("1961 to 1969" = book_blue, "1970 to 1983" = book_orange), guide = "none") +
  labs(x = "Unemployment rate (%)", y = "CPI inflation (%)") +
  theme(panel.grid.major.x = element_line(color = "#EEF0F3"))


# ---- fig-pareto ----
wealth <- data.frame(
  measure = rep(c("Share of adults", "Share of wealth"), each = 3),
  group = rep(c("Under $10,000", "$10,000 to $1 million", "Over $1 million"), 2),
  value = c(55, 43.9, 1.1, 1.3, 52.9, 45.8)
)
wealth$group <- factor(wealth$group, levels = c("Over $1 million", "$10,000 to $1 million", "Under $10,000"))
wealth$label <- ifelse(wealth$value < 2, "", paste0(wealth$value, "%"))
ggplot(wealth, aes(measure, value, fill = group)) +
  geom_col(width = 0.6, color = "white") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  scale_fill_manual(values = c("#123A63", "#1F66A8", "#7D8896"), name = NULL) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = "Percent") +
  theme(legend.position = "top", panel.grid = element_blank())


# ---- fig-wrong-type ----
balkans <- read.csv("data/gdp_balkans.csv") %>%
  mutate(GDP_billions = GDP / 1e9)
three <- c("Albania" = "#C2571A", "North Macedonia" = "#1F66A8", "Serbia" = "#2F8F6B")
p_line <- ggplot(balkans, aes(year, GDP_billions, color = country)) +
  geom_line(linewidth = 0.7) +
  scale_color_manual(values = three, guide = "none") +
  annotate("text", x = 2013, y = c(52, 16.5, 7), label = c("Serbia", "Albania", "N. Macedonia"),
           color = three[c(3, 1, 2)], size = 2.6) +
  labs(x = NULL, y = "GDP, US$ billions")
pie_data <- balkans %>%
  filter(year == 2022) %>%
  mutate(share = paste0(round(100 * GDP / sum(GDP)), "%"))
p_pie <- ggplot(pie_data, aes(x = "", y = GDP, fill = country)) +
  geom_col(width = 1, color = "white") +
  geom_text(aes(label = share), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  coord_polar("y") +
  scale_fill_manual(values = three, name = NULL) +
  guides(fill = guide_legend(ncol = 1)) +
  theme_void(base_size = 8) +
  theme(legend.position = "bottom", legend.key.size = unit(0.3, "cm"))
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2, widths = grid::unit(c(3, 2), "null"))))
print(p_line, vp = grid::viewport(layout.pos.col = 1, layout.pos.row = 1))
print(p_pie, vp = grid::viewport(layout.pos.col = 2, layout.pos.row = 1))


# ---- fig-too-much ----
library(tidyr)
ten <- read.csv("data/ten_countries_panel.csv", check.names = FALSE) %>%
  pivot_longer(c(`GDP growth`, Inflation, Unemployment, `Exports (% GDP)`),
               names_to = "indicator", values_to = "value")
ggplot(ten, aes(year, value, color = country)) +
  geom_line(linewidth = 0.35) +
  facet_wrap(~indicator, scales = "free_y", nrow = 1) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme(legend.position = "bottom", legend.text = element_text(size = 6.5),
        legend.key.height = unit(0.3, "cm"), axis.text = element_text(size = 6),
        strip.text = element_text(size = 7))


# ---- fig-map-colours ----
gdp22 <- read.csv("data/wb_cleaned_data.csv")
gdp22$GDP_billions <- gdp22$GDP / 1e9
gdp22$group <- cut(gdp22$GDP_billions, breaks = quantile(gdp22$GDP_billions, 0:5 / 5),
                   include.lowest = TRUE, labels = paste("Group", 1:5))
map_gdp <- read.csv("data/europe_map.csv") %>%
  arrange(group, order) %>%
  left_join(gdp22 %>% select(iso3c, GDP_billions, gdp_group = group), by = "iso3c")
map_base <- function(p) {
  p + geom_polygon(color = "white", linewidth = 0.1) +
    coord_fixed(1.6, xlim = c(-12, 42), ylim = c(35, 71)) +
    theme_void(base_size = 7) +
    theme(legend.position = "bottom", legend.key.size = unit(0.25, "cm"))
}
p_bad <- map_base(ggplot(map_gdp, aes(long, lat, group = group, fill = gdp_group))) +
  scale_fill_manual(values = c("#FF69B4", "#FFD700", "#00CED1", "#32CD32", "#FF8C00"),
                    na.value = "#DADDE2", name = NULL, na.translate = FALSE)
p_good <- map_base(ggplot(map_gdp, aes(long, lat, group = group, fill = GDP_billions))) +
  scale_fill_gradient(low = "#E8F0F8", high = "#123A63", trans = "log10", na.value = "#DADDE2",
                      name = "US$ bn", breaks = c(10, 100, 1000), labels = scales::comma)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
print(p_bad, vp = grid::viewport(layout.pos.col = 1, layout.pos.row = 1))
print(p_good, vp = grid::viewport(layout.pos.col = 2, layout.pos.row = 1))


# ---- fig-truncated ----
inc <- data.frame(country = c("Albania", "N. Macedonia", "Serbia", "Montenegro"),
                  income = c(6174, 7609, 7752, 9167))
inc$country <- factor(inc$country, levels = inc$country)
p_cut <- ggplot(inc, aes(country, income)) +
  geom_col(fill = book_orange, width = 0.65) +
  coord_cartesian(ylim = c(6000, 9500), expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 6.5))
p_zero <- ggplot(inc, aes(country, income)) +
  geom_col(fill = book_blue, width = 0.65) +
  coord_cartesian(ylim = c(0, 9500), expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_text(size = 6.5))
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, 2)))
print(p_cut, vp = grid::viewport(layout.pos.col = 1, layout.pos.row = 1))
print(p_zero, vp = grid::viewport(layout.pos.col = 2, layout.pos.row = 1))


# ---- v-setwd ----
# setwd("path/to/your/directory")


# ---- v-libs ----
library(readxl)
library(dplyr)
library(ggplot2)


# ---- v-read ----
# For CSV:
df_2022 <- read.csv("data/wb_cleaned_data.csv")

# For Excel (if the data are in that format):
# df_2022 <- read_excel("wb_cleaned_data.xlsx")


# ---- v-millions ----
df_2022$Pop_millions <- df_2022$Pop / 1e6
df_2022$GDP_millions <- df_2022$GDP / 1e6


# ---- fig-scatter-basic ----
ggplot(df_2022, aes(x = Pop_millions, y = GDP_millions)) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Relationship between population and GDP (2022)",
    x = "Population (millions)",
    y = "GDP (millions USD)"
  ) +
  theme_minimal()


# ---- v-ggsave-1 ----
# ggsave("plot_2022.png", width = 8, height = 6, dpi = 300)


# ---- v-repel-install ----
# install.packages("scales")
# install.packages("ggrepel")


# ---- v-repel-load ----
library(scales)
library(ggrepel)


# ---- v-piece-1 ----
# ggplot(df_2022, aes(x = Pop_millions, y = GDP_millions)) +
#   geom_point(color = "steelblue", alpha = 0.7) +


# ---- v-piece-2 ----
#   geom_smooth(method = "lm", se = FALSE, color = "red") +


# ---- v-piece-3 ----
#   geom_text_repel(
#     aes(label = iso3c),
#     size = 3,
#     color = "black"
#   ) +


# ---- v-piece-4 ----
#   scale_x_log10(labels = comma) +
#   scale_y_log10(labels = comma) +


# ---- v-piece-5 ----
#   labs(
#     title = "Population (millions) vs. GDP (millions), 2022",
#     x = "Population (millions, log scale)",
#     y = "GDP (millions USD, log scale)"
#   ) +


# ---- v-piece-6 ----
#   theme_minimal(base_size = 12) +
#   theme(
#     plot.title = element_text(size = 14, hjust = 0.5),
#     axis.title.x = element_text(size = 12),
#     axis.title.y = element_text(size = 12),
#     axis.text = element_text(size = 10)
#   )


# ---- fig-scatter-final ----
ggplot(df_2022, aes(x = Pop_millions, y = GDP_millions)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_text_repel(aes(label = iso3c), size = 2.4, color = "black",
                  max.overlaps = Inf, segment.size = 0.2) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(
    title = "Population (millions) vs. GDP (millions), 2022",
    x = "Population (millions, log scale)",
    y = "GDP (millions USD, log scale)"
  ) +
  theme_minimal(base_size = 9)


# ---- v-ggsave-2 ----
# # Save the chart as a PNG
# ggsave("pop_vs_gdp_per_capita_2022.png",
#        width = 7, height = 5, dpi = 300)


# ---- fig-bar ----
ggplot(
  data = df_2022,
  mapping = aes(x = reorder(iso3c, -Pop_millions),
                y = Pop_millions)
) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Population (millions) by Country",
    x = "Country (ISO3)",
    y = "Population (millions)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 6)
  )


# ---- v-line-get ----
# library(WDI)
# library(ggplot2)
# library(scales)
# 
# # Download data for North Macedonia, Albania and Serbia
# df_countries <- WDI(
#   country   = c("MKD", "ALB", "SRB"),
#   indicator = c("GDP" = "NY.GDP.MKTP.CD"),
#   start     = 2002,
#   end       = 2022
# )


# ---- v-line-load ----
df_countries <- read.csv("data/gdp_balkans.csv")


# ---- fig-line ----
# Create a new column with GDP in millions
df_countries$GDP_millions <- df_countries$GDP / 1e6

# Show GDP over time with a line chart
ggplot(df_countries,
       aes(x = year, y = GDP_millions, color = country)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(
    values = c("#C2571A", "#1F66A8", "#2F8F6B")
  ) +
  labs(
    title = "GDP (millions current US$)",
    x = "Year",
    y = "GDP (million US$)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


# ---- v-line-save ----
# ggsave("gdp_line_chart.png", width = 7, height = 5, dpi = 300)


# ---- fig-mk-real ----
mk <- read.csv("data/mk_gdp_nominal_real.csv") %>%
  arrange(year) %>%
  mutate(`Current US dollars` = 100 * current_usd / first(current_usd),
         `Constant denars` = 100 * constant_lcu / first(constant_lcu)) %>%
  select(year, `Current US dollars`, `Constant denars`) %>%
  pivot_longer(-year, names_to = "measure", values_to = "index")
ggplot(mk, aes(year, index, color = measure)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = 2015, linetype = "dotted", color = "#5A6675") +
  annotate("text", x = 2015.2, y = 395, label = "2015", hjust = 0, size = 2.8, color = "#5A6675") +
  annotate("text", x = 2012, y = c(330, 118), label = c("Current US dollars", "Constant denars"),
           color = c(book_orange, book_blue), size = 3.1) +
  scale_color_manual(values = c("Current US dollars" = book_orange, "Constant denars" = book_blue), guide = "none") +
  labs(x = NULL, y = "Index, 2002 = 100")


# ---- fig-stacked ----
ggplot(df_countries,
       aes(x = year, y = GDP_millions, fill = country)) +
  geom_area(alpha = 0.7, color = "black", position = "stack",
            linewidth = 0.2) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(
    values = c("#C2571A", "#1F66A8", "#2F8F6B")
  ) +
  labs(
    title = "Stacked area chart of GDP (millions US$)",
    x = "Year",
    y = "GDP (million US$)",
    fill = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


# ---- fig-pie ----
df_top5 <- df_2022 %>%
  arrange(desc(Pop_millions)) %>%
  head(5)

ggplot(
  data = df_top5,
  mapping = aes(x = "", y = Pop_millions,
                fill = reorder(iso3c, -Pop_millions))
) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(
    title = "Pie Chart of Population (millions) by Country",
    fill = "Country (ISO3)"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  theme_void()


# ---- fig-treemap ----
library(treemapify)

ggplot(df_2022, aes(area = Pop_millions, label = iso3c)) +
  geom_treemap(fill = "steelblue", color = "white") +
  geom_treemap_text(
    color = "white",
    place = "centre",
    size = 8,
    min.size = 3
  ) +
  labs(title = "Treemap of Population (millions)")


# ---- v-map-1 ----
# library(maps)
# library(countrycode)
# 
# world_map_data <- map_data("world")


# ---- v-map-2 ----
# world_map_data$iso3c <- countrycode(world_map_data$region,
#                                     origin = "country.name",
#                                     destination = "iso3c")


# ---- v-map-3 ----
# world_map_data$continent <- countrycode(world_map_data$iso3c,
#                                         origin = "iso3c",
#                                         destination = "continent")


# ---- v-map-4 ----
# world_map_data_europe <- world_map_data %>%
#   filter(continent == "Europe")


# ---- v-map-load ----
world_map_data_europe <- read.csv("data/europe_map.csv") %>%
  arrange(group, order)


# ---- v-map-join ----
europe_map_data <- left_join(world_map_data_europe, df_2022,
                             by = "iso3c")


# ---- fig-map ----
ggplot(europe_map_data,
       aes(x = long, y = lat, group = group,
           fill = Pop_millions)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  coord_fixed(1.3, xlim = c(-25, 60), ylim = c(34, 72)) +
  scale_fill_gradient(
    name = "Population (millions)",
    low = "white",
    high = "darkblue",
    na.value = "gray90"
  ) +
  labs(title = "Population by Country (millions) - Europe (2022)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )


# ---- fig-hist ----
ggplot(df_2022, aes(x = Pop_millions)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Histogram of Population (millions)",
    x = "Population (millions)",
    y = "Frequency (# of countries)"
  ) +
  theme_minimal()

