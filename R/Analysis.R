# --- Packages (let renv handle installs; don't install inside scripts) ---
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)
library(scales)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(quantmod)   # for VIX
library(janitor)

# --- Paths (your exact Windows locations) ---
base_dir <- "C:/Users/BHAVYA SINGH/Desktop/Github/oil-markets-brent-wti"
raw_dir  <- file.path(base_dir, "data", "raw")
fig_dir  <- file.path(base_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# --- Load & tidy data (robust to slight header differences) ---
brent <- read_excel(file.path(raw_dir, "Crude Oil Prices Brent.xlsx")) |>
  clean_names() |>
  rename(
    date        = any_of(c("date","day")),
    price_brent = any_of(c("daily","price","close","brent","price_brent"))
  ) |>
  mutate(date = as.Date(date)) |>
  arrange(date)

wti <- read_excel(file.path(raw_dir, "Crude Oil WTI Futures Historical Data UK.xlsx")) |>
  clean_names() |>
  rename(
    date      = any_of(c("date","day")),
    price_wti = any_of(c("price","close","wti","price_wti")),
    volume    = any_of(c("vol","volume"))
  ) |>
  mutate(date = as.Date(date)) |>
  arrange(date)

oil_daily <- full_join(brent, wti, by = "date") |>
  rename(Date = date) |>
  arrange(Date)

# Weekday filter (locale-safe) + forward-fill + spread
oil_daily_filled <- oil_daily |>
  filter(!wday(Date, label = TRUE) %in% c("Sat","Sun")) |>
  arrange(Date) |>
  mutate(
    Price_Brent = zoo::na.locf(price_brent, na.rm = FALSE),
    Price_WTI   = zoo::na.locf(price_wti,   na.rm = FALSE),
    Spread      = Price_Brent - Price_WTI
  )

custom_colors <- c(Brent = "#2a9d8f", WTI = "#e76f51")

# ===============================
# 1) PRICE CHART
# ===============================
event_labels <- tribble(
  ~Date,             ~Label,                            ~Offset,
  as.Date("2018-05-08"), "US exits Iran nuclear deal",   1.10,
  as.Date("2019-08-01"), "Tariff threat on China",       1.12,
  as.Date("2020-03-13"), "SPR fill order",               1.07,
  as.Date("2020-04-02"), "OPEC+ cut floated",            1.13,
  as.Date("2020-04-12"), "OPEC+ deal agreed",            1.13
)

price_data <- filter(oil_daily_filled, Date >= as.Date("2015-01-01"))

p1 <- ggplot(price_data, aes(x = Date)) +
  geom_line(aes(y = Price_Brent, color = "Brent"), linewidth = 1) +
  geom_line(aes(y = Price_WTI,   color = "WTI"),   linewidth = 1) +
  scale_color_manual(values = custom_colors, guide = guide_legend(title = NULL)) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  geom_vline(data = event_labels, aes(xintercept = as.numeric(Date)), linetype = "dashed") +
  geom_label_repel(
    data = event_labels,
    aes(x = Date, y = max(price_data$Price_Brent, na.rm = TRUE) * Offset, label = Label),
    size = 4, box.padding = 0.3, point.padding = 0.4, segment.color = "grey50"
  ) +
  labs(title = "Crude Oil Prices: Brent vs WTI",
       subtitle = "Selected market-moving events (2015–2020)",
       y = "Price per barrel", x = NULL) +
  theme_economist(base_size = 16) +
  theme(plot.margin = margin(20, 40, 20, 40))

ggsave(file.path(fig_dir, "oil_prices_events.png"), plot = p1,
       width = 14, height = 8, dpi = 300, bg = "white")

# ===============================
# 2) SPREAD CHART
# ===============================
spread_data <- oil_daily_filled |>
  filter(Date >= as.Date("2015-01-01"), Date <= as.Date("2025-12-31"))

p_spread <- ggplot(spread_data, aes(x = Date, y = Spread)) +
  geom_line(color = "#6c757d", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = dollar_format(suffix = "/bbl")) +
  labs(title = "Brent–WTI Spread (2015–2025)",
       subtitle = "Daily price spread in USD per barrel",
       x = "Year", y = "Spread (USD/bbl)") +
  theme_economist(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(20, 40, 20, 40))

ggsave(file.path(fig_dir, "brent_wti_spread_2015_2025.png"), plot = p_spread,
       width = 14, height = 8, dpi = 300, bg = "white")

# ===============================
# 3) 30-DAY ROLLING VOLATILITY (BRENT)
# ===============================
oil_volatility <- oil_daily_filled |>
  filter(Date >= as.Date("2015-01-01")) |>
  arrange(Date) |>
  mutate(
    log_return_brent = c(NA, diff(log(Price_Brent))),
    vol_30d = rollapply(log_return_brent, width = 30, FUN = sd, fill = NA, align = "right")
  )

# Use non-overlapping regimes
regime_periods <- tribble(
  ~start,           ~end,             ~president,
  as.Date("2009-01-20"), as.Date("2017-01-20"), "Obama",
  as.Date("2017-01-21"), as.Date("2021-01-20"), "Trump",
  as.Date("2021-01-21"), as.Date("2025-09-30"), "Biden"
)

assign_regime <- function(d) {
  hit <- regime_periods |> filter(d >= start, d <= end)
  if (nrow(hit) == 1) hit$president else NA_character_
}

oil_volatility <- oil_volatility |>
  mutate(president = vapply(Date, assign_regime, character(1)))

vol_plot <- ggplot(oil_volatility, aes(x = Date, y = vol_30d, color = president)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("Obama"="#1d3557","Trump"="#e76f51","Biden"="#2a9d8f"), na.value = "grey60") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
  labs(title = "30-Day Rolling Volatility of Brent Prices",
       subtitle = "Segmented by U.S. presidential terms (2015–2025)",
       x = "Date", y = "Volatility (std. dev. of log returns)", color = "Regime") +
  theme_economist(base_size = 16) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(20, 40, 20, 40))

ggsave(file.path(fig_dir, "brent_volatility_30d.png"), plot = vol_plot,
       width = 14, height = 8, dpi = 300, bg = "white")


# ===============================
# 4) ROLLING BETA TO VIX & SPREAD (126D)
# ===============================
# Return_WTI already computed above
# Get VIX (wrap in try so script doesn't crash if offline)
try({
  getSymbols("^VIX", src = "yahoo", from = "2010-01-01", auto.assign = TRUE)
  vix_df <- tibble(Date = as.Date(index(VIX)), VIX = as.numeric(Cl(VIX))) |>
    filter(!is.na(VIX))
  
  beta_data <- oil_daily_returns |>
    left_join(vix_df, by = "Date") |>
    arrange(Date) |>
    mutate(
      Spread_Change = c(NA, diff(Spread)),
      VIX_Change    = c(NA, diff(log(VIX)))
    ) |>
    filter(!is.na(VIX_Change) & !is.na(Return_WTI))
  
  # Robust rolling beta (skips windows with NAs)
  rolling_beta <- function(x, y, window = 126) {
    n <- length(x)
    purrr::map_dbl(seq_len(n), function(i) {
      if (i < window) return(NA_real_)
      xw <- x[(i - window + 1):i]; yw <- y[(i - window + 1):i]
      if (anyNA(xw) || anyNA(yw)) return(NA_real_)
      out <- tryCatch(coef(lm(xw ~ yw))[2], error = function(e) NA_real_)
      as.numeric(out)
    })
  }
  
  beta_df <- beta_data |>
    mutate(
      Beta_VIX    = rolling_beta(Return_WTI, VIX_Change),
      Beta_Spread = rolling_beta(Return_WTI, Spread_Change)
    )
  
  p_vix <- ggplot(filter(beta_df, Date >= as.Date("2015-01-01")), aes(x = Date, y = Beta_VIX)) +
    geom_line(color = "darkred", linewidth = 1) +
    labs(title = "Rolling Beta of WTI Returns to VIX Changes",
         subtitle = "126-day rolling window",
         x = NULL, y = "Beta (WTI ~ VIX)") +
    theme_economist(base_size = 16) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(20, 40, 20, 40))
  
  ggsave(file.path(fig_dir, "beta_wti_vix.png"), plot = p_vix,
         width = 14, height = 8, dpi = 300, bg = "white")
  
  p_spreadbeta <- ggplot(filter(beta_df, Date >= as.Date("2015-01-01")), aes(x = Date, y = Beta_Spread)) +
    geom_line(color = "steelblue", linewidth = 1) +
    labs(title = "Rolling Beta of WTI Returns to Brent–WTI Spread Changes",
         subtitle = "126-day rolling window",
         x = NULL, y = "Beta (WTI ~ Spread)") +
    theme_economist(base_size = 16) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.margin = margin(20, 40, 20, 40))
  
  ggsave(file.path(fig_dir, "beta_wti_spread.png"), plot = p_spreadbeta,
         width = 14, height = 8, dpi = 300, bg = "white")
}, silent = TRUE)

# ===============================
# 5) 5-DAY BEFORE/AFTER RETURNS BAR CHART
# ===============================
event_returns <- event_labels |>
  transmute(Label, Event_Date = Date) |>
  rowwise() |>
  mutate(
    Pre_Start  = Event_Date - 5, Pre_End  = Event_Date - 1,
    Post_Start = Event_Date + 1, Post_End = Event_Date + 5,
    Pre_Return_Brent  = (oil_daily_filled |> filter(Date >= Pre_Start  & Date <= Pre_End)  |> summarize(ret = last(Price_Brent)/first(Price_Brent) - 1))$ret,
    Post_Return_Brent = (oil_daily_filled |> filter(Date >= Post_Start & Date <= Post_End) |> summarize(ret = last(Price_Brent)/first(Price_Brent) - 1))$ret,
    Pre_Return_WTI    = (oil_daily_filled |> filter(Date >= Pre_Start  & Date <= Pre_End)  |> summarize(ret = last(Price_WTI)/first(Price_WTI) - 1))$ret,
    Post_Return_WTI   = (oil_daily_filled |> filter(Date >= Post_Start & Date <= Post_End) |> summarize(ret = last(Price_WTI)/first(Price_WTI) - 1))$ret
  ) |>
  ungroup()

event_returns_long <- event_returns |>
  select(Label, Pre_Return_Brent, Post_Return_Brent, Pre_Return_WTI, Post_Return_WTI) |>
  pivot_longer(cols = -Label, names_to = c("Period","Crude"), names_sep = "_Return_") |>
  mutate(
    Period = recode(Period, "Pre" = "Before", "Post" = "After"),
    Crude  = factor(Crude, levels = c("Brent","WTI"))
  )

fill_colors <- c("Before.Brent"="black","After.Brent"="grey60",
                 "Before.WTI"="#1f3b6f","After.WTI"="#87cefa")

p_return <- ggplot(event_returns_long, aes(x = Label, y = value, fill = interaction(Period, Crude))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = fill_colors,
                    labels = c("Before Brent","After Brent","Before WTI","After WTI"), name = "") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "5-Day Return Around Key Events",
       subtitle = "Comparing Brent and WTI performance before & after each event",
       x = "Event", y = "Cumulative return (%)") +
  theme_economist(base_size = 16) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
        plot.margin = margin(20, 50, 20, 50),
        legend.position = "bottom")

ggsave(file.path(fig_dir, "oil_event_returns_5d.png"), plot = p_return,
       width = 18, height = 9, dpi = 300, bg = "white")