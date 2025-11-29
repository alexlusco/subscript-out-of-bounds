# Load packages
library(tidyverse)
library(lubridate)

# Load data directly from the TPS CSV URL
uof_raw <- readr::read_csv(
  "https://data.torontopolice.on.ca/datasets/98d88b18c0364c8d86e6a7c690037b85_0.csv"
)

# Keep only the total "Reported Use of Force Incidents" rows
uof_monthly <- uof_raw %>%
  filter(`Data Type` == "Reported Use of Force Incidents") %>%
  mutate(
    # Build a proper date from "Jan", "Feb", etc. and year
    month_date = parse_date_time(
      paste("1", OccurredMonth, OccurredYear),
      orders = "d b Y"   # day short-month year
    )
  ) %>%
  arrange(month_date) %>%
  select(month_date, incidents = `Incident Count`)

# Compute PBC statistics
mean_n <- mean(uof_monthly$incidents, na.rm = TRUE)
sd_n   <- sd(uof_monthly$incidents, na.rm = TRUE)
k      <- 2   # use 2 SD; switch to 3 if you prefer

uof_pbc <- uof_monthly %>%
  mutate(
    cl = mean_n,
    ul = mean_n + k * sd_n,
    ll = pmax(0, mean_n - k * sd_n),  # no negative counts
    signal = case_when(
      incidents > ul ~ "Above upper limit",
      incidents < ll ~ "Below lower limit",
      TRUE           ~ "In control"
    )
  )

# Plot process-behaviour chart
ggplot(uof_pbc, aes(x = month_date, y = incidents)) +
  geom_line() +
  geom_point(aes(color = signal)) +
  geom_hline(aes(yintercept = cl), linetype = "solid") +
  geom_hline(aes(yintercept = ul), linetype = "dashed") +
  geom_hline(aes(yintercept = ll), linetype = "dashed") +
  scale_color_manual(
    values = c(
      "In control"          = "black",
      "Above upper limit"   = "red",
      "Below lower limit"   = "blue"
    )
  ) +
  labs(
    title    = "Toronto Police Service – Reported Use of Force Incidents (Monthly)",
    subtitle = "Process-behaviour chart with ±2 SD limits",
    x        = "Month",
    y        = "Number of incidents",
    color    = "Signal"
  ) +
  theme_minimal()

