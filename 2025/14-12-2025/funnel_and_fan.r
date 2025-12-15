library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

homicide_data <- 
  read_csv("https://raw.githubusercontent.com/alexlusco/subscript-out-of-bounds/refs/heads/main/2025/14-12-2025/3510006801_databaseLoadingData.csv")

homicide_data <- homicide_data |>
  transmute(
    year   = REF_DATE,
    prov   = GEO,
    indicator = Homicides,
    value = VALUE
  ) |>
  filter(indicator %in% c("Number of homicide victims",
                          "Homicide rates per 100,000 population")) |>
  pivot_wider(
    names_from  = indicator,
    values_from = value,
    names_repair = "minimal"
  ) |>
  rename(
    count = `Number of homicide victims`,
    rate  = `Homicide rates per 100,000 population`
  ) |>
  mutate(
    population = if_else(!is.na(count) & !is.na(rate) & rate > 0,
                         count / rate * 100000,
                         NA_real_)
  )

funnel_df <- homicide_data |>
  filter(
    year == 2024,
    !prov %in% c("Canada", "Northwest Territories including Nunavut"),
    !is.na(count),
    !is.na(population)
  )

# overall national homicide probability
p0 <- sum(funnel_df$count) / sum(funnel_df$population)
national_rate <- p0 * 100000  # convert to per 100,000

# grid of population sizes for the funnel bands
pop_grid <- tibble(
  population = exp(seq(log(min(funnel_df$population)),
                       log(max(funnel_df$population)),
                       length.out = 400))
)

# function to get binomial funnel band (e.g., 95% or 99.9%)
funnel_band <- function(population, p0, alpha = 0.05) {
  mu <- p0 * population
  lower_p <- qbeta(alpha / 2,      mu, population - mu + 1)
  upper_p <- qbeta(1 - alpha / 2,  mu + 1, population - mu)
  tibble(
    population = population,
    lower_rate = lower_p * 100000,
    upper_rate = upper_p * 100000
  )
}

band_95  <- funnel_band(pop_grid$population, p0, alpha = 0.05)
band_999 <- funnel_band(pop_grid$population, p0, alpha = 0.001)

ggplot(funnel_df, aes(x = population, y = rate)) +
  # central national rate line
  geom_hline(yintercept = national_rate, linetype = "solid", colour = "black") +
  # provinces
  geom_point() +
  ggrepel::geom_text_repel(aes(label = prov), size = 3, max.overlaps = 20) +
  # 95% funnel
  geom_line(data = band_95,
            aes(y = lower_rate, x = population), linetype = "dashed") +
  geom_line(data = band_95,
            aes(y = upper_rate, x = population), linetype = "dashed") +
  # 99.9% funnel
  geom_line(data = band_999,
            aes(y = lower_rate, x = population), colour = "red") +
  geom_line(data = band_999,
            aes(y = upper_rate, x = population), colour = "red") +
  scale_x_log10() +
  labs(
    x = "Population (log scale)",
    y = "Homicide rate per 100,000",
    title = "Funnel chart of provincial/territorial homicide rates, 2024"
  ) +
  theme_minimal()




