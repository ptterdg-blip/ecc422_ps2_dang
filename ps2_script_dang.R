library(tidyquant)
library(tidyverse)

#unemp_raw <- tq_get(
#  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#  get  = "economic.data",
#  from = "1976-01-01"
#)

#head(unemp_raw)

#dir.create("data", showWarnings = FALSE)

#write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR" ~ "Oregon",
      symbol == "CAUR" ~ "California",
      symbol == "TXUR" ~ "Texas",
      symbol == "NYUR" ~ "New York"
    )
  )

unemp <- unemp |> 
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |> 
  ungroup()
# Month-over-month change within each region
unemp <- unemp |> 
  arrange(region, date) |> 
  group_by(region) |> 
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |> 
  ungroup()
# Keep only January 2015 and onward.
unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

# Full-sample summary by region
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

# Same summary for the recent sample
unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_top_increases <- unemp |> 
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases <- unemp |> 
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)
    
unemp_no_covid <- unemp |> 
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |> 
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE)

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE)

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() + 
  labs(title = "Monthly Unemployment rate, 1976 - Present",
       x = "Date",
       y = "Unemployment Rate (%)")
+ theme_minimum()

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(
    title = "Monthly Unemployment Rate by Region, 1976 - Present",
    x = "Date",
    y = "Unemployment Rate (%):"
  ) +
  theme_minimal()

ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Unemployment Rate by Region, 1976–Present",
    x = "Region",
    y = "Mean Unemployment Rate (%)"
  ) +
  theme_minimal()

ggplot(unemp, aes(x = fct_reorder(region, unemployment_rate, .fun = median),
                  y = unemployment_rate,
                  fill = region)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Distribution of Monthly Unemployment Rates by Region, 1976–Present",
    x = "Region",
    y = "Unemployment Rate (%)"
  ) +
  theme_minimal()
# I made a box plot to compare the full distribution of unemployment rates across regions, not just the 
# averages in 5.3. It shows which states have higher typical unemployment and which have more variable labor markets.
