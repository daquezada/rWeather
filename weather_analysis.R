# Load packages
library(tidyverse)  # includes dplyr, stringr, readr, ggplot2, tidyr, purrr, tibble, forcats, lubridate
library(conflicted)
library(janitor)    # clean_names()

# Makes conflicts explicit:
conflict_prefer("filter", "dplyr")
conflict_prefer("read_csv", "readr")
conflict_prefer("mutate", "dplyr")

# Import CSV files (adjust file names if needed)
df_weather <- read_csv("weather.csv", show_col_types = FALSE)

# Remove leading/trailing spaces
df_weather <- df_weather %>%
  mutate(Station.State = stringr::str_trim(Station.State))

# Clean column names -> snake_case
df_weather <- df_weather %>%
  clean_names()

# Convert numeric columns from character to numeric
df_weather <- df_weather %>%
  dplyr::mutate(
    dplyr::across(
      c(
        data_precipitation,
        data_temperature_avg_temp,
        data_temperature_max_temp,
        data_temperature_min_temp,
        data_wind_direction,
        data_wind_speed
      ),
      as.numeric
    )
  )

# Convert dates to Date type
df_weather <- df_weather %>%
  dplyr::mutate(date_full = as.Date(date_full))

# Two cities on the SAME plot (comparison lines)
df_multiple_cities <- df_weather %>%
  dplyr::filter(station_city %in% c("Jacksonville", "New York"))

plot_two <- df_multiple_cities %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = date_full,
      y = data_temperature_avg_temp,
      color = station_city
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 1.2) +
  ggplot2::labs(
    title = "Average Temperature Over Time: JAX vs NYC",
    x = "Date",
    y = "Avg Temp (°F)",
    color = "City"
  ) +
  ggplot2::theme_minimal()

print(plot_two)

ggplot2::ggsave("weather_jax_vs_ny.png", plot_two, width = 10, height = 6)