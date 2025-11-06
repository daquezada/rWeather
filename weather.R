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

df_weather_filtered <- df_weather %>%
  dplyr::filter(station_state %in% c("Florida", "New York"))

# (optional) quick peek
if (interactive()) {
  View(df_weather)
  View(df_weather_filtered)
}

plot_weather <- df_weather %>%
  dplyr::filter(station_city == "Jacksonville") %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = date_full,
      y = data_temperature_avg_temp
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Jacksonville Avg Temp: 2016–2017") +
  ggplot2::theme_minimal()

print(plot_weather)

# print(plot_weather)
ggplot2::ggsave("jax_temp.png", plot_weather, width = 9, height = 6, dpi = 150)
