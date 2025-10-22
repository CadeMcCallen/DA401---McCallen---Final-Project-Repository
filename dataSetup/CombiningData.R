library(stringr)
library(dplyr)

weather <- read.csv("data/weatherData.csv")
ag <- read.csv("data/agData1.csv")

weather <- weather %>%
  mutate(county = str_to_upper(county))

final <- ag %>%
  left_join(weather, by = c("county", "year"))

final <- final %>%
  mutate(
    missingWeather = if_else(is.na(precip), TRUE, FALSE)
  )
final <- final [!is.na(final$ID), ]
