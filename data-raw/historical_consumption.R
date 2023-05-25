library(dplyr)
library(lubridate)

historical_consumption <- readr::read_csv("hp_2020.csv") %>%
  filter(id == 31260, month(datetime) >= 2) %>% # 24378 31260 31329 40322
  mutate(
    datetime = with_tz(datetime, "Europe/Amsterdam"),
    power = consumption*12
  ) %>%
  distinct() %>%
  select(datetime, power)

usethis::use_data(historical_consumption, overwrite = TRUE)
