library(esteps)
library(dplyr)
library(testthat)

# Plot example historical consumption
historical_consumption %>%
  esteps:::df_to_xts() %>%
  dygraphs::dygraph() %>%
  dygraphs::dyOptions(useDataTimezone = T)

test_that("the steps are detected", {
  steps_record <- get_steps_record(historical_consumption, device_power = 450, at_least = 15)
  plot_device_consumption(historical_consumption, device_power = 450, at_least = 15)
  expect_true(nrow(steps_record) > 0)
})


