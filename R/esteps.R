
# Main functions -----------------------------------------------------------

#' Detect steps of constant power
#'
#' @param historical_consumption A tibble with `datetime` and `power` columns
#' @param device_power A number corresponding to device constant power
#'
#' @return A tibble with the activation and deactivation time of all detected steps
#' @keywords internal
#'
#' @importFrom dplyr %>% mutate filter lag
#' @importFrom tidyr drop_na
#' @importFrom rlang .data
#'
get_steps <- function(historical_consumption, device_power){
  historical_consumption %>%
    mutate(
      active_period = .data$power > device_power,
      activation = .data$active_period & !lag(.data$active_period),
      deactivation = !.data$active_period & lag(.data$active_period)
    ) %>%
    drop_na()
}


#' Summarize all events detected
#'
#' @param historical_consumption A tibble with `datetime` and `power` columns
#' @param device_power A number corresponding to device constant power
#' @param at_least Minimum of minutes to consider the step
#'
#' @return A tibble of events record, composed by:
#'     activation datetime
#'     deactivation datetime
#'     total consumption within this period
#'     device consumption whithin this period
#' @export
#' @importFrom dplyr tibble %>%  mutate filter
#'
get_steps_record <- function(historical_consumption, device_power, at_least) {
  steps <- get_steps(historical_consumption, device_power)
  activation <- steps$datetime[steps$activation]
  deactivation <- steps$datetime[steps$deactivation]
  if (length(activation) != length(deactivation)) {
    if (length(activation) < length(deactivation)) {
      deactivation <- deactivation[1:length(activation)]
    } else {
      activation <- activation[1:length(deactivation)]
    }
  }
  tibble(
    activation = activation,
    deactivation = deactivation) %>%
    mutate(
      duration = as.numeric(difftime(deactivation, activation, units='mins')),
      kWh = device_power/1000*.data$duration/60
    ) %>%
    filter(.data$duration > at_least)
}


#' Average of energy consumed per activation period
#'
#' @param steps_record A tibble of events record returned by `get_steps_record`
#'
#' @return A value. Energy (Wh)
#'
get_average_consumption <- function(steps_record) {
  stats::median(unlist(steps_record$consumed))
}


#' Average duration (minutes) of device activation period
#'
#' @param steps_record A tibble of events record returned by `get_steps_record`
#'
#' @return Mean of duration values (minutes).
#'
get_average_duration <- function(steps_record) {
  mean(unlist(steps_record$duration))
}


#' Number of sessions detected
#'
#' @param steps_record A tibble of events record returned by `get_steps_record`
#'
#' @return A value
#'
get_number_sessions <- function(steps_record) {
  nrow(steps_record)
}


#' Average session starting time
#'
#' @param steps_record A tibble of events record returned by `get_steps_record`
#'
#' @return A value
#'
get_average_start_time <- function(steps_record) {
  stats::median(lubridate::hour(steps_record$activation))
}


#' Summary of device sessions indicators
#'
#' @param historical_consumption A tibble with `datetime` and `power` columns
#' @param device_power A number corresponding to device constant power
#' @param at_least Minimum of minutes to consider the step
#'
#' @return A tibble with the following indicators:
#'
#' - Average consumption per session (Wh)
#'
#' - Average duration of a session (hours)
#'
#' - Average sessions starting time
#'
#' - Number of sessions in the time studied
#'
#' @export
#'
#' @importFrom dplyr tibble
#'
summarise_steps <- function(historical_consumption, device_power, at_least = 60){
  steps_record <- get_steps_record(historical_consumption, device_power, at_least)
  tibble(
    consumption = get_average_consumption(steps_record),
    duration = get_average_duration(steps_record),
    start_time = get_average_start_time(steps_record),
    n_sessions = get_number_sessions(steps_record)
  )
}


#' Dygrpahs plot object of device cosumption over total consumption
#'
#' @param historical_consumption A tibble with `datetime` and `power` columns
#' @param device_power A number corresponding to device constant power
#' @param at_least Minimum of minutes to consider the step
#'
#' @return tibble with device's consumption profile
#' @export
#'
#' @importFrom dplyr mutate %>% select
#'
get_device_profile <- function(historical_consumption, device_power, at_least = 60) {
  steps <- get_steps_record(historical_consumption, device_power, at_least)
  consumption_df <- historical_consumption %>%
    mutate(
      device_consumption = 0
    ) %>%
    select("datetime", "power", "device_consumption")
  if (nrow(steps) > 0) {
    for (s in 1:nrow(steps)) {
      consumption_df$device_consumption[
        consumption_df$datetime >= steps$activation[s] & consumption_df$datetime < steps$deactivation[s]
        ] <- device_power
    }
  }
  return(consumption_df)

}


#' Dygrpahs plot object of device cosumption over total consumption
#'
#' @param historical_consumption A tibble with `datetime` and `power` columns
#' @param device_power A number corresponding to device constant power
#' @param at_least Minimum of minutes to consider the step
#'
#' @return Dygraphs plot object
#' @export
#'
plot_device_consumption <- function(historical_consumption, device_power, at_least = 60) {
  get_device_profile(historical_consumption, device_power, at_least) %>%
    df_to_xts() %>%
    dygraphs::dygraph() %>%
    dygraphs::dySeries("power", fillGraph = TRUE, color = "green") %>%
    dygraphs::dySeries("device_consumption", fillGraph = TRUE, stepPlot = TRUE, color = "blue") %>%
    dygraphs::dyOptions(useDataTimezone = T)
}


#' Histogram of sessions starting hours
#'
#' @param steps_record Summary from device events summary function
#'
#' @return Histogram plot of steps starting hour
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_histogram labs
#' @importFrom rlang .data
#'
plot_start_time_histogram <- function(steps_record) {
  start_time_df <- data.frame(start_time = lubridate::hour(steps_record$activation))
  ggplot(start_time_df, aes(.data$start_time)) +
    geom_histogram(binwidth = 1, color = "black", fill = "grey") +
    labs(x = "Hour of the day", y = "Count", title = "Steps start hour")
}


#' Histogram of sessions duration
#'
#' @param steps_record Summary from device events summary function
#'
#' @return Histogram plot of steps starting hour
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_histogram labs
#' @importFrom rlang .data
#'
plot_session_duration_histogram <- function(steps_record) {
  duration_df <- data.frame(duration = steps_record$duration/60)
  ggplot(duration_df, aes(.data$duration)) +
    geom_histogram(binwidth = 1, color = "black", fill = "grey") +
    labs(x = "Hour of the day", y = "Count", title = "Steps duration")
}


# Subfunctions ---------------------------------------------------------------

# Convert a tibble to a timeseries vector
df_to_xts <- function(df) {
  new_ts <- xts::xts(x = df[, 2:(length(colnames(df)))], order.by = df[[1]])
  if (is.null(colnames(new_ts))) colnames(new_ts) <- colnames(df[2])
  cbind(new_ts)
}

