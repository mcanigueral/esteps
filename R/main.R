
# Main functions -----------------------------------------------------------

#' Summarize all events detected
#'
#' @param historical_consumption A dataframe with datetime and consumption columns
#' @param device_power A number corresponding to device constant power
#'
#' @return A dataframe of events record, composed by:
#'     activation datetime
#'     deactivation datetime
#'     total consumption within this period
#'     device consumption whithin this period
#' @export
#' @import dplyr
#'
steps_record <- function(historical_consumption, device_power) {
  steps <- get_steps(historical_consumption, device_power)
  activation <- steps$datetime[steps$activation]
  deactivation <- steps$datetime[steps$deactivation]
  if (length(activation) < length(deactivation)) {
    deactivation <- deactivation[1:length(activation)]
  }
  else if (length(activation) > length(deactivation)) {
    activation <- activation[1:length(deactivation)]
  }
  tibble(
    activation = activation,
    deactivation = deactivation) %>%
    mutate(consumption = purrr::map2(activation, deactivation, function(x, y) {
      sum(steps$consumption[(steps$datetime > x) & (steps$datetime < y)])
    })) %>%
    mutate(duration = as.numeric(difftime(deactivation, activation, units='hour')),
           consumed = device_power*duration) %>%
    filter(duration > 1) # At least 1 hour of consumption
}


#' Average of energy consumed per activation period
#'
#' @param steps_record A dataframe of events record
#'
#' @return A value. Energy (Wh)
#'
average_consumption <- function(steps_record) {
  stats::median(unlist(steps_record$consumed))
}


#' Average duration (hours) of device activation period
#'
#' @param steps_record A dataframe of events record
#'
#' @return A value. Duration (hours).
#'
average_duration <- function(steps_record) {
  stats::median(unlist(steps_record$duration))
}


#' Number of sessions detected
#'
#' @param steps_record A dataframe of events record
#'
#' @return A value
#'
number_sessions <- function(steps_record) {
  nrow(steps_record)
}


#' Average session starting time
#'
#' @param steps_record A dataframe of events record
#'
#' @return A value
#'
average_start_time <- function(steps_record) {
  stats::median(lubridate::hour(steps_record$activation))
}


#' Summary of device sessions indicators
#'
#' @param historical_consumption A dataframe with datetime and consumption columns
#' @param device_power A number corresponding to device constant power (W)
#'
#' @return A dataframe with the following indicators:
#' - Average consumption per session (Wh)
#' - Average duration of a session (hours)
#' - Average sessions starting time
#' - Number of sessions in the time studied
#' @export
#'
steps_summary <- function(historical_consumption, device_power){
  steps_record <- steps_record(historical_consumption, device_power)
  tibble(
    consumption = average_consumption(steps_record),
    duration = average_duration(steps_record),
    start_time = average_start_time(steps_record),
    n_sessions = number_sessions(steps_record)
  )
}


#' Dygrpahs plot object of device cosumption over total consumption
#'
#' @param user_name Prosumer name
#' @param historical_consumption A dataframe with datetime and consumption columns
#' @param device_power A number corresponding to device constant power (W)
#'
#' @return Dygraphs plot object
#' @export
#'

device_consumption_plot <- function(historical_consumption, device_power) {
  steps_record <- steps_record(historical_consumption, device_power)
  dt_interval_factor <- 60/as.numeric(difftime(historical_consumption$datetime[2], historical_consumption$datetime[1], units='mins'))
  power_consumption <- dplyr::mutate(historical_consumption, power_consumption = consumption*dt_interval_factor)
  power_consumption <- dplyr::select(power_consumption, datetime, power_consumption)
  if (nrow(steps_record) > 0) {
    device_consumption <- data.frame(datetime = c(steps_record$activation, steps_record$deactivation),
                                     device_consumption = c(rep(device_power, nrow(steps_record)), rep(0, nrow(steps_record))))
    consumption_to_plot <- xts::merge.xts(df_to_xts(power_consumption), df_to_xts(device_consumption))
    consumption_to_plot <- zoo::na.locf(consumption_to_plot) # Fill dates between activation/deactivation with last same value (steps)
    dyplot <- dygraphs::dygraph(consumption_to_plot)
    dyplot <- dygraphs::dySeries(dyplot, "power_consumption", fillGraph = TRUE, color = "green")
    dyplot <- dygraphs::dySeries(dyplot, "device_consumption", fillGraph = TRUE, stepPlot = TRUE, color = "blue")
  } else {
    dyplot <- dygraphs::dygraph(df_to_xts(power_consumption))
    dyplot <- dygraphs::dySeries(dyplot, "power_consumption", fillGraph = TRUE, color = "green")
  }
  # dyplot <- dygraphs::dyRangeSelector(dyplot, height = 50)
  dyplot
}


#' Histogram of sessions starting hours
#'
#' @param user_name Prosumer name
#' @param steps_record Summary from device events summary function
#'
#' @return Histogram plot of steps starting hour
#' @export
#'
start_time_histogram <- function(user_name, steps_record) {
  start_time_df <- data.frame(start_time = lubridate::hour(steps_record$activation))
  ggplot2::qplot(start_time,
                 data = start_time_df,
                 main=paste("Counts of sessions' starting time for", user_name, sep=" "),
                 geom="histogram",
                 binwidth=1,
                 xlab="Hour of the day",
                 color=I("black"),
                 fill=I("grey"))
}


#' Histogram of sessions duration
#'
#' @param user_name Prosumer name
#' @param steps_record Summary from device events summary function
#'
#' @return Histogram plot of steps starting hour
#' @export
#'
session_duration_histogram <- function(user_name, steps_record) {
  duration_df <- data.frame(duration = steps_record$duration)
  ggplot2::qplot(duration,
                 data = duration_df,
                 main=paste("Counts of sessions duration for", user_name, sep=" "),
                 geom="histogram",
                 binwidth=0.25,
                 xlab="Number of hours",
                 color=I("black"),
                 fill=I("grey"))
}


# Subfunctions ---------------------------------------------------------------

# Convert a dataframe to a timeseries vector

df_to_xts <- function(df) {
  new_ts <- xts::xts(x = df[, 2:(length(colnames(df)))], order.by = df[[1]])
  if (is.null(colnames(new_ts))) colnames(new_ts) <- colnames(df[2])
  cbind(new_ts)
}


# Detect steps in historical consumption dataframe according a predefined power

get_steps <- function(historical_consumption, device_power){
  dt_interval_factor = 60/as.numeric(difftime(historical_consumption$datetime[2], historical_consumption$datetime[1], units='mins'))
  historical_consumption %>%
    mutate(power_consumption = consumption*dt_interval_factor) %>%
    mutate(active_period = (power_consumption > device_power) &
             (lead(power_consumption) > device_power) &
             (lead(power_consumption,2) > device_power)
    ) %>%
    mutate(activation = active_period & !lag(active_period),
           deactivation = active_period & !lead(active_period)
    ) %>%
    mutate(device_consumption = active_period * device_power)
}


