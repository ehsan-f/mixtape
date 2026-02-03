#' Calculate elapsed time between two dates
#'
#' @description
#' Calculates the time between two dates using different accuracy types.
#'
#' @param start_date Start date
#' @param end_date End date
#' @param accuracy_type Type of accuracy calculation ('day' or 'sql') (default: 'day')
#'
#' @importFrom lubridate isoweek as_date
#' @export
elapsed_years <- function(start_date, end_date, accuracy_type = 'day') {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)

  if (accuracy_type == 'day') {
    years <- (ed$year - sd$year)
    years <- ifelse(ed$yday < sd$yday, years - 1, years)
  }

  if (accuracy_type == 'sql') {
    years <- (ed$year - sd$year)
  }

  return(years)

}

#' Calculate elapsed months between two dates
#'
#' @description
#' Calculates the months between two dates using different accuracy types.
#'
#' @param start_date Start date
#' @param end_date End date
#' @param accuracy_type Type of accuracy calculation ('day' or 'sql') (default: 'day')
#'
#' @export
elapsed_months <- function(start_date, end_date, accuracy_type = 'day') {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)

  if (accuracy_type == 'day') {
    months <- (12 * (ed$year - sd$year)) + (ed$mon - sd$mon)
    months <- ifelse(ed$mday < sd$mday, months - 1, months)
  }

  if (accuracy_type == 'sql') {
    months <- (12 * (ed$year - sd$year)) + (ed$mon - sd$mon)
  }

  return(months)

}

#' Calculate elapsed weeks between two dates
#'
#' @description
#' Calculates the weeks between two dates.
#'
#' @param start_date Start date
#' @param end_date End date
#' @param iso Whether to use ISO week calculation (default: FALSE)
#'
#' @export
elapsed_weeks <- function(start_date, end_date, iso = F) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)

  if (iso == T) {
    weeks <- (52 * (ed$year - sd$year)) + (isoweek(ed) - isoweek(sd))
  } else {
    weeks <- floor(as.numeric(as_date(ed) - as_date(sd))/7)
  }

  return(weeks)
}

#' Calculate elapsed days between two dates
#'
#' @description
#' Calculates the days between two dates.
#'
#' @param start_date Start date
#' @param end_date End date
#'
#' @export
elapsed_days <- function(start_date, end_date) {
  ed <- as_date(end_date)
  sd <- as_date(start_date)
  days <- as.numeric(ed - sd)

  return(days)
}
