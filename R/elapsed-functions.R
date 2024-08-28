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

elapsed_weeks <- function(start_date, end_date, iso = F) {
  library(lubridate)

  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)

  if (iso == T) {
    weeks <- (52 * (ed$year - sd$year)) + (isoweek(ed) - isoweek(sd))
  } else {
    weeks <- floor(as.numeric(as_date(ed) - as_date(sd))/7)
  }
}

elapsed_days <- function(start_date, end_date) {
  library(lubridate)

  ed <- as_date(end_date)
  sd <- as_date(start_date)
  days <- as.numeric(ed - sd)
  days
}
