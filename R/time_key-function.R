#' @export
time_key <- function(df = ds,
                     x,
                     start = '2019-01-01',
                     end = Sys.Date(),
                     end_of_week = 'Saturday',
                     week_days = 7,
                     weekend_days = c('Friday', 'Saturday'),
                     group_year = F) {

  #-- Packages
  library(lubridate)
  library(dplyr)

  #-- Variables
  d <- seq(as_date(start), as_date(end), by = 'days')
  ds_time_key <- as_tibble(d) %>% rename('date' = 'value')
  ds_time_key$month <- substr(ds_time_key$date, 1, 7)
  ds_time_key$weekday <- weekdays(ds_time_key$date)
  ds_time_key$calendar_week_no <- epiweek(ds_time_key$date)
  ds_time_key$weekend <- if_else(weekdays(ds_time_key$date) %in% weekend_days, 1, 0)
  ds_time_key$year <- year(ds_time_key$date) %>% as.character()

  #----- Month and day numbers
  ds_time_key <- ds_time_key %>%
    full_join(
      bind_cols(month = unique(ds_time_key$month),
                month_no = seq_along(unique(ds_time_key$month))),
      by = 'month'
    ) %>%
    full_join(
      bind_cols(date = unique(ds_time_key$date),
                day_no = seq_along(unique(ds_time_key$date))),
      by = 'date'
    )

  #----- Week numbers
  ds_time_key$week_no <- 0

  end_week_one <- ds_time_key %>%
    filter(weekday == end_of_week) %>%
    slice(1)
  end_week_one <- end_week_one$day_no

  t <- seq.int(ds_time_key$day_no[end_week_one], ds_time_key$day_no[nrow(ds_time_key)], by = week_days)

  #-- Week 1
  ds_time_key <- ds_time_key %>%
    mutate(
      week_no = ifelse(day_no <= end_week_one, 1, week_no)
    )

  #-- Rest of the weeks
  for (i in 2:length(t)) {
    ds_time_key$week_no[ds_time_key$day_no > t[i-1] & ds_time_key$day_no <= t[i]] <- seq_along(t)[i]
  }

  #-- Latest Week
  ds_time_key <- ds_time_key %>%
    mutate(
      week_no = ifelse(week_no == 0, max(week_no) + 1, week_no)
    )

  #-- year
  if (group_year == T) {
    ds_time_key <- ds_time_key %>%
      mutate(
        year = ifelse(month < '2021-06',
                      '<= 2021-H1',
                      ifelse(month >= '2021-06' & year == '2021',
                             '2021-H2',
                             year))
      )
  }

  #----- Bring Together
  ds_time_key <- ds_time_key %>%
    group_by(week_no) %>%
    mutate(
      # min = min(date),
      # max = max(date),
      # week_date = paste(min, '-', max),
      week_start_date = min(date)
    ) %>%
    mutate() %>%
    rename(day = 'date') %>%
    select(day, day_no, week_start_date, week_no, calendar_week_no, month, month_no, year, weekend)

  #----- Output
  if (is.null(df)) {
    df <- ds_time_key %>% as_tibble()
  } else {
    #-- Join to data
    df <- as.data.frame(df)
    df$day <- as_date(df[,x])

    df <- df %>%
      left_join(ds_time_key, by = 'day') %>%
      as_tibble()
  }

}
