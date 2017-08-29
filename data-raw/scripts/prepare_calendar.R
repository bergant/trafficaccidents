library(tidyverse)

si_holidays = c(
  "1_1",
  "2_1",
  "8_2",
  "27_4",
  "1_5",
  "2_5",
  "25_6",
  "15_8",
  "31_10",
  "1_11",
  "25_12",
  "26_12"
)

easter_mondays <- as.Date(timeDate::EasterMonday(2004:2016)@Data)

calendar <-
  data_frame(
    date = seq.Date(from = min(event$date), to = max(event$date), by = 1),
    date_POSIXct = as.POSIXct(format(date, "%Y-%m-%d")),
    year = lubridate::year(date),
    month = lubridate::month(date),
    day = lubridate::day(date),
    wday = lubridate::wday(date),
    year_day = lubridate::yday(date)
  ) %>%
  unite(day_month, day, month, remove = FALSE) %>%
  mutate(
    working_day = ! (day_month %in% si_holidays |
                       date %in% easter_mondays |
                       wday %in% c(1, 7)),
    day_month = NULL
  )


calendar$working_day <- factor(
  c("Weekend or holiday", "Working day")[as.integer(calendar$working_day)+1],
  levels = c("Working day", "Weekend or holiday"))



rm(si_holidays, easter_mondays)


