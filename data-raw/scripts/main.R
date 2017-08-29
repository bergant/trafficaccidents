source("data-raw/scripts/prepare_events.R", encoding = "UTF-8")
source("data-raw/scripts/prepare_party.R", encoding = "UTF-8")
source("data-raw/scripts/prepare_party_2016.R", encoding = "UTF-8")
source("data-raw/scripts/prepare_calendar.R", encoding = "UTF-8")

devtools::use_data(event, overwrite = TRUE)
devtools::use_data(party, overwrite = TRUE)
devtools::use_data(calendar, overwrite = TRUE)


if(FALSE) {
  # generate roxygen item comments for data frames
  library(datamodelr)
  dm <-
    dm_from_data_frames(event, party, calendar) %>%
    dm_add_references(
      party$src_file == event$src_file,
      party$event_id == event$id,
      event$date == calendar$date
    )

  dm$columns$column %>%  split(dm$columns$table) %>%
    map(~sprintf("#' \\item{%s}{}", .)) %>%
    map(~paste(., collapse = "\n")) %>%
    map(~cat("\n",., sep = "\n"))
}



