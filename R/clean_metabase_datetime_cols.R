clean_metabase_datetime_cols = function(dat){

  dat = dat |>
    dplyr::mutate(across(c(`Start Time`,`End Time`,raw_timestamp),
                  \(x) lubridate::as_datetime(stringr::str_replace(x, "T", "-")))) |>
    dplyr::mutate(`Inspection Date` = format(raw_timestamp, "%Y-%m-%d"))

  # If the metabase data has the newer (i.e. as of 2025-04-07) datetime code
  # that keeps everything in UTC, don't do anything really!
  dat_cleaned = dat |>
    dplyr::filter(stringr::str_detect(`Inspection Time`,"^[0-9]{4}-[0-9]{2}-[0-9]{2}"))

  dat_to_clean = dat |>
    dplyr::filter(!stringr::str_detect(`Inspection Time`,"^[0-9]{4}-[0-9]{2}-[0-9]{2}") | is.na(`Inspection Time`))

  # If the metabase data has the old, defunct, buggy datetime code and thus
  # needs fixing by hand, do the the following:
  # If the metabase year's data has column 'Inspection Time', do the following:
  if('Inspection Time' %in% names(dat_to_clean)){
    dat_to_clean = dat_to_clean |>
    dplyr::mutate(TimeOfInspection = lubridate::force_tz(raw_timestamp, "America/Los_Angeles")) |>
    # dplyr::select(-raw_timestamp) |>
    dplyr::mutate(hour_min_from_raw_timestamp = stringr::str_extract(TimeOfInspection, '(?<= )[0-9]{2}:[0-9]{2}(?=:)')) |>
    dplyr::mutate(`Inspection Time` = as.character(`Inspection Time`)) |>
    dplyr::mutate(Hour_Min_of_Insp = dplyr::coalesce(`Inspection Time`,
                                                     hour_min_from_raw_timestamp)) |>
    dplyr::select(-raw_timestamp, -hour_min_from_raw_timestamp)
  } else {
    # If not, do the following:
    dat_to_clean = dat_to_clean |>
      dplyr::mutate(TimeOfInspection = lubridate::force_tz(raw_timestamp, "America/Los_Angeles")) |>
      # dplyr::select(-raw_timestamp) |>
      dplyr::mutate(hour_min_from_raw_timestamp = stringr::str_extract(TimeOfInspection, '(?<= )[0-9]{2}:[0-9]{2}(?=:)')) |>
      dplyr::mutate(Hour_Min_of_Insp = hour_min_from_raw_timestamp) |>
      dplyr::select(-raw_timestamp, -hour_min_from_raw_timestamp)
  }

  # Recombine data.
  dat = dplyr::bind_rows(dat_to_clean, dat_cleaned)

  return(dat)
}
