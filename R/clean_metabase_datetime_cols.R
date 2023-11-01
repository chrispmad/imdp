clean_metabase_datetime_cols = function(dat){

  dat = dat |>
    dplyr::mutate(across(c(`Start Time`,`End Time`,raw_timestamp),
                  \(x) lubridate::as_datetime(stringr::str_replace(x, "T", "-")))) |>
    dplyr::mutate(`Inspection Date` = format(raw_timestamp, "%Y-%m-%d"))

  # If the metabase year's data has column 'Inspection Time', do the following:
  if('Inspection Time' %in% names(dat)){
  dat = dat |>
    dplyr::mutate(TimeOfInspection = lubridate::force_tz(raw_timestamp, "America/Los_Angeles")) |>
    # dplyr::select(-raw_timestamp) |>
    dplyr::mutate(hour_min_from_raw_timestamp = stringr::str_extract(TimeOfInspection, '(?<= )[0-9]{2}:[0-9]{2}(?=:)')) |>
    dplyr::mutate(`Inspection Time` = as.character(`Inspection Time`)) |>
    dplyr::mutate(Hour_Min_of_Insp = dplyr::coalesce(`Inspection Time`,
                                                     hour_min_from_raw_timestamp)) |>
    dplyr::select(-raw_timestamp, -hour_min_from_raw_timestamp)
  } else {
    # If not, do the following:
    dat = dat |>
      dplyr::mutate(TimeOfInspection = lubridate::force_tz(raw_timestamp, "America/Los_Angeles")) |>
      # dplyr::select(-raw_timestamp) |>
      dplyr::mutate(hour_min_from_raw_timestamp = stringr::str_extract(TimeOfInspection, '(?<= )[0-9]{2}:[0-9]{2}(?=:)')) |>
      dplyr::mutate(Hour_Min_of_Insp = hour_min_from_raw_timestamp) |>
      dplyr::select(-raw_timestamp, -hour_min_from_raw_timestamp)
  }

  return(dat)
}
