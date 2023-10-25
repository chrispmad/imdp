clean_metabase_datetime_cols = function(dat){
  dat = dat |>
    dplyr::mutate(across(c(`Start Time`,`End Time`,raw_timestamp),
                  \(x) lubridate::as_datetime(stringr::str_replace(x, "T", "-")))) |>
    dplyr::mutate(`Inspection Date` = format(raw_timestamp, "%Y-%m-%d"))

  dat = dat |>
    dplyr::mutate(TimeOfInspection = lubridate::force_tz(raw_timestamp, "America/Los_Angeles")) |>
    dplyr::select(-raw_timestamp)

  return(dat)
}
