import_metabase_dat = function(my_opts){

  dat_folder = paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years")

  files.to.read = list.files(pattern = "metabase_",
                             path = dat_folder,
                             full.names = T)

  #Read in each metabase csv file and put them all together in a list.
  metabase_data_list = lapply(files.to.read, readr::read_csv, col_types = readr::cols(.default = "c"))

  #Add names to each element (i.e. dataframe) in this list.
  metabase_data_list = stats::setNames(metabase_data_list, 2020:(2020+length(metabase_data_list)-1))

  metabase_dat = metabase_data_list |>
    dplyr::bind_rows(.id = "Year") |>
    dplyr::mutate(Year = as.numeric(Year))

  rm(metabase_data_list)

  # Use a pretty complicated bifurcating function to clean up the shift start and end time fields.
  # The developers corrected timezone application to inspections and the new app that does
  # this was implemented on 2025-04-07. Any record that has a datetime in the Inspection Time
  # column is part of this new tranche of data. This tranche sets all times to UTC. To find
  # Pacific time, use lubridate's function "with_tz", it is awesome!
  metabase_dat_utc_timezone = metabase_dat |>
    dplyr::filter(stringr::str_detect(`Inspection Time`,"^[0-9\\-]{10}")) |>
    dplyr::mutate(start_time_dt = lubridate::ymd_hms(`Start Time`),
                  end_time_dt = lubridate::ymd_hms(`End Time`)) |>
    dplyr::mutate(start_time_pst = lubridate::with_tz(start_time_dt, tzone = "America/Los_Angeles"),
                  end_time_pst = lubridate::with_tz(end_time_dt, tzone = "America/Los_Angeles")) |>
    dplyr::mutate(start_time_pst = as.character(start_time_pst),
                  end_time_pst = as.character(end_time_pst)) |>
    dplyr::select(-c(`Start Time`,`End Time`,start_time_dt,end_time_dt)) |>
    dplyr::rename(`Start Time` = start_time_pst,
                  `End Time` = end_time_pst,
                  Workflow_ID = `Workflow ID`)

  metabase_dat_old_timezones = metabase_dat |>
    dplyr::filter(!stringr::str_detect(`Inspection Time`,"^[0-9\\-]{10}") | is.na(`Inspection Time`))

  metabase_dat_old_timezones = correct_shift_start_and_end_times(metabase_dat_old_timezones)

  # Recombing metabase data, now with datetime fields more or less correct!
  metabase_dat = dplyr::bind_rows(metabase_dat_old_timezones, metabase_dat_utc_timezone)

  return(metabase_dat)
}
