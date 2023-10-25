clean_excel_datetime_cols = function(dat){
  #Quick fix for date/time columns in some of the years.
  dat[[1]] = dat[[1]] |>
    dplyr::mutate(TimeOfInspection = dplyr::coalesce(`Time of High Risk Inspection`,
                                       `Timestamp of inspection`,
                                       `Shift Start Time`)) |>
    #Some date/times are recorded in the excel format still (looks like this: "42577.02391").
    #This line converts that to a readable format (e.g. "2016-07-28 10:53:00)"
    dplyr::mutate(TimeOfInspection = ifelse(stringr::str_detect(TimeOfInspection, "^[0-9]{5}"),
                                     as.character(openxlsx::convertToDateTime(TimeOfInspection)),
                                     TimeOfInspection)) |>
    #Sometimes records are missing the seconds column. These 2 lines add in 00 for seconds.
    dplyr::mutate(TimeOfInspection = stringr::str_replace_all(TimeOfInspection, " ([0-9])(:[0-9]{2})$", " 0\\1\\2:00")) |>
    dplyr::mutate(TimeOfInspection = stringr::str_replace_all(TimeOfInspection, " ([0-9]{2})(:[0-9]{2})$", " \\1\\2:00")) |>
    dplyr::mutate(TimeOfInspection = lubridate::ymd_hms(TimeOfInspection)) |>
    dplyr::mutate(dplyr::across(c(`Time of High Risk Inspection`,`Timestamp of inspection`,
                    `Shift Start Time`,`Shift End Time`), \(x) lubridate::as_datetime(x)))

  dat[[2]] = dat[[2]] |>
    #A few of the timestamps quote 1970 as their year of entry! Crazy. Replace with
    #Shift start time.
    dplyr::mutate(`Timestamp of inspection` = dplyr::case_when(
      stringr::str_extract(`Timestamp of inspection`,"^[0-9]{4}") != "2016" ~ `Shift Start Time`,
      T ~ `Timestamp of inspection`)) |>
    dplyr::mutate(TimeOfInspection = dplyr::coalesce(`Date + Time of High Risk Inspection`,
                                       `Timestamp of inspection`,
                                       `Shift Start Time`)) |>
    #Some date/times are recorded in the excel format still (looks like this: "42577.02391").
    #This line converts that to a readable format (e.g. "2016-07-28 10:53:00)"
    dplyr::mutate(TimeOfInspection = ifelse(stringr::str_detect(TimeOfInspection, "^[0-9]{5}"),
                                     as.character(openxlsx::convertToDateTime(TimeOfInspection)),
                                     TimeOfInspection)) |>
    #Sometimes records are missing the seconds column. These 2 lines add in 00 for seconds.
    dplyr::mutate(TimeOfInspection = stringr::str_replace_all(TimeOfInspection, " ([0-9])(:[0-9]{2})$", " 0\\1\\2:00")) |>
    dplyr::mutate(TimeOfInspection = stringr::str_replace_all(TimeOfInspection, " ([0-9]{2})(:[0-9]{2})$", " \\1\\2:00")) |>
    dplyr::mutate(TimeOfInspection = lubridate::ymd_hms(TimeOfInspection)) |>
    dplyr::mutate(dplyr::across(c(`Date + Time of High Risk Inspection`,`Timestamp of inspection`,
                    `Shift Start Time`,`Shift End Time`), \(x) lubridate::as_datetime(x))) |>
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`),origin = "1899-12-30")) |>
    dplyr::filter(lubridate::year(TimeOfInspection) == 2016)

  #When we have time of inspection (as opposed to just raw_timestamp), use that instead of raw timestamp.
  #We'll need to combine the hour/minute/second info from time of inspection with the date info from raw timestamp.
  dat[[3]] = dat[[3]] |>
    dplyr::rename(time_of_insp = `Time of inspection`,
           shift_start = `Shift Start Time`) |>
    #If the raw timestamp field is blank, coalesce with shift start.
    #Convert data types into date-times and periods.
    dplyr::mutate(Timestamp = openxlsx::convertToDateTime(dplyr::coalesce(Timestamp, shift_start))) |>
    dplyr::mutate(time_of_insp = lubridate::hms(time_of_insp)) |>
    #If the hour field was 24, it got changed to 0 and automatically dropped. Add it back in.
    dplyr::mutate(time_of_insp = dplyr::case_when(
      lubridate::hour(time_of_insp) == 0 ~ lubridate::period(paste0("24H ", as.character(time_of_insp))),
      T ~ time_of_insp)) |>
    #If the Time of Inspection field is totally blank, stick to raw timestamp. Best we can do.
    dplyr::mutate(Timestamp = dplyr::case_when(
      !is.na(time_of_insp) ~ lubridate::ymd_hms(paste(lubridate::date(Timestamp), time_of_insp)),
      T ~ Timestamp
    )) |>
    dplyr::rename(TimeOfInspection = Timestamp) |>
    dplyr::select(-time_of_insp) |>
    dplyr::mutate(dplyr::across(c(`Date + Time of Inspection`,
                    shift_start,`Shift End Time`), \(x) openxlsx::convertToDateTime(as.numeric(x)))) |>
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`),origin = "1899-12-30"))

  dat[[4]] = dat[[4]] |>
    dplyr::rename(time_of_insp = `Time of Inspection`,
           shift_start = `Shift Start Time`) |>
    #If the raw timestamp field is blank, coalesce with shift start.
    #Convert data types into date-times and periods.
    dplyr::mutate(Timestamp = openxlsx::convertToDateTime(dplyr::coalesce(Timestamp, shift_start))) |>
    dplyr::mutate(time_of_insp = openxlsx::convertToDateTime(time_of_insp)) |>
    dplyr::mutate(time_of_insp = lubridate::hms(stringr::str_extract(as.character(time_of_insp), "(?<= ).*"))) |>
    #If the hour field was 24, it got changed to 0 and automatically dropped. Add it back in.
    dplyr::mutate(time_of_insp = dplyr::case_when(
      lubridate::hour(time_of_insp) == 0 ~ lubridate::period(paste0("24H ", as.character(time_of_insp))),
      T ~ time_of_insp)) |>
    #If the Time of Inspection field is totally blank, stick to raw timestamp. Best we can do.
    dplyr::mutate(Timestamp = dplyr::case_when(
      !is.na(time_of_insp) ~ lubridate::ymd_hms(paste(lubridate::date(Timestamp), time_of_insp)),
      T ~ Timestamp
    )) |>
    dplyr::rename(TimeOfInspection = Timestamp) |>
    dplyr::select(-time_of_insp) |>
    dplyr::mutate(dplyr::across(c(shift_start,`Shift End Time`), \(x) openxlsx::convertToDateTime(as.numeric(x)))) |>
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`),origin = "1899-12-30"))

  dat[[5]] = dat[[5]] |>
    dplyr::rename(time_of_insp = `Time of Inspection`,
           shift_start = `Shift Start Time`) |>
    #If the raw timestamp field is blank, coalesce with shift start.
    #Convert data types into date-times and periods.
    dplyr::mutate(Timestamp = openxlsx::convertToDateTime(dplyr::coalesce(Timestamp, shift_start))) |>
    dplyr::mutate(time_of_insp = openxlsx::convertToDateTime(time_of_insp)) |>
    dplyr::mutate(time_of_insp = lubridate::hms(stringr::str_extract(as.character(time_of_insp), "(?<= ).*"))) |>
    #If the hour field was 24, it got changed to 0 and automatically dropped. Add it back in.
    dplyr::mutate(time_of_insp = dplyr::case_when(
      lubridate::hour(time_of_insp) == 0 ~ lubridate::period(paste0("24H ", as.character(time_of_insp))),
      T ~ time_of_insp)) |>
    #If the Time of Inspection field is totally blank, stick to raw timestamp. Best we can do.
    dplyr::mutate(Timestamp = dplyr::case_when(
      !is.na(time_of_insp) ~ lubridate::ymd_hms(paste(lubridate::date(Timestamp), time_of_insp)),
      T ~ Timestamp
    )) |>
    dplyr::rename(TimeOfInspection = Timestamp) |>
    dplyr::select(-time_of_insp) |>
    dplyr::mutate(dplyr::across(c(shift_start,`Shift End Time`), \(x) openxlsx::convertToDateTime(as.numeric(x)))) |>
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`),origin = "1899-12-30"))

  return(dat)
}
