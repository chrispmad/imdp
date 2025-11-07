clean_excel_datetime_cols = function(dat, buffer_minutes = 30) {

  # --- Helper function for timezone and shift correction ---
  adjust_times <- function(df, time_col = "TimeOfInspection",
                           start_col = "Shift Start Time",
                           end_col = "Shift End Time",
                           buffer_minutes = 30) {

    # Only proceed if required columns exist
    if (!all(c(time_col, start_col, end_col) %in% names(df))) return(df)

    df <- df %>%
      dplyr::mutate(
        # Convert to datetime
        !!sym(time_col) := lubridate::as_datetime(!!sym(time_col)),
        !!sym(start_col) := lubridate::as_datetime(!!sym(start_col)),
        !!sym(end_col)   := lubridate::as_datetime(!!sym(end_col)),

        # Convert to Vancouver local time, then UTC
        Inspection.Datetime_VAN = lubridate::with_tz(!!sym(time_col), tzone = "America/Vancouver"),
        Inspection.Datetime_UTC = lubridate::with_tz(Inspection.Datetime_VAN, tzone = "UTC"),

        # Check if inspection is within shift ± buffer
        inspection_ok = Inspection.Datetime_UTC >= (!!sym(start_col) - lubridate::minutes(buffer_minutes)) &
                       Inspection.Datetime_UTC <= (!!sym(end_col) + lubridate::minutes(buffer_minutes)),

        # Adjust back 8 hours if outside range
        Inspection.Datetime_VAN = dplyr::if_else(
          !inspection_ok,
          Inspection.Datetime_VAN - lubridate::hours(8),
          Inspection.Datetime_VAN
        ),

        # Recalculate UTC and inspection hour
        Inspection.Datetime_UTC = lubridate::with_tz(Inspection.Datetime_VAN, tzone = "UTC"),
        Inspection.Hour = lubridate::hour(Inspection.Datetime_VAN)
      ) %>%
      dplyr::select(-inspection_ok)

    return(df)
  }

  # --- Original per-year cleaning logic (all preserved) ---
  dat[[1]] <- dat[[1]] %>%
    dplyr::mutate(TimeOfInspection = dplyr::coalesce(`Time of High Risk Inspection`,
                                                     `Timestamp of inspection`,
                                                     `Shift Start Time`)) %>%
    dplyr::mutate(TimeOfInspection = ifelse(stringr::str_detect(TimeOfInspection, "^[0-9]{5}"),
                                            as.character(openxlsx::convertToDateTime(TimeOfInspection)),
                                            TimeOfInspection)) %>%
    dplyr::mutate(TimeOfInspection = stringr::str_replace_all(TimeOfInspection, " ([0-9])(:[0-9]{2})$", " 0\\1\\2:00")) %>%
    dplyr::mutate(TimeOfInspection = stringr::str_replace_all(TimeOfInspection, " ([0-9]{2})(:[0-9]{2})$", " \\1\\2:00")) %>%
    dplyr::mutate(TimeOfInspection = lubridate::ymd_hms(TimeOfInspection)) %>%
    dplyr::mutate(dplyr::across(c(`Time of High Risk Inspection`,`Timestamp of inspection`,
                                  `Shift Start Time`,`Shift End Time`), \(x) lubridate::as_datetime(x))) %>%
    dplyr::mutate(Hour_Min_of_Insp = stringr::str_extract(TimeOfInspection, "(?<= )[0-9]{2}:[0-9]{2}(?=:)"))

  dat[[2]] <- dat[[2]] %>%
    dplyr::mutate(`Timestamp of inspection` = dplyr::case_when(
      stringr::str_extract(`Timestamp of inspection`, "^[0-9]{4}") != "2016" ~ `Shift Start Time`,
      TRUE ~ `Timestamp of inspection`
    )) %>%
    dplyr::mutate(TimeOfInspection = dplyr::coalesce(`Date + Time of High Risk Inspection`,
                                                     `Timestamp of inspection`,
                                                     `Shift Start Time`)) %>%
    dplyr::mutate(TimeOfInspection = ifelse(stringr::str_detect(TimeOfInspection, "^[0-9]{5}"),
                                            as.character(openxlsx::convertToDateTime(TimeOfInspection)),
                                            TimeOfInspection)) %>%
    dplyr::mutate(TimeOfInspection = stringr::str_replace_all(TimeOfInspection, " ([0-9])(:[0-9]{2})$", " 0\\1\\2:00")) %>%
    dplyr::mutate(TimeOfInspection = stringr::str_replace_all(TimeOfInspection, " ([0-9]{2})(:[0-9]{2})$", " \\1\\2:00")) %>%
    dplyr::mutate(TimeOfInspection = lubridate::ymd_hms(TimeOfInspection)) %>%
    dplyr::mutate(dplyr::across(c(`Date + Time of High Risk Inspection`,`Timestamp of inspection`,
                                  `Shift Start Time`,`Shift End Time`), \(x) lubridate::as_datetime(x))) %>%
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`), origin = "1899-12-30")) %>%
    dplyr::filter(lubridate::year(TimeOfInspection) == 2016) %>%
    dplyr::mutate(Hour_Min_of_Insp = stringr::str_extract(TimeOfInspection, "(?<= )[0-9]{2}:[0-9]{2}(?=:)"))

  dat[[3]] <- dat[[3]] %>%
    dplyr::rename(time_of_insp = `Time of inspection`, shift_start = `Shift Start Time`) %>%
    dplyr::mutate(Timestamp = openxlsx::convertToDateTime(dplyr::coalesce(Timestamp, shift_start))) %>%
    dplyr::mutate(time_of_insp = lubridate::hms(time_of_insp)) %>%
    dplyr::mutate(time_of_insp = dplyr::case_when(
      lubridate::hour(time_of_insp) == 0 ~ lubridate::period(paste0("24H ", as.character(time_of_insp))),
      TRUE ~ time_of_insp)) %>%
    dplyr::mutate(Timestamp = dplyr::case_when(
      !is.na(time_of_insp) ~ lubridate::ymd_hms(paste(lubridate::date(Timestamp), time_of_insp)),
      TRUE ~ Timestamp
    )) %>%
    dplyr::rename(TimeOfInspection = Timestamp) %>%
    dplyr::select(-time_of_insp) %>%
    dplyr::mutate(dplyr::across(c(`Date + Time of Inspection`, shift_start, `Shift End Time`),
                                \(x) openxlsx::convertToDateTime(as.numeric(x)))) %>%
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`), origin = "1899-12-30")) %>%
    dplyr::mutate(Hour_Min_of_Insp = stringr::str_extract(TimeOfInspection, "(?<= )[0-9]{2}:[0-9]{2}(?=:)"))

  dat[[4]] <- dat[[4]] %>%
    dplyr::rename(time_of_insp = `Time of Inspection`, shift_start = `Shift Start Time`) %>%
    dplyr::mutate(Timestamp = openxlsx::convertToDateTime(dplyr::coalesce(Timestamp, shift_start))) %>%
    dplyr::mutate(time_of_insp = openxlsx::convertToDateTime(time_of_insp)) %>%
    dplyr::mutate(time_of_insp = lubridate::hms(stringr::str_extract(as.character(time_of_insp), "(?<= ).*"))) %>%
    dplyr::mutate(time_of_insp = dplyr::case_when(
      lubridate::hour(time_of_insp) == 0 ~ lubridate::period(paste0("24H ", as.character(time_of_insp))),
      TRUE ~ time_of_insp)) %>%
    dplyr::mutate(Timestamp = dplyr::case_when(
      !is.na(time_of_insp) ~ lubridate::ymd_hms(paste(lubridate::date(Timestamp), time_of_insp)),
      TRUE ~ Timestamp
    )) %>%
    dplyr::rename(TimeOfInspection = Timestamp) %>%
    dplyr::select(-time_of_insp) %>%
    dplyr::mutate(dplyr::across(c(shift_start, `Shift End Time`),
                                \(x) openxlsx::convertToDateTime(as.numeric(x)))) %>%
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`), origin = "1899-12-30")) %>%
    dplyr::mutate(Hour_Min_of_Insp = stringr::str_extract(TimeOfInspection, "(?<= )[0-9]{2}:[0-9]{2}(?=:)"))

  dat[[5]] <- dat[[5]] %>%
    dplyr::rename(time_of_insp = `Time of Inspection`, shift_start = `Shift Start Time`) %>%
    dplyr::mutate(Timestamp = openxlsx::convertToDateTime(dplyr::coalesce(Timestamp, shift_start))) %>%
    dplyr::mutate(time_of_insp = openxlsx::convertToDateTime(time_of_insp)) %>%
    dplyr::mutate(time_of_insp = lubridate::hms(stringr::str_extract(as.character(time_of_insp), "(?<= ).*"))) %>%
    dplyr::mutate(time_of_insp = dplyr::case_when(
      lubridate::hour(time_of_insp) == 0 ~ lubridate::period(paste0("24H ", as.character(time_of_insp))),
      TRUE ~ time_of_insp)) %>%
    dplyr::mutate(Timestamp = dplyr::case_when(
      !is.na(time_of_insp) ~ lubridate::ymd_hms(paste(lubridate::date(Timestamp), time_of_insp)),
      TRUE ~ Timestamp
    )) %>%
    dplyr::rename(TimeOfInspection = Timestamp) %>%
    dplyr::select(-time_of_insp) %>%
    dplyr::mutate(dplyr::across(c(shift_start, `Shift End Time`),
                                \(x) openxlsx::convertToDateTime(as.numeric(x)))) %>%
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`), origin = "1899-12-30")) %>%
    dplyr::mutate(Hour_Min_of_Insp = stringr::str_extract(TimeOfInspection, "(?<= )[0-9]{2}:[0-9]{2}(?=:)"))

  # --- Apply timezone/shift adjustment to all ---
  dat <- lapply(dat, adjust_times, buffer_minutes = buffer_minutes)

  return(dat)
}