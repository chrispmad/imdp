clean_excel_datetime_cols <- function(dat) {

  # --- Helper: safely convert mixed Excel/character date-time values ---
  safe_excel_to_datetime <- function(x) {
    suppressWarnings({
      dplyr::case_when(
        # Numeric or Excel-encoded value
        stringr::str_detect(x, "^[0-9.]+$") ~
          as.character(openxlsx::convertToDateTime(as.numeric(x))),
        # Already ISO or standard datetime (YYYY-MM-DD HH:MM[:SS])
        stringr::str_detect(x, "^[0-9]{4}-[0-9]{2}-[0-9]{2}") ~ x,
        # Time-only (HH:MM or HH:MM:SS) → attach today's date placeholder
        stringr::str_detect(x, "^[0-9]{1,2}:[0-9]{2}(:[0-9]{2})?$") ~
          paste(Sys.Date(), x),
        TRUE ~ NA_character_
      )
    })
  }

  # ==============================================================
  # Sheet 1
  # ==============================================================
  dat[[1]] <- dat[[1]] |>
    dplyr::mutate(TimeOfInspection = dplyr::coalesce(`Time of High Risk Inspection`,
                                                     `Timestamp of inspection`,
                                                     `Shift Start Time`)) |>
    dplyr::mutate(TimeOfInspection = safe_excel_to_datetime(TimeOfInspection)) |>
    dplyr::mutate(TimeOfInspection = lubridate::ymd_hms(TimeOfInspection, quiet = TRUE)) |>
    dplyr::mutate(dplyr::across(c(`Time of High Risk Inspection`,`Timestamp of inspection`,
                                  `Shift Start Time`,`Shift End Time`),
                                \(x) lubridate::as_datetime(safe_excel_to_datetime(as.character(x))))) |>
    dplyr::mutate(Hour_Min_of_Insp = stringr::str_extract(TimeOfInspection, "(?<= )[0-9]{2}:[0-9]{2}(?=:)"))

  # ==============================================================
  # Sheet 2
  # ==============================================================
  dat[[2]] <- dat[[2]] |>
    dplyr::mutate(`Timestamp of inspection` = dplyr::case_when(
      stringr::str_extract(`Timestamp of inspection`, "^[0-9]{4}") != "2016" ~ `Shift Start Time`,
      TRUE ~ `Timestamp of inspection`)
    ) |>
    dplyr::mutate(TimeOfInspection = dplyr::coalesce(`Date + Time of High Risk Inspection`,
                                                     `Timestamp of inspection`,
                                                     `Shift Start Time`)) |>
    dplyr::mutate(TimeOfInspection = safe_excel_to_datetime(TimeOfInspection)) |>
    dplyr::mutate(TimeOfInspection = lubridate::ymd_hms(TimeOfInspection, quiet = TRUE)) |>
    dplyr::mutate(dplyr::across(c(`Date + Time of High Risk Inspection`,`Timestamp of inspection`,
                                  `Shift Start Time`,`Shift End Time`),
                                \(x) lubridate::as_datetime(safe_excel_to_datetime(as.character(x))))) |>
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`), origin = "1899-12-30")) |>
    dplyr::filter(lubridate::year(TimeOfInspection) == 2016) |>
    dplyr::mutate(Hour_Min_of_Insp = stringr::str_extract(TimeOfInspection, "(?<= )[0-9]{2}:[0-9]{2}(?=:)"))

  # ==============================================================
  # Sheet 3
  # ==============================================================
  dat[[3]] <- dat[[3]] |>
    dplyr::rename(time_of_insp = `Time of inspection`,
                  shift_start = `Shift Start Time`) |>
    dplyr::mutate(Timestamp = openxlsx::convertToDateTime(dplyr::coalesce(Timestamp, shift_start))) |>
    dplyr::mutate(time_of_insp = safe_excel_to_datetime(as.character(time_of_insp))) |>
    dplyr::mutate(time_of_insp = lubridate::hms(stringr::str_extract(time_of_insp, "(?<= )[0-9]{2}:[0-9]{2}(:[0-9]{2})?"))) |>
    dplyr::mutate(time_of_insp = dplyr::case_when(
      lubridate::hour(time_of_insp) == 0 ~ lubridate::period(paste0("24H ", as.character(time_of_insp))),
      TRUE ~ time_of_insp)) |>
    dplyr::mutate(Timestamp = dplyr::case_when(
      !is.na(time_of_insp) ~ lubridate::ymd_hms(paste(lubridate::date(Timestamp), time_of_insp)),
      TRUE ~ Timestamp)) |>
    dplyr::rename(TimeOfInspection = Timestamp) |>
    dplyr::select(-time_of_insp) |>
    dplyr::mutate(dplyr::across(c(`Date + Time of Inspection`, shift_start, `Shift End Time`),
                                \(x) openxlsx::convertToDateTime(as.numeric(x)))) |>
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`), origin = "1899-12-30")) |>
    dplyr::mutate(Hour_Min_of_Insp = stringr::str_extract(TimeOfInspection, "(?<= )[0-9]{2}:[0-9]{2}(?=:)"))

  # ==============================================================
  # Sheet 4
  # ==============================================================
  dat[[4]] <- dat[[4]] |>
    dplyr::rename(time_of_insp = `Time of Inspection`,
                  shift_start = `Shift Start Time`) |>
    dplyr::mutate(Timestamp = openxlsx::convertToDateTime(dplyr::coalesce(Timestamp, shift_start))) |>
    dplyr::mutate(time_of_insp = safe_excel_to_datetime(as.character(time_of_insp))) |>
    dplyr::mutate(time_of_insp = lubridate::hms(stringr::str_extract(time_of_insp, "(?<= )[0-9]{2}:[0-9]{2}(:[0-9]{2})?"))) |>
    dplyr::mutate(time_of_insp = dplyr::case_when(
      lubridate::hour(time_of_insp) == 0 ~ lubridate::period(paste0("24H ", as.character(time_of_insp))),
      TRUE ~ time_of_insp)) |>
    dplyr::mutate(Timestamp = dplyr::case_when(
      !is.na(time_of_insp) ~ lubridate::ymd_hms(paste(lubridate::date(Timestamp), time_of_insp)),
      TRUE ~ Timestamp)) |>
    dplyr::rename(TimeOfInspection = Timestamp) |>
    dplyr::select(-time_of_insp) |>
    dplyr::mutate(dplyr::across(c(shift_start, `Shift End Time`),
                                \(x) openxlsx::convertToDateTime(as.numeric(x)))) |>
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`), origin = "1899-12-30")) |>
    dplyr::mutate(Hour_Min_of_Insp = stringr::str_extract(TimeOfInspection, "(?<= )[0-9]{2}:[0-9]{2}(?=:)"))

  # ==============================================================
  # Sheet 5
  # ==============================================================
  dat[[5]] <- dat[[5]] |>
    dplyr::rename(time_of_insp = `Time of Inspection`,
                  shift_start = `Shift Start Time`) |>
    dplyr::mutate(Timestamp = openxlsx::convertToDateTime(dplyr::coalesce(Timestamp, shift_start))) |>
    dplyr::mutate(time_of_insp = safe_excel_to_datetime(as.character(time_of_insp))) |>
    dplyr::mutate(time_of_insp = lubridate::hms(stringr::str_extract(time_of_insp, "(?<= )[0-9]{2}:[0-9]{2}(:[0-9]{2})?"))) |>
    dplyr::mutate(time_of_insp = dplyr::case_when(
      lubridate::hour(time_of_insp) == 0 ~ lubridate::period(paste0("24H ", as.character(time_of_insp))),
      TRUE ~ time_of_insp)) |>
    dplyr::mutate(Timestamp = dplyr::case_when(
      !is.na(time_of_insp) ~ lubridate::ymd_hms(paste(lubridate::date(Timestamp), time_of_insp)),
      TRUE ~ Timestamp)) |>
    dplyr::rename(TimeOfInspection = Timestamp) |>
    dplyr::select(-time_of_insp) |>
    dplyr::mutate(dplyr::across(c(shift_start, `Shift End Time`),
                                \(x) openxlsx::convertToDateTime(as.numeric(x)))) |>
    dplyr::mutate(`Shift Date` = as.Date(as.numeric(`Shift Date`), origin = "1899-12-30")) |>
    dplyr::mutate(Hour_Min_of_Insp = stringr::str_extract(TimeOfInspection, "(?<= )[0-9]{2}:[0-9]{2}(?=:)"))

  return(dat)
}
