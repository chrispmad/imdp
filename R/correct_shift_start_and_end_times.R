correct_shift_start_and_end_times = function(metabase_dat){

  # This section of code corrects errors in shift start and end times. To do this,
  # we first test to see if the end_time is later than the start_time. If it is,
  # we find the difference in hours between the raw timestamp and the shift start,
  # then we subtract that number of hours from the start time and end time.
  # If the end time is earlier than the start time, we find the earliest and latest
  # raw timestamp by station and Workflow ID and adjusts shift start and
  # end times to fit these boundary inspections.
  metabase_shift_time_corrs = metabase_dat |>
    dplyr::ungroup() |>
    # Make start and end times of shifts into datetime columns.
    dplyr::mutate(`Start Time` = lubridate::ymd_hms(`Start Time`),
                  `End Time` = lubridate::ymd_hms(`End Time`),
                  raw_timestamp = lubridate::ymd_hms(raw_timestamp)) |>
    dplyr::mutate(shift_start_before_end = `Start Time` < `End Time`)

  # Some rows were not parsed successfully - how many? 490.
  metabase_no_end_times = metabase_shift_time_corrs |>
    dplyr::filter(is.na(`End Time`))

  # can we recover the shift end time from the shift comments? Yes!
  metabase_no_end_times = metabase_no_end_times |>
    # dplyr::filter(stringr::str_detect(`Shift Start Comment`,"-[0-9]{2}[:]?[0-9]{2}")) |>
    dplyr::mutate(`End Time` = stringr::str_extract(as.character(`Start Time`),"[0-9]{4}-[0-9]{2}-[0-9]{2}")) |>
    dplyr::mutate(hours_for_end_time = stringr::str_extract(`Shift Start Comment`,"(?<=-)[0-9]{2}")) |>
    # In the case when someone has written 24 as the end time, it should actually be 00 of the following day
    dplyr::mutate(hours_for_end_time = ifelse(hours_for_end_time == '24', '00',hours_for_end_time)) |>
    dplyr::mutate(`End Time` = paste0(`End Time`," ",hours_for_end_time,":00:00")) |>
    dplyr::mutate(`End Time` = lubridate::ymd_hms(`End Time`)) |>
    # For any rows that list midnight as their ending time, we need to change that to 00:00:00 of the following day.
    # dplyr::reframe(new_end_date = lubridate::ymd_hms(paste0(as.character(lubridate::date(`End Time`) + lubridate::days(1)), " 00:00:00")))
    dplyr::mutate(`End Time` = lubridate::as_datetime(ifelse(
      hours_for_end_time == '00',
      # Pull out date
      paste0(as.character(lubridate::date(`End Time`) + lubridate::days(1)), " 00:00:00"),
      `End Time`))) |>
    dplyr::select(dplyr::all_of(names(metabase_dat)))

  # ===========================
  # Metabase data partitions that require shifting shift start and end times.

  # Supplement gaps in the workflow ID column with raw_timestamp column.
  metabase_shift_time_corrs = metabase_shift_time_corrs |>
    dplyr::filter(!is.na(`End Time`)) |>
    dplyr::rename(wfid = `Workflow ID`) |>
    dplyr::mutate(date_from_rt = lubridate::date(raw_timestamp)) |>
    dplyr::mutate(date_from_rt_char = as.character(date_from_rt)) |>
    # Replace NA in workflow ID with this raw timestamp date.
    dplyr::mutate(wfid = ifelse(is.na(wfid), date_from_rt_char, wfid)) |>
    dplyr::select(-c(date_from_rt,date_from_rt_char))

  # Split data apart based on test: is start time before end time.

  # =======================

  # Test if there are shifts for which all inspections already fall inside the
  # reported shift start and end times.
  insp_by_wfid = metabase_shift_time_corrs |>
    dplyr::mutate(insp_inside_shift = raw_timestamp >= `Start Time` & raw_timestamp <= `End Time`) |>
    dplyr::count(Year,Station,wfid,insp_inside_shift) |>
    tidyr::pivot_wider(names_from = insp_inside_shift, values_from = n, names_prefix = "insp_in_shift_") |>
    dplyr::mutate(dplyr::across(dplyr::contains("insp_in_shift_"), \(x) ifelse(is.na(x), 0, x))) |>
    dplyr::mutate(total_inspections = insp_in_shift_FALSE + insp_in_shift_TRUE)

  insp_no_time_issues = insp_by_wfid |>
    dplyr::filter(total_inspections == insp_in_shift_TRUE)

  # Use the above lookup table to filter the metabase data table.
  metabase_no_time_issues = metabase_dat |>
    dplyr::filter(!is.na(lubridate::ymd_hms(`End Time`))) |>
    dplyr::rename(wfid = `Workflow ID`) |>
    dplyr::mutate(date_from_rt = lubridate::date(raw_timestamp)) |>
    dplyr::mutate(date_from_rt_char = as.character(date_from_rt)) |>
    dplyr::mutate(wfid = ifelse(is.na(wfid), date_from_rt_char, wfid)) |>
    dplyr::select(-c(date_from_rt,date_from_rt_char)) |>
    # Left join lookup table from just above
    dplyr::left_join(insp_no_time_issues) |>
    dplyr::filter(!is.na(total_inspections)) |>
    dplyr::rename(`Workflow ID` = wfid) |>
    dplyr::select(dplyr::all_of(names(metabase_dat))) |>
    dplyr::mutate(`Start Time` = lubridate::ymd_hms(`Start Time`),
                  `End Time` = lubridate::ymd_hms(`End Time`),
                  raw_timestamp = lubridate::ymd_hms(raw_timestamp))

  insp_with_time_issues = insp_by_wfid |>
    dplyr::filter(total_inspections != insp_in_shift_TRUE)

  # # The following table of inspections across all metabase years seems to require no fixing!
  # test_already_good = metabase_dat |>
  #   dplyr::rename(wfid = `Workflow ID`) |>
  #   dplyr::left_join(insp_no_time_issues |>
  #                      dplyr::select(Year,Station,wfid) |>
  #                      dplyr::distinct()) |>
  #   dplyr::filter(!is.na(total_inspections)) |>
  #   dplyr::select(-total_inspections)

  # The rest, though, need a fix :D
  metabase_with_time_issues = metabase_shift_time_corrs |>
    dplyr::filter(!is.na(lubridate::ymd_hms(`End Time`))) |>
    dplyr::mutate(date_from_rt = lubridate::date(raw_timestamp)) |>
    dplyr::mutate(date_from_rt_char = as.character(date_from_rt)) |>
    dplyr::mutate(wfid = ifelse(is.na(wfid), date_from_rt_char, wfid)) |>
    dplyr::select(-c(date_from_rt,date_from_rt_char)) |>
    dplyr::inner_join(insp_with_time_issues |> dplyr::select(Year,Station,wfid))

  # ===============================

  # Start time IS before end time.
  metabase_with_time_issues_test_yes = metabase_with_time_issues |> dplyr::filter(`Start Time` <= `End Time`)

  # Start time is NOT before end time.
  metabase_with_time_issues_test_no = metabase_with_time_issues |> dplyr::filter(`Start Time` > `End Time`)

  # For inspections with end time after start time.
  # =======================
  # Find difference between original start time (not to be trusted)
  # and raw timestamp (to be trusted!). Add that time difference to shift start
  # and end times.
  metabase_with_time_issues_test_yes = metabase_with_time_issues_test_yes |>
    dplyr::rename(
      start_time = `Start Time`,
      end_time = `End Time`) |>
    dplyr::group_by(Year,Station,wfid) |>
    dplyr::mutate(first_insp_hour = lubridate::hour(raw_timestamp)) |>
    dplyr::mutate(first_insp_hour = min(first_insp_hour)) |>
    dplyr::ungroup() |>
    # and the current start_time hour
    dplyr::mutate(current_start_time_hour = lubridate::hour(start_time)) |>
    # Find the difference in hours
    dplyr::mutate(hours_to_add = first_insp_hour - current_start_time_hour) |>
    dplyr::mutate(date_difference = (as.numeric(raw_timestamp) - as.numeric(start_time))/3600) |>
    dplyr::mutate(start_time_new = start_time + lubridate::hours(ceiling(date_difference)-1),
                  end_time_new = end_time + lubridate::hours(ceiling(date_difference))) |>
    dplyr::select(-first_insp_hour,-current_start_time_hour) |>
    # Write over the original shift start and end times.
    dplyr::mutate(start_time = start_time_new, end_time = end_time_new) |>
    dplyr::mutate(shift_length = end_time_new - start_time_new) |>
    dplyr::rename(`Start Time` = start_time,
                  `End Time` = end_time,
                  `Workflow ID` = wfid) |>
    dplyr::select(dplyr::all_of(names(metabase_dat)))

  # ================================

  # For data that did not pass the test of start time being before end time,
  # approximate shift hours by using first and last raw timestamp for each combination
  # of Station and Workflow ID.
  metabase_with_time_issues_test_no = metabase_with_time_issues_test_no |>
    dplyr::rename(
      start_time = `Start Time`,
      end_time = `End Time`) |>
    # Find the first and last raw timestamp in each year-station-workflow_id combination
    dplyr::group_by(Year,Station,wfid) |>
    dplyr::mutate(first_insp_datetime = min(raw_timestamp)) |>
    dplyr::mutate(last_insp_datetime = max(raw_timestamp)) |>
    dplyr::mutate(first_insp_hour = lubridate::hour(first_insp_datetime),
                  last_insp_hour = lubridate::hour(last_insp_datetime)) |>
    dplyr::ungroup() |>
    # Round the first_insp_datetime down to the nearest hour, and the last_insp_datetime
    # up to nearest hour.
    dplyr::mutate(first_insp_datetime = lubridate::floor_date(first_insp_datetime, unit = "hours"),
                  last_insp_datetime = lubridate::ceiling_date(last_insp_datetime, unit = "hours")) |>
    dplyr::rename(`Start Time` = start_time,
                  `End Time` = end_time,
                  `Workflow ID` = wfid) |>
    dplyr::select(dplyr::all_of(names(metabase_dat)))

  # ===============================
  # Merge together the three partitions of our metabase inspection data:
  metabase_dat_remade = dplyr::bind_rows(
    metabase_no_end_times,
    metabase_no_time_issues,
    metabase_with_time_issues_test_yes,
    metabase_with_time_issues_test_no
  )

  metabase_dat = metabase_dat_remade

  rm(metabase_dat_remade, metabase_no_end_times, metabase_no_time_issues,
     metabase_shift_time_corrs, metabase_with_time_issues,
     metabase_with_time_issues_test_no, metabase_with_time_issues_test_yes,
     insp_by_wfid, insp_no_time_issues, insp_with_time_issues)

  # It's a bit silly, but return the old column type to Start Time and End Time.
  metabase_dat = metabase_dat |>
    dplyr::mutate(`Start Time` = as.character(`Start Time`),
                  `End Time` = as.character(`End Time`),
                  raw_timestamp = as.character(raw_timestamp))

  return(metabase_dat)

}
