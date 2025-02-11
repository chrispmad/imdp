read_combine_excel_metabase_data = function(verbose, my_opts){

  if(verbose) cat("\nReading in excel files...")
  # Bring in raw excel data (2015 - 2019).
  excel_dat_list = suppressMessages(import_excel_data(my_opts))
  if(verbose) cat("\nFinished reading in excel files...")

  if(verbose) cat("\nReading in excel files...")
  # Bring in metabase data.
  metabase_dat = import_metabase_dat(my_opts)
  if(verbose) cat("\nFinished reading in metabase files...")

  # Clean excel datetime columns
  excel_dat_list = suppressWarnings(clean_excel_datetime_cols(excel_dat_list))
  if(verbose) cat("\nFinished correcting excel datetime columns...")

  # Clean metabase datetime columns
  metabase_dat = clean_metabase_datetime_cols(metabase_dat)
  if(verbose) cat("\nFinished correcting metabase datetime columns...")

  # Clean column names
  excel_dat_list = lapply(excel_dat_list,
                          excel_name_improver)

  metabase_dat = excel_name_improver(metabase_dat)

  # Shorten province/state data of old excel files to 2-letter codes
  excel_dat_list = suppressWarnings(purrr::map2(excel_dat_list,
                                                "State_Province_of_Boat_Residence",
                                                ~ {destination_name_abbreviator(.x,.y)}))

  excel_dat_list = suppressWarnings(purrr::map2(excel_dat_list,
                                                "State_Province_of_Destination_Waterbody",
                                                ~ {destination_name_abbreviator(.x,.y)}))
  if(verbose) cat("\nstate/prov of boat residence and destination abbreviated to 2-letters")

  # Homogenize old excel file column names.
  excel_dat_list = homogenize_excel_col_names(excel_dat_list)

  # Make sure mussel-fouled records align between hand-corrected sheets from
  # Martina and the raw data.
  excel_dat_list = suppressMessages(align_mf_excel_records(excel_dat_list))

  metabase_dat = suppressMessages(suppressWarnings(align_mf_metabase_records(metabase_dat,my_opts)))

  # Combine data
  dat = combine_all_data(metabase_dat,
                         excel_dat_list)

  # Combine MF cols
  dat = combine_mf_cols(dat)

  dat
}
