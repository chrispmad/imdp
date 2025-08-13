#' Combine and clean IMDP inspection data
#'
#' @param options_filepath Filepath to options .csv file
#' @param verbose Return copious feedback?
#'
#' @description
#' This is one of the main functions exported by the {imdp} package.
#' It collates and cleans excel and metabase data files. Adjust the data imports
#' and destination folders for the cleaned data files with the options .csv file.
#'
#' @return Three excel files will be saved to the external output folder specified in the options file.
#' @export
#'
#' @examples \dontrun
general_imdp_data_prep = function(
    options_filepath = NA,
    verbose = T){

  if(verbose) {

    estimated_finish_time = Sys.time() + lubridate::minutes(30)

    finish_minute = lubridate::minute(estimated_finish_time)
    if(as.numeric(finish_minute) < 10) finish_minute = paste0('0',finish_minute)

    suppressWarnings(
      cat(paste0("\nRough ETA for function completion: ",lubridate::hour(estimated_finish_time),":",finish_minute))
    )

    cat(paste0('\n',lubridate::hour(Sys.time()),":",lubridate::minute(Sys.time()),' - Finished reading and combining the excel and metabase data...'))
  }
  # Read in options file.
  my_opts = suppressMessages(readr::read_csv(options_filepath))

  # Set up output folders.
  my.output.folder = paste0(my_opts$base_dir,"01_DataCleaning/output/")
  my.external.output.folder = paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Multiyear data/")

  #Data folders.
  my.data.folder = paste0(my_opts$base_dir,"01_DataCleaning/data/")
  my.external.data.folder = "//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/shared_data_sets/"

  # Set working directory to location of data file.
  # setwd(paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years"))

  # =======
  # Step 1
  # =======
  # Read in and combine excel and metabase data files (1 per year)
  dat = read_combine_excel_metabase_data(verbose, my_opts)

  cat("Finished combining excel and metabase data...\n")

  abbrev = readxl::read_excel(paste0("//SFP.IDIR.BCGOV/S140/S40203/WFC AEB/General/2 SCIENCE - Invasives/AIS_R_Projects/CMadsen_Wdrive/Projects/ZQMussels/data/Province_States_Abbreviation_Table.xlsx"))

  # =======
  # Step 2
  # =======
  # Clean data - returns list of length three: cleaned dat, highrisk dat, musselfouled dat.
  dat_c = clean_imdp_dat(dat,
                         my_opts,
                         abbrev,
                         verbose)
  cat()
  if(verbose) cat(paste0('\n',lubridate::hour(Sys.time()),":",lubridate::minute(Sys.time()),' - Finished cleaning the excel and metabase data...'))

  all_dat_c_all_cols = dat_c[[1]]
  all_dat_c_to_write = dat_c[[2]]
  hr_dat = dat_c[[3]]
  mf_dat = dat_c[[4]]

  # =======
  # Step 3
  # =======
  #Write out cleaned data as excel file.
  openxlsx::write.xlsx(all_dat_c_to_write,
                       paste0(my.external.output.folder,"WatercraftInspectionData_AllYears_Selected_Columns.xlsx"),
                       overwrite = T)

  # Write out cleaned data with ALL columns (useful for summarising inspections to waterbodies).
  openxlsx::write.xlsx(all_dat_c_all_cols,
                       paste0(my.external.output.folder,"WatercraftInspectionData_AllYears_All_Columns.xlsx"),
                       overwrite = T)

  # Same for high-risk data.
  openxlsx::write.xlsx(hr_dat,
                       paste0(my.external.output.folder,"WatercraftInspectionData_AllYears_high_risk.xlsx"),
                       overwrite = T)

  # Same for mussel-fouled data.
  openxlsx::write.xlsx(mf_dat,
                       paste0(my.external.output.folder,"WatercraftInspectionData_AllYears_mussel_fouled.xlsx"),
                       overwrite = T)
  if(verbose) cat(paste0('\n',lubridate::hour(Sys.time()),":",lubridate::minute(Sys.time()),' - Finished writing out files for all inspection data, high-risk data, and mussel-fouled data.'))
}
