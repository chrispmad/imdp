#' Title Run both IMDP data cleaning functions in succession
#'
#' @param dat This should be the cleaned data output from the 'general_imdp_data_prep' function; i.e. a file named 'WatercraftInspectionData_AllYears_Selected_Columns.xlsx'. If NULL, will search J: LAN folder for data file.
#' @param options_filepath Filepath to 'my_opts.csv' file on local machine.
#' @param year Which year's IMDP final report are we prepping this data for? If NULL, will default to the year in 'my_opts.csv'
#' @param verbose Return copious feedback?
#'
#' @return Four excel files (this year's inspections, all inspections, high-risk inspections, and mussel-fouled inspections), plus updated GIS files on W: drive and excel tables on LAN J: drive.
#' @export
#'
#' @examples \dontrun
update_all_imdp_data = function(
    dat = NULL,
    year = NULL,
    options_filepath = 'C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/Options.csv',
    verbose = T,
    update_spatial_files = T){

  general_imdp_data_prep()

  figure_imdp_data_prep()

  if(update_spatial_files) {
    summarise_imdp_data_to_waterbodies()
  }
}
