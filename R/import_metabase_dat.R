import_metabase_dat = function(){

  files.to.read = list.files(pattern = "metabase_")

  #Read in each metabase csv file and put them all together in a list.
  metabase_data_list = lapply(files.to.read, readr::read_csv, col_types = readr::cols(.default = "c"))

  #Add names to each element (i.e. dataframe) in this list.
  metabase_data_list = stats::setNames(metabase_data_list, 2020:(2020+length(metabase_data_list)-1))

  metabase_dat = metabase_data_list |>
    dplyr::bind_rows(.id = "Year") |>
    dplyr::mutate(Year = as.numeric(Year))

  rm(metabase_data_list)

  return(metabase_dat)
}
