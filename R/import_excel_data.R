import_excel_data = function(my_opts){
  # Load in old excel sheets
  dat_folder = paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years")

  dat15 = readxl::read_excel(paste0(dat_folder,"/2015 watercraft inspection data clean.xlsx"), sheet = "All inspections",
                     skip = 1, col_types = "text")
  dat16 = readxl::read_excel(paste0(dat_folder,"/2016 watercraft inspection data clean.xlsx"), sheet = "All inspections",
                     skip = 1, col_types = "text")
  dat17 = readxl::read_excel(paste0(dat_folder,"/2017 watercraft inspection data clean.xlsx"), sheet = "All inspections",
                     skip = 1, col_types = "text")
  dat18 = readxl::read_excel(paste0(dat_folder,"/2018 watercraft inspection data clean.xlsx"), sheet = "All inspections",
                     skip = 1, col_types = "text")
  dat19 = readxl::read_excel(paste0(dat_folder,"/2019 watercraft inspection data clean.xlsx"), sheet = "All inspections",
                     skip = 1, col_types = "text")

  return(list(dat15, dat16, dat17, dat18, dat19))
}
