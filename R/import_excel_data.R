import_excel_data = function(){
  # Load in old excel sheets
  dat15 = readxl::read_excel("2015 watercraft inspection data clean.xlsx", sheet = "All inspections",
                     skip = 1, col_types = "text")
  dat16 = readxl::read_excel("2016 watercraft inspection data clean.xlsx", sheet = "All inspections",
                     skip = 1, col_types = "text")
  dat17 = readxl::read_excel("2017 watercraft inspection data clean.xlsx", sheet = "All inspections",
                     skip = 1, col_types = "text")
  dat18 = readxl::read_excel("2018 watercraft inspection data clean.xlsx", sheet = "All inspections",
                     skip = 1, col_types = "text")
  dat19 = readxl::read_excel("2019 watercraft inspection data clean.xlsx", sheet = "All inspections",
                     skip = 1, col_types = "text")

  return(list(dat15, dat16, dat17, dat18, dat19))
}
