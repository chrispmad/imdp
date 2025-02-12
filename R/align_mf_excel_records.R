align_mf_excel_records = function(dat,my_opts){

  cat("We are inside 'align_mf_excel_records' function...\n")

  dat_folder = paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Raw inspection data for sharing (all years)/Clean files all years/")

  # Read in MF excel datasheets.
  dat15_mf = readxl::read_excel(paste0(dat_folder,"2015 watercraft inspection data clean.xlsx"), sheet = "Mussel Fouled",
                        skip = 1, col_types = "text")
  dat16_mf = readxl::read_excel(paste0(dat_folder,"2016 watercraft inspection data clean.xlsx"), sheet = "Mussel Fouled",
                        skip = 1, col_types = "text")
  dat17_mf = readxl::read_excel(paste0(dat_folder,"2017 watercraft inspection data clean.xlsx"), sheet = "Mussel Fouled",
                        skip = 1, col_types = "text")
  dat18_mf = readxl::read_excel(paste0(dat_folder,"2018 watercraft inspection data clean.xlsx"), sheet = "Mussel fouled",
                        skip = 1, col_types = "text")
  dat19_mf = readxl::read_xlsx(paste0(dat_folder,"2019 watercraft inspection data clean.xlsx"), sheet = 5,
                       skip = 1, col_types = "text")

  dat[[2]] = dat[[2]] |>
    dplyr::mutate(Adult_Dressenidae_Found_Ind = dplyr::case_when(
      stringr::str_remove(Watercraft_Risk_Assessment_ID, "2016-") %in% dat16_mf$`Inspection ID` ~ "true",
      TRUE ~ "false"),
      Adult_Dreissenidae_Mussel_Found_Ind = dplyr::case_when(
        stringr::str_remove(Watercraft_Risk_Assessment_ID, "2016-") %in% dat16_mf$`Inspection ID` ~ "true",
        TRUE ~ "false"))

  dat[[3]] = dat[[3]] |>
    dplyr::mutate(Adult_Dressenidae_Found_Ind = dplyr::case_when(
      stringr::str_remove(Watercraft_Risk_Assessment_ID, "2017-") %in% dat17_mf$`Inspection ID` ~ "true",
      TRUE ~ "false"),
      Adult_Dreissenidae_Mussel_Found_Ind = dplyr::case_when(
        stringr::str_remove(Watercraft_Risk_Assessment_ID, "2017-") %in% dat17_mf$`Inspection ID` ~ "true",
        TRUE ~ "false"))

  dat[[4]] = dat[[4]] |>
    dplyr::mutate(Adult_Dressenidae_Found_Ind = dplyr::case_when(
      stringr::str_remove(Watercraft_Risk_Assessment_ID, "2018-") %in% dat18_mf$`Inspection ID` ~ "true",
      TRUE ~ "false"),
      Adult_Dreissenidae_Mussel_Found_Ind = dplyr::case_when(
        stringr::str_remove(Watercraft_Risk_Assessment_ID, "2018-") %in% dat18_mf$`Inspection ID` ~ "true",
        TRUE ~ "false"))

  dat[[5]] = dat[[5]] |>
    dplyr::mutate(Adult_Dressenidae_Found_Ind = dplyr::case_when(
      stringr::str_remove(Watercraft_Risk_Assessment_ID, "2019-") %in% dat19_mf$`Inspection ID` ~ "true",
      TRUE ~ "false"),
      Adult_Dreissenidae_Mussel_Found_Ind = dplyr::case_when(
        stringr::str_remove(Watercraft_Risk_Assessment_ID, "2019-") %in% dat19_mf$`Inspection ID` ~ "true",
        TRUE ~ "false"))

  return(dat)
}
