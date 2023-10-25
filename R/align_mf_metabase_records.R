align_mf_metabase_records = function(dat,my_opts){

  # Read in MF sheets for metabase data. Currently only looks at data sheets
  # from 2020, 2021, and 2022. If we want to check 2023 onwards, will need
  # to add those sheets...

  dat20_mf = readxl::read_xlsx(paste0(my_opts$zqm_operations_data_folder,
                              "Watercraft Inspection Data/2020 data/Mussel Fouled boats filtered from raw flat file.xlsx")) |>
    dplyr::slice(1:16) |>
    purrr::set_names(snakecase::to_snake_case)

  dat21_mf = readxl::read_xlsx(paste0(my_opts$zqm_operations_data_folder,
                              "Watercraft Inspection Data/2021 data/Mussel fouled boats tracking sheet 2021-08-20.xlsx")) |>
    dplyr::slice(1:17) |>
    purrr::set_names(snakecase::to_snake_case)

  dat22_mf = readxl::read_xlsx(paste0(my_opts$zqm_operations_data_folder,
                              "Watercraft Inspection Data/2022 data/2022 mussel fouled boats tracking sheet.xlsx")) |>
    dplyr::slice(1:14) |>
    purrr::set_names(snakecase::to_snake_case) |>
    dplyr::filter(!is.na(watercraft_risk_assessment_id))

  dat23_mf = readxl::read_xlsx(paste0(my_opts$zqm_operations_data_folder,
                                      "Watercraft Inspection Data/2023 data/2023_mussel_fouled_details.xlsx")) |>
    purrr::set_names(snakecase::to_snake_case) |>
    dplyr::filter(!is.na(watercraft_risk_assessment_id))

  # Double check mussel fouled status.

  dat = dat |>
    dplyr::mutate(Adult_Dressenidae_Found_Ind = dplyr::case_when(
      stringr::str_remove(Watercraft_Risk_Assessment_ID,"202[0-3]{1}-") %in% c(dat20_mf$watercraft_risk_assessment_id,
                                                                      dat21_mf$watercraft_risk_assessment_id,
                                                                      dat22_mf$watercraft_risk_assessment_id,
                                                                      dat23_mf$watercraft_risk_assessment_id) ~ "true",
      stringr::str_detect(Watercraft_Risk_Assessment_ID,'202[4-9]{1}') ~ Adult_Dressenidae_Found_Ind,
      TRUE ~ "false"),
      Adult_Dreissenidae_Mussel_Found_Ind = dplyr::case_when(
        stringr::str_remove(Watercraft_Risk_Assessment_ID,"202[0-3]{1}-") %in% c(dat20_mf$watercraft_risk_assessment_id,
                                                                        dat21_mf$watercraft_risk_assessment_id,
                                                                        dat22_mf$watercraft_risk_assessment_id,
                                                                        dat23_mf$watercraft_risk_assessment_id) ~ "true",
        stringr::str_detect(Watercraft_Risk_Assessment_ID,'202[4-9]{1}') ~ Adult_Dreissenidae_Mussel_Found_Ind,
        TRUE ~ "false"))

  return(dat)
}
