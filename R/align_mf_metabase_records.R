align_mf_metabase_records = function(dat,my_opts){

  # Read in MF sheets for metabase data. Currently only looks at data sheets
  # from 2020, 2021, and 2022. If we want to check 2023 onwards, will need
  # to add those sheets...

  dat20_mf = readxl::read_xlsx(paste0(my_opts$zqm_operations_data_folder,
                              "Watercraft Inspection Data/2020 data/Mussel Fouled boats filtered from raw flat file.xlsx")) |>
    dplyr::slice(1:16) |>
    purrr::set_names(snakecase::to_snake_case) |>
    dplyr::mutate(watercraft_risk_assessment_id = as.character(watercraft_risk_assessment_id))

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
    dplyr::filter(!is.na(watercraft_risk_assessment_id)) |>
    dplyr::mutate(watercraft_risk_assessment_id = as.character(watercraft_risk_assessment_id))

  dat24_mf = readxl::read_xlsx(paste0(my_opts$zqm_operations_data_folder,
                                      "Watercraft Inspection Data/2024 data/mussel_fouled_summary.xlsx")) |>
    purrr::set_names(snakecase::to_snake_case) |>
    dplyr::filter(!is.na(watercraft_risk_assessment_id)) |>
    dplyr::mutate(watercraft_risk_assessment_id = as.character(watercraft_risk_assessment_id))

  dat25_mf = readxl::read_xlsx(paste0(my_opts$zqm_operations_data_folder,
                                      "Watercraft Inspection Data/2025 data/mussel_fouled_summary.xlsx")) |>
    purrr::set_names(snakecase::to_snake_case) |>
    dplyr::filter(!is.na(watercraft_risk_assessment_id)) |>
    dplyr::mutate(watercraft_risk_assessment_id = as.character(watercraft_risk_assessment_id))

  all_mf = c(dat20_mf$watercraft_risk_assessment_id, dat21_mf$watercraft_risk_assessment_id,
             dat22_mf$watercraft_risk_assessment_id, dat23_mf$watercraft_risk_assessment_id,
             dat24_mf$watercraft_risk_assessment_id, dat25_mf$watercraft_risk_assessment_id)

  # Double check mussel fouled status.
  dat = dat |>
    dplyr::mutate(
      Adult_Dressenidae_Found_Ind = dplyr::case_when(
        Watercraft_Risk_Assessment_ID %in% all_mf ~ "true",
        TRUE ~ "false"),
      Adult_Dreissenidae_Mussel_Found_Ind = dplyr::case_when(
        Watercraft_Risk_Assessment_ID %in% all_mf ~ "true",
        TRUE ~ "false")
    )

  return(dat)
}
