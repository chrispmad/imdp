combine_all_data = function(metabase_dat,
                            excel_data_list){

  dplyr::bind_rows(metabase_dat,
    dplyr::bind_rows(excel_data_list[[5]] |> dplyr::mutate(TimeOfInspection = lubridate::force_tz(TimeOfInspection, "America/Los_Angeles")),
              excel_data_list[[4]] |> dplyr::mutate(TimeOfInspection = lubridate::force_tz(TimeOfInspection, "America/Los_Angeles")),
              excel_data_list[[3]] |> dplyr::mutate(TimeOfInspection = lubridate::force_tz(TimeOfInspection, "America/Los_Angeles")),
              excel_data_list[[2]] |> dplyr::mutate(TimeOfInspection = lubridate::force_tz(TimeOfInspection, "America/Los_Angeles")),
              excel_data_list[[1]] |> dplyr::mutate(TimeOfInspection = lubridate::force_tz(TimeOfInspection, "America/Los_Angeles")))
  )

}
