sum_wbs_by_name_subwatershed = function(external.shared.datasets) {
  if(file.exists(paste0(external.shared.datasets,"summarized_bc_waterbodies_same_gnis_joined.gpkg"))) {
    wbs_m = sf::read_sf(paste0(external.shared.datasets,"summarized_bc_waterbodies_same_gnis_joined.gpkg"))
  }
  if(!file.exists(paste0(external.shared.datasets,"summarized_bc_waterbodies_same_gnis_joined.gpkg"))){
    # Double check before we start on this odyssey of data downloads!

    cat("\nLooks like we're going to make the 'summarized_bc_waterbodies_same_gnis_joined.gpkg' file... This will take a long time!")
    cat("\nWould you like to continue? (Y or N)")
    embark_on_odyssey = check_users_input(c("Y","N"))

    if(embark_on_odyssey == 'Y'){
      #The code below in this if chunk is untested.
      manmade_noNA = bcdata::bcdc_get_data("freshwater-atlas-manmade-waterbodies") |>
        dplyr::select(-id) |> dplyr::filter(is.na(GNIS_NAME_1)==F)
      lakes_noNA = bcdata::bcdc_get_data("freshwater-atlas-lakes") |>
        dplyr::select(-id) |> dplyr::filter(is.na(GNIS_NAME_1)==F)
      rivers_noNA = bcdata::bcdc_get_data("freshwater-atlas-rivers") |>
        dplyr::select(-id) |> dplyr::filter(is.na(GNIS_NAME_1)==F)

      #Put these water body shapefiles together.
      wbs = lakeshp_noNA |> dplyr::mutate(WaterBodyType = "Lake") |>
        dplyr::bind_rows(rivershp_noNA |> dplyr::mutate(WaterBodyType = "River"),
                         manmadeshp_noNA |> dplyr::mutate(WaterBodyType = "Manmade"))

      wbs_m = wbs |>
        dplyr::group_by(WATERSHED_,GNIS_NAME_) |>
        dplyr::summarise() |>
        dplyr::rename(WATERSH = WATERSHED_,
                      GNIS_NA = GNIS_NAME_)

      # Add in boxes located in the ocean for Pacific Ocean and Dry Storage.
      pac_ocean_box = sf::st_as_sf(data.frame(lat = c(49.9512, 48.767),
                          lng = c(-130.805, -129.090)),
                          coords = c('lng','lat'),
                          crs = 4326) |>
        sf::st_bbox() |>
        sf::st_as_sfc() |>
        sf::st_transform(3005)

      pac_ocean_box = dplyr::tibble(WATERSH = 0, GNIS_NA = 'Pacific Ocean') |>
        sf::st_set_geometry(pac_ocean_box)

      dry_storage_box = sf::st_as_sf(data.frame(lat = c(49.9512, 48.767),
                                              lng = c(-129.090, -127.375)),
                                   coords = c('lng','lat'),
                                   crs = 4326) |>
        sf::st_bbox() |>
        sf::st_as_sfc() |>
        sf::st_transform(3005)

      dry_storage_box = dplyr::tibble(WATERSH = 0, GNIS_NA = "Dry Storage") |>
        sf::st_set_geometry(dry_storage_box)

      wbs_m = dplyr::bind_rows(wbs_m,
                       pac_ocean_box,
                       dry_storage_box)

      sf::write_sf(wbs_m, paste0(external.shared.datasets,"summarized_bc_waterbodies_same_gnis_joined.gpkg"))
    } else {
      stop('Process interrupted.')
    }
  }
  return(wbs_m)
}
