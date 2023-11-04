#' Title Perform the long-winded process of joining IMDP inspection data to waterbodies.
#'
#' @param dat This should be the cleaned data output from the 'general_imdp_data_prep' function; i.e. a file named 'WatercraftInspectionData_AllYears_Selected_Columns.xlsx'. If NULL, will search J: LAN folder for data file.
#' @param options_filepath Filepath to 'my_opts.csv' file on local machine.
#' @param verbose Return copious feedback?
#' @param redo_geocoding Shall we redo the time-intensive geocoding of closest cities? This takes about 20 minutes.
#'
#' @return Four excel files (this year's inspections, all inspections, high-risk inspections, and mussel-fouled inspections)
#' @export
#'
#' @examples \dontrun
summarise_imdp_data_to_waterbodies = function(
    dat = NULL,
    options_filepath = 'C:/Users/CMADSEN/Downloads/LocalR/long_term_projects/ZQMussels/Options.csv',
    redo_geocoding = F,
    verbose = T){

  # Capture the start time...
  if(verbose) cat(paste0('\nSummarising of IMDP data to waterbodies begun at ',Sys.time()))

  my_opts = suppressMessages(readr::read_csv(options_filepath))

  setwd(my_opts$base_dir)

  # IMPORT DATA

  #Read in data (cleaned, for all years?). These data do not have any of the "test" records from metabase.
  suppressWarnings(
    if(is.null(dat)){
      dat = readxl::read_excel(paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Multiyear data/WatercraftInspectionData_AllYears_All_Columns.xlsx"))
      if(verbose) cat("\nIMDP inspection data read in...")
    }
  )


  # #Which year should we focus on?
  # if(is.null(year)){
  #   report.year = max(dat$Year)
  # } else {
  #   report.year = year
  # }

  #Data folders
  my.data.folder = paste0(my_opts$zqm_figure_local_folder,"data/")
  my.external.data.folder = paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/data/")
  external.shared.datasets = paste0(my_opts$remote_spatial_data,"shared_data_sets/")

  #Which folder should we put specific output files in?
  # my.output.folder = paste0(my_opts$zqm_figure_local_folder,"output/")
  # my.external.output.folder = paste0(my_opts$zqm_figure_output_remote_folder,my_opts$year)
  # zqm.operations.folder = my_opts$zqm_operations_data_folder

  #Where can we find the CBSA notifications excel sheet for this year?

  ## NOTE: CURRENTLY NO CBSA FOR 2022!! ##
  # cbsa_dat = read_excel(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/2021 COS inbox notifications.xlsx"),
  #                       sheet = "Filtered list")

  # Read in the data file for high-risk and mussel-fouled inspections; we use these
  # later on in the function to guarantee that we have the proper number / identify of these records for our
  # summaries.
  highrisk_dat = readxl::read_excel(paste0(my_opts$zqm_figure_local_folder,'data/figure_dat_hr.xlsx'))
  musselfouled_dat = readxl::read_excel(paste0(my_opts$zqm_figure_local_folder,'data/figure_dat_mf.xlsx'))
  if(verbose) cat("\nHR and MF inspection files read in.")

  # #If there is no folder for excel figures for the target year in the I: drive, make it now.
  # if(!dir.exists(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/ExcelFigures"))){
  #   dir.create(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/ExcelFigures"))
  # }

  #=====================================================
  #                     END OF OPTIONS
  #=====================================================


  # ===========================================================
  # We need to bring in lakes, rivers and man-made water bodies,
  # drop nameless ones, then summarize any that have multiple
  # polygons into a single multipolygon.

  wbs_m = sum_wbs_by_name_subwatershed(external.shared.datasets)

  if(verbose) cat("\nMerged waterbodies geopackage read in.")

  ## We now have a layer of lake/river/manmade waterbody polygons for BC.
  ## We also have a bunch of watercraft inspection data with data on
  ## the destination waterbody (1, maybe 2 and 3) and the closest cities
  ## thereof. To help join these tables, it would be helpful to have
  ## the coordinates of the nearest city reported in the inspection data.

  ## If we have run this script before, and have this data on file,
  ## we'll load it now. If not, the code below will rerun the BC
  ## geocoder.

  DestinationPlacenames = geocode_destination_names(dat,
                            my.data.folder,
                            redo_geocoding,
                            verbose)


  # =======================================
  #     Matching IMDP data to waterbodies
  # =======================================
  # What we need:
  # IMDP data summarised to waterbody name and anything else needed to identify
  #     waterbody uniquely, e.g. closest city, watershed #, etc., then join
  #     to the 'wbs_m' object.
  #
  # Ways to match:
  # 1. Unique names of waterbodies (one in whole province); immediate join potential.
  # 2. Waterbody with common name, use closest city to find most likely waterbody in wbs_m.
  # 3. Record doesn't match a waterbody (e.g. recorded name is Dry Storage or Pacific Ocean),
  #    but it has a closest city that can be matched up with the most common pairing
  #    (e.g. Kamloops )
  # X. Record specifies either Dry Storage or Pacific Ocean as destination; keep record aside.

  # Add the coordinates of the closest city that we got using the
  # BC geocoder. These coordinates will be used in the case of a record
  # matching names with multiple lakes.

  dat_join = dat |>
    dplyr::rename(GNIS_NAME_ = Destination_Waterbody_1_Name,
                  Closest_City = Destination_Waterbody_1_Closest_City) |>
    dplyr::bind_rows(dat |>
                       dplyr::mutate(GNIS_NAME_ = Destination_Waterbody_2_Name,
                                     Closest_City = Destination_Waterbody_2_Closest_City) |>
                       dplyr::filter(is.na(GNIS_NAME_)==F)) |>
    dplyr::bind_rows(dat |>
                       dplyr::mutate(GNIS_NAME_ = as.character(Destination_Waterbody_3_Name),
                                     Closest_City = as.character(Destination_Waterbody_3_Closest_City)) |>
                       dplyr::filter(!is.na(GNIS_NAME_)) |>
                       dplyr::filter(!is.na(Closest_City))) |>
    dplyr::select(Year,Watercraft_Risk_Assessment_ID, GNIS_NAME_, Closest_City,
                  Simple_Counter, Complex_Counter, Very_Complex_Counter, Non_Motorized_Counter) |>
    dplyr::rename(GNIS_NA = GNIS_NAME_) |>
    dplyr::mutate(GNIS_NA = tidyr::replace_na(GNIS_NA, "NA")) |>
    dplyr::arrange(dplyr::desc(Watercraft_Risk_Assessment_ID))

  dat_join = dat_join |>
    dplyr::filter(GNIS_NA != "NA") |>
    dplyr::left_join(DestinationPlacenames |>
                       dplyr::rename(Closest_City = names),
                     by = dplyr::join_by(Closest_City))

  # Homogenize spelling of Pacific Ocean in GNIS_NA field.
  dat_join = dat_join |>
    dplyr::mutate(GNIS_NA = ifelse(stringr::str_detect(GNIS_NA, '[o,O]cean'),
                         'Pacific Ocean',
                         GNIS_NA))

  # # I actually don't think we can do the following steps (replacing blanks in
  # lat and long for pacific ocean / dry storage destinated watercraft), as it
  # seems to make a lot of records NOT heading to a lake in BC appear as if they are!

  # Replace missing coords for any records going to either 'Pacific Ocean'
  # or 'Dry Storage'
  # pac_centroids = as.character(as.data.frame(sf::st_coordinates(sf::st_centroid(sf::st_transform(wbs_m[wbs_m$GNIS_NA == 'Pacific Ocean',], 4326)))))
  # ds_centroids = as.character(as.data.frame(sf::st_coordinates(sf::st_centroid(sf::st_transform(wbs_m[wbs_m$GNIS_NA == 'Dry Storage',], 4326)))))

  # dat_join |>
  #   dplyr::filter(GNIS_NA %in% c('Pacific Ocean','Dry Storage')) |>
  #   dplyr::filter(is.na(lat))
  #
  # dat_join = dat_join |>
  #   dplyr::mutate(lat = dplyr::case_when(
  #     GNIS_NA == 'Pacific Ocean' ~ pac_centroids[2],
  #     T ~ lat
  #   )) |>
  #   dplyr::mutate(lon = dplyr::case_when(
  #     GNIS_NA == 'Pacific Ocean' ~  pac_centroids[1],
  #     T ~ lon
  #   )) |>
  #   dplyr::mutate(lat = dplyr::case_when(
  #     GNIS_NA == 'Dry Storage' ~  ds_centroids[2],
  #     T ~ lat
  #   )) |>
  #   dplyr::mutate(lon = dplyr::case_when(
  #     GNIS_NA == 'Dry Storage' ~ ds_centroids[1],
  #     T ~ lon
  #   ))

  dat_join_w_coords = dat_join |>
    dplyr::filter(!is.na(lon),!is.na(lat))

  # Confirm the high-risk and mussel-fouled inspections, then
  # summarise the inspection records by year, waterbody name, closest city.
  dat_summ = dat_join_w_coords |>
    dplyr::mutate(
      msfouled_Counter = dplyr::case_when(
        paste0(Year,Watercraft_Risk_Assessment_ID) %in%
          paste0(musselfouled_dat$Year,
                 musselfouled_dat$Watercraft_Risk_Assessment_ID) ~ T,
        T ~ F
      ),
      highrisk_Counter = dplyr::case_when(
        paste0(Year,Watercraft_Risk_Assessment_ID) %in%
          paste0(highrisk_dat$Year,
                 highrisk_dat$Watercraft_Risk_Assessment_ID) ~ T,
        T ~ F
      )
    ) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(Year, GNIS_NA, Closest_City, lat, lon) |>
    dplyr::summarise(n = dplyr::n(),
                     dplyr::across(dplyr::ends_with('Counter'), \(x) sum(x,na.rm=T))) |>
    dplyr::arrange(desc(n)) |>
    dplyr::ungroup() |>
    sf::st_as_sf(
      coords = c('lon','lat'),
      crs = 4326
    ) |>
    sf::st_transform(crs = 3005)

  # Split apart the low- and high-risk inspections into different boat types.
  dat_summ_split_types = dat_summ |>
    dplyr::full_join(
      dat_join_w_coords |>
        sf::st_drop_geometry() |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("Counter"), as.numeric)) |>
        dplyr::filter(!paste0(Year,Watercraft_Risk_Assessment_ID) %in%
                        paste0(highrisk_dat$Year,
                               highrisk_dat$Watercraft_Risk_Assessment_ID),
                      !paste0(Year,Watercraft_Risk_Assessment_ID) %in%
                        paste0(musselfouled_dat$Year,
                               musselfouled_dat$Watercraft_Risk_Assessment_ID)) |>
        tidyr::pivot_longer(dplyr::ends_with("Counter"),
                            names_to = "BoatType", values_to = "BoatTypeNumber") |>
        dplyr::mutate(BoatTypeNumber = replace(BoatTypeNumber, BoatTypeNumber > 1, 1)) |>
        dplyr::filter(BoatTypeNumber > 0) |>
        dplyr::group_by(Closest_City,GNIS_NA,Year,BoatType) |>
        dplyr::summarise(TotalInsp = dplyr::n()) |>
        tidyr::pivot_wider(names_from = BoatType, values_from = TotalInsp,
                           names_prefix = "LowRisk_", values_fill = 0)
    ) |>
    #High-risk inspections, split into 4 categories of boat 'complexity'
    dplyr::full_join(
      dat_join_w_coords |>
        sf::st_drop_geometry() |>
        dplyr::mutate(dplyr::across(dplyr::ends_with("Counter"), as.numeric)) |>
        dplyr::filter(paste0(Year,Watercraft_Risk_Assessment_ID) %in%
                        paste0(highrisk_dat$Year,
                               highrisk_dat$Watercraft_Risk_Assessment_ID)) |>
        tidyr::pivot_longer(dplyr::ends_with("Counter"),
                            names_to = "BoatType", values_to = "BoatTypeNumber") |>
        dplyr::mutate(BoatTypeNumber = replace(BoatTypeNumber, BoatTypeNumber > 1, 1)) |>
        dplyr::filter(BoatTypeNumber > 0) |>
        dplyr::group_by(Closest_City,GNIS_NA,Year,BoatType) |>
        dplyr::summarise(NumberHighRisk = dplyr::n()) |>
        tidyr::pivot_wider(names_from = BoatType, values_from = NumberHighRisk,
                           names_prefix = "HR_", values_fill = 0)
    ) |> dplyr::distinct() |>
    dplyr::group_by(GNIS_NA, Closest_City) |>
    dplyr::summarise(dplyr::across(c(n,dplyr::ends_with("Counter")), \(x) sum(x,na.rm=T))) |>
    dplyr::ungroup()
  if(verbose) cat("\nIMDP inspection data, low and high risk inspections split into boat types...")
  # Turn the spatial object of waterbodies (merged) into a list,
  # split by name (each element has 1 or more elements, all with the same GNIS_NA name)
  wbs_l = wbs_m |>
    dplyr::ungroup() |>
    dplyr::group_by(GNIS_NA) |>
    dplyr::group_split()

  if(verbose) cat("\nMerged waterbodies object split into list...")
  # For each element of the waterbodies list,
  imdp_dat_with_watershed = lapply(
    wbs_l, \(x)
    # Join it spatially to the IMDP inspection data that list the same name in their destination field.
    sf::st_join(
      dat_summ_split_types[dat_summ_split_types$GNIS_NA == x$GNIS_NA,],
      x,
      sf::st_nearest_feature
    ) |>
      dplyr::filter(GNIS_NA.x == GNIS_NA.y) |>
      dplyr::rename(GNIS_NA = GNIS_NA.x) |>
      dplyr::select(-GNIS_NA.y)
  ) |> dplyr::bind_rows() |>
    dplyr::distinct() |>
    # Summarise by waterbody name and subwatershed...
    dplyr::group_by(WATERSH,GNIS_NA) |>
    dplyr::summarise(dplyr::across(c(n,dplyr::ends_with('Counter')), \(x) sum(x,na.rm=T))) |>
    dplyr::ungroup() |>
    dplyr::rename(
      TotalInspections = n,
      NumberMusselFouled = msfouled_Counter
    ) |>
    dplyr::arrange(dplyr::desc(TotalInspections))

  if(verbose) cat("\nSpatial match between waterbodies and IMDP inspection destination waterbodies complete.")

  wbs_with_dat = wbs_m |>
    dplyr::left_join(
      imdp_dat_with_watershed |>
        sf::st_drop_geometry()
    )

  # Drop NULL rows.
  if(verbose) cat(paste0("\nAbout to drop NULL rows (n = ",nrow(wbs_with_dat |> dplyr::filter(is.na(TotalInspections))),")"))

  wbs_with_dat = wbs_with_dat |>
    dplyr::filter(!is.na(TotalInspections))

  sf::write_sf(wbs_with_dat, paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/data/Waterbodies_with_Inspection_Data_Summaries.gpkg"))

  cat(paste0('Summarising of IMDP data to waterbodies completed at ',Sys.time()))

  return(wbs_with_dat)
}

