#' More stringent data preparation for IMDP report figures
#'
#' @param dat This should be the cleaned data output from the 'general_imdp_data_prep' function; i.e. a file named 'WatercraftInspectionData_AllYears_Selected_Columns.xlsx'. If NULL, will search J: LAN folder for data file.
#' @param options_filepath Filepath to 'my_opts.csv' file on local machine.
#' @param year Which year's IMDP final report are we prepping this data for? If NULL, will default to the year in 'my_opts.csv'
#' @param verbose Return copious feedback?
#'
#' @return Four excel files (this year's inspections, all inspections, high-risk inspections, and mussel-fouled inspections)
#' @export
#'
#' @examples \dontrun
figure_imdp_data_prep = function(
    dat = NULL,
    options_filepath = NA,
    year = NULL,
    verbose = T){

  # Record initial working directory; reset wd to this at end of function.
  original_wd = getwd()

  my_opts = suppressMessages(readr::read_csv(options_filepath))

  #Which year should we focus on?
  if(is.null(year)){
    report.year = my_opts$year
  } else {
    report.year = year
  }

  # setwd(my_opts$base_dir)

  #Data folders
  my.data.folder = paste0(my_opts$zqm_figure_local_folder,"data/")
  my.external.data.folder = paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/data/")

  #Which folder should we put specific output files in?
  my.output.folder = paste0(my_opts$zqm_figure_local_folder,"output/")
  my.external.output.folder = paste0(my_opts$zqm_figure_output_remote_folder,my_opts$year)
  zqm.operations.folder = my_opts$zqm_operations_data_folder
  this.years.report.folder = paste0(my_opts$remote_spatial_data,"Projects/ZQMussels/",report.year," IMDP Final Report/")

  #Where is the mussel-fouled tracking sheet?
  # MusselFouledTracker = paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/", report.year," data/2022 mussel fouled boats tracking sheet.xlsx")

  #What is the sheet name of the destination regions for the mussel fouled tracking sheet?
  # MF_tracker_sheet = "DestRegions"

  #Where can we find the CBSA notifications excel sheet for this year?

  ## NOTE: CURRENTLY NO CBSA FOR 2022!! ##
  # cbsa_dat = read_excel(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/2021 COS inbox notifications.xlsx"),
  #                       sheet = "Filtered list")

  #If there is no folder for excel figures for the target year in the I: drive, make it now.
  if(!dir.exists(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/ExcelFigures"))){
    dir.create(paste0(my.external.output.folder,"/GIS Maps and Excel Figures/ExcelFigures"))
  }

  #=====================================================
  #                     END OF OPTIONS
  #=====================================================
  # IMPORT DATA

  #Read in data (cleaned, for all years?). These data do not have any of the "test" records from metabase.
  if(is.null(dat)){
    dat = readxl::read_excel(paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Multiyear data/WatercraftInspectionData_AllYears_Selected_Columns.xlsx"))
  }

  #Get Station coordinates, might be unnecessary.
  stations = sf::read_sf(paste0(my.external.data.folder,"inspection_stations.gpkg"))

  #Lookup table of provinces/territories and U.S. States (full names and abbreviations)
  abbrev = readxl::read_excel(paste0(my.external.data.folder,"Province_States_Abbreviation_Table.xlsx"))

  #Lookup table for named waterbodies in BC and which FLNRO fisheries region they can be found in.
  flnro_lookup = readxl::read_excel(paste0(my.external.data.folder,"waterbody_name_flrno_region_lookup_table.xlsx")) |> dplyr::distinct()

  #Lookup table for what the Previous Knowledge of AIS field's codes mean.
  ais_know = readxl::read_excel(paste0(my.external.data.folder,"Previous_Knowledge_of_AIS_lookup_table.xlsx"))

  if(verbose) cat("\nFinished reading in data.")

  # #We need to clean up the field(s) that talk about which province/state a given inspection is from.
  # # CHECK IF WE NEED THIS AFTER THE GENERAL CLEANING.
  # dat = dat |>
  #   #Make the name shorter for now...
  #   rename(Source = Previous_Waterbody_1_Province_Or_State) |>
  #   #Replace NA in Source with previous major city, if we have it. If not, use the province_code of the boat. If we don't have any of those, or if the Source field wasn't blank, keep it as it was.
  #   mutate(NewSource = case_when(
  #     (Source == "Unknown" | is.na(Source)) & !is.na(Previous_Major_City) & Previous_Major_City != "None" ~ Previous_Major_City,
  #     (Source == "Unknown" | is.na(Source)) & (is.na(Previous_Major_City) | Previous_Major_City == "None") ~ Province_Code,
  #     T ~ Source)) |>
  #   #If we replaced the Source field with the closest major city, we need to pull the province/state name out of the string.
  #   mutate(NewSource = case_when(
  #     str_detect(NewSource, ",.*,") ~ str_extract(NewSource, "(?<=, ).*(?=,)"),
  #     T ~ NewSource)) |>
  #   #Finally, change the long form (e.g. "Alberta") to abbreviated form for names (e.g. "AB").
  #   left_join(abbrev |> rename(NewSource = Province_or_State)) |>
  #   mutate(NewSource = coalesce(Abbrev,NewSource)) |>
  #   dplyr::select(-Abbrev,-Source) |>
  #   rename(Previous_Waterbody_1_Province_Or_State = NewSource)


  # calculate_total_boats_and_convert_to_datetime
  #Add total boats (multiply inspections by boat type counters)
  dat = dat |>
    dplyr::mutate(TotalBoats = as.numeric(Non_Motorized_Counter) +
             as.numeric(Simple_Counter) +
             as.numeric(Complex_Counter) +
             as.numeric(Very_Complex_Counter)) |>
    dplyr::mutate(TotalBoats = replace(TotalBoats, TotalBoats == 0, 1))

  # #Convert start_time, end_time and TimeOfInspection to datetime format.
  # dat$Start_Time = convertToDateTime(dat$Start_Time)
  # dat$End_Time = convertToDateTime(dat$End_Time)
  # dat$TimeOfInspection = convertToDateTime(dat$TimeOfInspection)

  dat = dat |>
    dplyr::mutate(Shift_hours = as.numeric(End_Time - Start_Time)/3600)

  # Correct negative shift lengths to 0
  dat = dat |>
    dplyr::mutate(Shift_hours = ifelse(Shift_hours < 0, -Shift_hours, Shift_hours))

  #Add a unique identifier for each shift. I'm using combinations of start and end time
  #so hopefully they are unique.
  dat = dat |>
    dplyr::group_by(Start_Time, End_Time) |>
    dplyr::mutate(Shift_ID = dplyr::cur_group_id()) |>
    dplyr::select(Year,Shift_ID, tidyr::everything()) |>
    dplyr::arrange(Shift_ID) |>
    dplyr::ungroup()

  #First, we need to get total inspections, HR inspections, and mussel-fouled inspections by source location.
  if(!dir.exists(my.data.folder)) dir.create(my.data.folder)

  setwd(my.data.folder)

  if(!file.exists(paste0(my_opts$remote_spatial_data,"shared_data_sets/northamerica.gpkg"))){
    if(verbose) cat('\nNo geopackage detected in remote spatial data repository for North American provs/states')
    if(verbose) cat('\nMaking it now...')
    canada = raster::getData(country = "CAN", level = 1)
    usa = raster::getData(country = "USA", level = 1)
    mex = raster::getData(country = "MEX", level = 1)

    canada = sf::st_as_sf(canada)
    usa = sf::st_as_sf(usa)
    mex = sf::st_as_sf(mex)

    na_sf = canada |>
      dplyr::bind_rows(usa, mex)

    na_sf = sf::st_simplify(na_sf)

    na_sf = na_sf |>
      dplyr::filter(!NAME_1 %in% c("Coahuila","Hidalgo"))

    sf::write_sf(na_sf,
                 paste0(my_opts$remote_spatial_data,"shared_data_sets/northamerica.gpkg"))
  } else {
    if(verbose) cat('\nNorth American states/provs geopackage detected in remote data storage; importing...')
    na_sf = sf::read_sf(paste0(my_opts$remote_spatial_data,"shared_data_sets/northamerica.gpkg"))
    if(verbose) cat('\nFinished reading in geopackage.')
  }

    # #Cleaning up previous waterbody 1 prov/state field...
    # source_dat = dat |>
    #   dplyr::rename(ABBR = Previous_Waterbody_1_Province_Or_State) |>
    #   dplyr::mutate(ABBR = dplyr::case_when(
    #     ABBR == "unknown" ~ "Unknown",
    #     ABBR == "Calgary" ~ "AB",
    #     ABBR == "Mexico" ~ "BN",
    #     ABBR == "Wi" ~ "WI",
    #     ABBR == "Radium" ~ "BC",
    #     ABBR == "Sk" ~ "SK",
    #     ABBR == "SA" ~ "SK",
    #     ABBR == "Ab" ~ "AB",
    #     ABBR == "QB" ~ "QC",
    #     ABBR == "Yukon Territory" ~ "YT",
    #     ABBR == "YK" ~ "YT",
    #     ABBR == 'Yukon Territory' ~ "YT",
    #     ABBR == 'Sonora' ~ 'SO',
    #     ABBR == 'Baja California Sur' ~ "BS",
    #     ABBR == "Northwest Territories" ~ "NT",
    #     ABBR == "NWT" ~ "NT",
    #     ABBR == "W" ~ "WA",
    #     ABBR == "LI" ~ "LA",
    #     ABBR == "KA" ~ "KS",
    #     T ~ ABBR
    #   )) |>
  source_dat = dat |>
    dplyr::filter(Year == report.year) |>
    dplyr::rename(ABBR = Previous_Waterbody_1_Province_Or_State)

  #Join total insp, HR insp, MS insp, and commercially hauled insp, to north america shapefile.
  if(verbose) cat('\nJoining total inspections, HR inspections, MF inspections and commercially hauled inspections to states/provs')


  insp_by_source = suppressWarnings(na_sf |>
    dplyr::mutate(ABBR = stringr::str_remove(HASC_1, "[A-Z]{2}\\.")) |>
    dplyr::left_join(source_dat |>
                       dplyr::count(ABBR) |>
                       dplyr::rename(TotalInsp = n),
                     by = dplyr::join_by(ABBR)) |>
    dplyr::left_join(source_dat |>
                       dplyr::filter(High_Risk_AIS_Ind == T,
                         Clean_Drain_Dry_After_Inspection_Ind == F) |>
                       dplyr::count(ABBR) |>
                       dplyr::rename(HRInsp = n),
                     by = dplyr::join_by(ABBR)) |>
    dplyr::left_join(source_dat |>
                       dplyr::filter(MusselsFound_Ind == T) |>
                       dplyr::count(ABBR) |>
                       dplyr::rename(MFInsp = n),
                     by = dplyr::join_by(ABBR)) |>
    dplyr::left_join(source_dat |>
                       dplyr::filter(Commercially_Hauled_Ind == T) |>
                       dplyr::count(ABBR) |>
                       dplyr::rename(CHInsp = n),
                     by = dplyr::join_by(ABBR)) |>
      sf::st_transform(crs = 4326) |>
      sf::st_centroid())

  # Remove inspections by source centroid geopackage file manually (was erroring occasionally otherwise)
  file.remove(paste0(this.years.report.folder,"data/spatial/Inspections_by_source_centroid.gpkg"))

  sf::write_sf(insp_by_source,
           paste0(this.years.report.folder,"data/spatial/Inspections_by_source_centroid.gpkg"))
  if(verbose) cat("\nInspections summarised by source state/prov centroid geopackage written to this year's report folder")

  #Make label shapefiles...
  #No inspections at all.
  blank_state_prov_labels = suppressWarnings(na_sf |>
    dplyr::mutate(ABBR = stringr::str_remove(HASC_1, "[A-Z]{2}\\.")) |>
    dplyr::filter(!ABBR %in% (insp_by_source |>
                                             dplyr::filter(!is.na(TotalInsp)) |>
                                             dplyr::pull(ABBR))) |>
    dplyr::filter(NAME_0 != "Mexico") |>
    sf::st_centroid())

  sf::write_sf(blank_state_prov_labels, paste0(this.years.report.folder,"data/spatial/No_inspection_states_labels.shp"))

  #No High-risk inspections.
  suppressWarnings(
    na_sf |>
    dplyr::mutate(ABBR = stringr::str_remove(HASC_1, "[A-Z]{2}\\.")) |>
    dplyr::filter(!ABBR %in% (insp_by_source |>
                                dplyr::filter(!is.na(HRInsp)) |>
                                dplyr::pull(ABBR))) |>
    dplyr::filter(NAME_0 != "Mexico") |>
    sf::st_centroid() |>
    sf::write_sf(paste0(this.years.report.folder,"data/spatial/No_HR_inspection_states_labels.shp")))

    #No Mussel-fouled inspections.
  suppressWarnings(
    na_sf |>
    dplyr::mutate(ABBR = stringr::str_remove(HASC_1, "[A-Z]{2}\\.")) |>
    dplyr::filter(!ABBR %in% (insp_by_source |>
                                dplyr::filter(!is.na(MFInsp)) |>
                                dplyr::pull(ABBR))) |>
    dplyr::filter(NAME_0 != "Mexico") |>
    sf::st_centroid() |>
    sf::write_sf(paste0(this.years.report.folder,"data/spatial/No_MF_inspection_states_labels.shp")))

    #No Commercially-hauled inspections.
    suppressWarnings(
      na_sf |>
        dplyr::mutate(ABBR = stringr::str_remove(HASC_1, "[A-Z]{2}\\.")) |>
        dplyr::filter(!ABBR %in% (insp_by_source |>
                                    dplyr::filter(!is.na(CHInsp)) |>
                                    dplyr::pull(ABBR))) |>
        dplyr::filter(NAME_0 != "Mexico") |>
        sf::st_centroid() |>
        sf::write_sf(paste0(this.years.report.folder,"data/spatial/No_CH_inspection_states_labels.shp")))

    # And also to write out inspection data joined to stations.
    insp_by_station = suppressWarnings(
      suppressMessages(
      stations |>
      dplyr::filter(map_label != "Penticton Roving") |>
      dplyr::rename(Station = map_label) |>
      dplyr::left_join(dat |> dplyr::group_by(Station,Year,Province_Code) |> dplyr::count(name = "TotalInspections")) |>
      dplyr::left_join(dat |> dplyr::group_by(Station,Year,Province_Code) |> dplyr::filter(MusselsFound_Ind == T) |> dplyr::count(name = "MusselFouled")) |>
      dplyr::left_join(dat |> dplyr::group_by(Station,Year,Province_Code) |> dplyr::filter(High_Risk_AIS_Ind == T) |> dplyr::count(name = "HighRisk")) |>
      dplyr::left_join(dat |> dplyr::group_by(Station,Year,Province_Code) |>
                         dplyr::filter(Province_Code != "Unknown") |>
                         dplyr::summarise(number_in_provs = dplyr::n()) |>
                         dplyr::summarise(number_of_provs = dplyr::n())) |>
        dplyr::distinct() |>
        dplyr::group_by(Station,Year) |>
        dplyr::mutate(row.number = dplyr::row_number()) |>
        dplyr::select(Year, tidyr::everything()) |>
        dplyr::select(-Province_Code,-row.number) |>
        dplyr::group_by(Year,Station) |>
        dplyr::mutate(TotalInspections = sum(TotalInspections, na.rm=T),
                      MusselFouled = sum(MusselFouled, na.rm=T),
                      HighRisk = sum(HighRisk, na.rm=T)) |>
        dplyr::distinct() |>
        #We're missing a couple rows of coordinates... let's get them.
        sf::st_transform(crs = 4326)
      ))

    sf::write_sf(insp_by_station,
                 paste0(this.years.report.folder,"data/spatial/Inspections_Summarised_at_Station_level.gpkg"))
    if(verbose) cat("\nUpdated the inspections summarised at station level geopackage in this year's report folder on the LAN drive.")

  #Combine the two fields containing closest city info: "Destination_Waterbody_1_Closest_City" and "Destination_Major_City".
  dat = dat |>
    dplyr::mutate(CityName = dplyr::case_when(
      #When we have info for the closest city field, use that.
      !is.na(Destination_Waterbody_1_Closest_City) & Destination_Waterbody_1_Closest_City != "Other" ~ Destination_Waterbody_1_Closest_City,
      #If we lack closest city but have info in 'Destination_Major_City' (more coarse detail), use that.
      is.na(Destination_Waterbody_1_Closest_City) | Destination_Waterbody_1_Closest_City == "Other" ~ Destination_Major_City,
      T ~ "Unknown"
    )) |>
    dplyr::mutate(CityName = tidyr::replace_na(CityName, "Unknown")) |>
    dplyr::mutate(CityName = stringr::str_remove_all(CityName, ",.*"))

  #For inspections that lack a destination waterbody name, use the BC geocoder to get lat/long coordinates for city names. If the dest city is outside BC, don't get lat/long for those, in case there's a tiny town in BC with the same name and we incorrectly get the coordinates for that town.
  city_names_for_coords = dplyr::tibble(dat |>
                                          dplyr::filter(is.na(Destination_Waterbody_1_Name)) |>
                                          dplyr::filter(CityName != "Unknown") |>
                                   #Get rid of any that are outside BC. We'll flag those when we make the DestRegion field.
                                     dplyr::filter(stringr::str_detect(Destination_Major_City, ", British Columbia")) |>
                                     dplyr::select(CityName) |>
                                     dplyr::distinct())

  #Create new variables that we will fill with the loop below.
  city_names_for_coords$lon = 0
  city_names_for_coords$lat = 0

  #This loop uses the BC geocoder to find the most likely coordinates
  # for each of the unique place names.
  if(verbose) cat("\nGeocoding the city names contained in Destination Major City field.")

  suppressWarnings(
    for(i in 1:nrow(city_names_for_coords)){
    # Pull out place name.
    my.name = city_names_for_coords[i,]$CityName
    #Clean up names. Remove anything in brackets.
    my.name = stringr::str_remove_all(my.name, " \\(.*\\)")
    #Add spaces to names.
    my.name = stringr::str_replace_all(my.name, " ", "%20")

    url = paste0('https://geocoder.api.gov.bc.ca/addresses.json?addressString=',
                 my.name,'&maxResults=1&outputSRS=4326')

    my.coords = jsonlite::fromJSON(url)$features$geometry |>
      dplyr::summarise(lon = stringr::str_extract(coordinates, "(?<=c\\().*(?=\\,)"),
                       lat = stringr::str_extract(coordinates, "(?<=\\,).*(?=\\))"))

    city_names_for_coords[i,]$lon = as.numeric(my.coords$lon)
    city_names_for_coords[i,]$lat = as.numeric(my.coords$lat)
    }
  )

  if(verbose) cat("\nFinished geocoding!")

  #Find out which FLRNO region each of these city coords fall into.
  flnro_regions = sf::read_sf(paste0(my_opts$remote_spatial_data,"shared_data_sets/FLNRO_Fishing_Boundaries.shp"))

  city_names_for_coords = suppressWarnings(
    city_names_for_coords |>
    sf::st_as_sf(coords = c("lon","lat"), crs = 4326) |>
    sf::st_transform(crs = 3005) |>
    sf::st_join(flnro_regions, sf::st_intersects)
  )

  #Join those coordinates to dat_all. For inspections with Unknown nearest city, those remain blank for lat and long.
  dat = suppressMessages(
    dat |>
    dplyr::left_join(city_names_for_coords |>
                sf::st_drop_geometry() |>
                  dplyr::rename(City_region_g = REGION_G,
                                City_region_n = REGION_N))
  )

  # Subset multiyear dataset for this years data}
  #Set aside the records for mussel-fouled boats.
  dat_mf = dat |> dplyr::filter(MusselsFound_Ind == T)

  #And set aside the records for high-risk boats.
  dat_hr = dat |> dplyr::filter(High_Risk_AIS_Ind == T) |>
    dplyr::filter(Clean_Drain_Dry_After_Inspection_Ind == F | Year < 2020)

  dat_this_year = dat |> dplyr::filter(Year == report.year)

  # sort_into_destination_regions
  #We also need to pull together the destination waterbody region from a couple columns.
  dat_this_year[dat_this_year$DestNotBC == 'FALSE',]$DestNotBC = NA
  dat_this_year[dat_this_year$OceanBoat == 'FALSE',]$OceanBoat = NA
  dat_this_year[dat_this_year$DryStorage == 'FALSE',]$DryStorage = NA

  dat_this_year = dat_this_year |>
    #Combine the 2 categories of destinations that aren't a waterbody.
    dplyr::mutate(Dest_not_wb = dplyr::coalesce(DestNotBC,DryStorage)) |>
    dplyr::mutate(DestRegion = dplyr::case_when(
      #If we have the destination name, use that.
      !is.na(Destination_Waterbody_1_Name) ~ Destination_Waterbody_1_Name,
      #If we don't have name, but have city, we'll use the city region we found above.
      is.na(Destination_Waterbody_1_Name) & !is.na(City_region_n) ~ City_region_n,
      #If we have no name or city, and either of the categories flagged (e.g. "DestNotBC"), use those.
      Dest_not_wb == T ~ "Outside BC",
      DryStorage == T ~ "Dry Storage",
      T ~ "Unknown"))

  #Same thing for our little mussel-fouled table...
  dat_mf[dat_mf$DestNotBC == 'FALSE',]$DestNotBC = NA
  dat_mf[dat_mf$OceanBoat == 'FALSE',]$OceanBoat = NA
  dat_mf[dat_mf$DryStorage == 'FALSE',]$DryStorage = NA

  dat_mf = dat_mf |>
    #Combine the 2 categories of destinations that aren't a waterbody.
    dplyr::mutate(Dest_not_wb = dplyr::coalesce(DestNotBC,DryStorage)) |>
    dplyr::mutate(DestRegion = dplyr::case_when(
      #If we have the destination name, use that.
      !is.na(Destination_Waterbody_1_Name) ~ Destination_Waterbody_1_Name,
      #If we don't have name, but have city, we'll use the city region we found above.
      is.na(Destination_Waterbody_1_Name) & !is.na(City_region_n) ~ City_region_n,
      #If we have no name or city, and either of the categories flagged (e.g. "DestNotBC"), use those.
      Dest_not_wb == T ~ "Outside BC",
      DryStorage == T ~ "Dry Storage",
      T ~ "Unknown"))

  #And same thing for high-risk inspections.
  #Same thing for our little mussel-fouled table...
  dat_hr[dat_hr$DestNotBC == 'FALSE',]$DestNotBC = NA
  dat_hr[dat_hr$OceanBoat == 'FALSE',]$OceanBoat = NA
  dat_hr[dat_hr$DryStorage == 'FALSE',]$DryStorage = NA

  dat_hr = dat_hr |>
    #left_join(dat_hr |>
    #Combine the 2 categories of destinations that aren't a waterbody.
    dplyr::mutate(Dest_not_wb = dplyr::coalesce(DestNotBC,DryStorage)) |>
    dplyr::mutate(DestRegion = dplyr::case_when(
      #If we have the destination name, use that.
      !is.na(Destination_Waterbody_1_Name) ~ Destination_Waterbody_1_Name,
      #If we don't have name, but have city, we'll use the city region we found above.
      is.na(Destination_Waterbody_1_Name) & !is.na(City_region_n) ~ City_region_n,
      #If we have no name or city, and either of the categories flagged (e.g. "DestNotBC"), use those.
      Dest_not_wb == T ~ "Outside BC",
      DryStorage == T ~ "Dry Storage",
      T ~ "Unknown"))

  #select(Watercraft_Risk_Assessment_ID,DestRegion))

  setwd(paste0(my_opts$zqm_figure_local_folder,'data/'))

  # Output data that will be used to make figures.
  openxlsx::write.xlsx(dat_this_year, "./figure_dat.xlsx", overwrite = T)
  openxlsx::write.xlsx(dat, "./figure_dat_all.xlsx", overwrite = T)
  openxlsx::write.xlsx(dat_hr, "./figure_dat_hr.xlsx", overwrite = T)
  openxlsx::write.xlsx(dat_mf, "./figure_dat_mf.xlsx", overwrite = T)

  readr::write_csv(dat, "./figure_dat_all.csv")

  # Reset working directory.
  setwd(original_wd)
}
