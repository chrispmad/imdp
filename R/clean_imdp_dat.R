#' Title
#'
#' @param dat
#' @param my_opts
#' @param abbrev
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
clean_imdp_dat = function(dat, my_opts, abbrev, verbose){

  # Set up output folders.
  my.output.folder = paste0(my_opts$base_dir,"01_DataCleaning/output/")
  my.external.output.folder = paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Multiyear data/")

  #Data folders.
  my.data.folder = paste0(my_opts$base_dir,"01_DataCleaning/data/")
  my.external.data.folder = "W:/CMadsen/shared_data_sets/"

  # Adjust working directory.
  setwd(paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Multiyear data/"))

  # cleaning steps from script 01.

  # Clean up high risk field.
  dat = dat |>
    dplyr::mutate(High_Risk_AIS_Ind = dplyr::case_when(
      is.na(High_Risk_AIS_Ind) ~ "false",
      High_Risk_AIS_Ind == "TRUE" ~ "true",
      High_Risk_AIS_Ind == "FALSE" ~ "false",
      T ~ High_Risk_AIS_Ind))

  # Destination Water Body Name cleaning -
  # some names have typos. Add spaces and homogenize capitalization.
  dat = dat |>
    dplyr::arrange(Watercraft_Risk_Assessment_ID) |>
    #Clean up lake names.
    dplyr::mutate(Destination_Waterbody_1_Name = stringr::str_replace(Destination_Waterbody_1_Name,
                                                      "([0-9]+)([a-zA-Z]+)",
                                                      "\\1 \\2")) |>
    dplyr::mutate(Destination_Waterbody_1_Name = stringr::str_to_title(Destination_Waterbody_1_Name))

  dat = dat |>
    tidyr::pivot_longer(cols = c("Destination_Waterbody_1_Name",
                          "Destination_Waterbody_2_Name",
                          "Destination_Waterbody_3_Name")) |>
    dplyr::mutate(value = stringr::str_remove(value, ", Bc")) |>
    dplyr::mutate(value = stringr::str_to_title(value)) |>
    tidyr::pivot_wider(names_from = name, values_from = value) |>
    dplyr::arrange(desc(Year),Watercraft_Risk_Assessment_ID)

  #Homogenize station name spellings.
  dat = dat |>
    dplyr::mutate(Station = stringr::str_to_title(Station)) |>
    dplyr::mutate(Station = dplyr::case_when(
      stringr::str_detect(Station, "Cascade") ~ "Cascade Border",
      stringr::str_detect(Station, "Cutts") ~ "Cutts (Hwy 93)",
      stringr::str_detect(Station, "Hwy 93 Columbia Lake") ~ "Cutts (Hwy 93)",
      stringr::str_detect(Station, "Christina Lake") ~ "Christina Lake",
      stringr::str_detect(Station, "Dawson Creek") ~ "Dawson Creek",
      stringr::str_detect(Station, "Dry") ~ "Dry Gulch",
      stringr::str_detect(Station, "Olsen") ~ "Olsen",
      stringr::str_detect(Station, "Pacific") ~ "Pacific",
      stringr::str_detect(Station, "Keremeos (Hwy 3)") ~ "Keremeos",
      stringr::str_detect(Station, "Paulson") ~ "Paulson Summit",
      stringr::str_detect(Station, "Penticton") ~ "Penticton Roving",
      stringr::str_detect(Station, "Valemount") ~ "Mt. Robson",
      stringr::str_detect(Station, "Salmo") ~ "Salmo",
      # stringr::str_detect(Station, "^Scheduled") ~ "Scheduled Inspection",
      # stringr::str_detect(Station, 'Sumas') ~ 'Fraser Valley Roving',
      stringr::str_detect(Station, "Covid") ~ "COVID Border",
      stringr::str_detect(Station, "Okanagan") ~ "Okanagan",
      TRUE ~ Station)
    )

  # ==============================================================
  # Update the older data's watercraft type to the 4
  # classifications used in recent years.

  dat = update_old_data_watercraft_type(dat)

  # ====================================================================
  #  Correct Destination Water bodies that were incorrectly spelled and
  #  classify inspections where the boat is actually headed outside of
  #  BC. I prepared this excel sheet by exporting a list of destination
  #  waterbody names that did not match with any waterbody in the BC water body shapefile.
  #  (the shapefile I refer to is an amalgamation I made of lakes, rivers,
  #  and man-made waterbodies in BC)

  name_corr = readxl::read_excel("WatercraftInspections_no_WB_NameMatch.xlsx") |>
    dplyr::select(-WATERSHED)

  #Add a column with the corrected destination wb names.
  dat = suppressMessages(
    dat |>
    dplyr::left_join(name_corr |>
                       dplyr::rename(Destination_Waterbody_1_Name = Non_Matching_Name)) |>
    dplyr::left_join(name_corr |>
                dplyr::rename(Destination_Waterbody_2_Name = Non_Matching_Name,
                       Corrected_Name2 = Corrected_Name)) |>
    dplyr::left_join(name_corr |>
                dplyr::rename(Destination_Waterbody_3_Name = Non_Matching_Name,
                       Corrected_Name3 = Corrected_Name))
  )

  #Replace names for those waterbodies with incorrect names.
  dat = dat |>
    dplyr::mutate(Destination_Waterbody_1_Name = dplyr::case_when(
      is.na(Corrected_Name) == F ~ Corrected_Name,
      is.na(Corrected_Name) == T ~ Destination_Waterbody_1_Name
    )) |>
    dplyr::mutate(Destination_Waterbody_2_Name = dplyr::case_when(
      is.na(Corrected_Name2) == F ~ Corrected_Name2,
      is.na(Corrected_Name2) == T ~ Destination_Waterbody_2_Name
    )) |>
    dplyr::mutate(Destination_Waterbody_3_Name = dplyr::case_when(
      is.na(Corrected_Name3) == F ~ Corrected_Name3,
      is.na(Corrected_Name3) == T ~ Destination_Waterbody_3_Name
    ))

  #Flag boats that have unknown source province/state.
  dat = dat |>
    dplyr::mutate(UnknownSourceBoat = dplyr::case_when(
      is.na(Province_Code) & is.na(dplyr::coalesce(Previous_Waterbody_1_Province_Or_State,
                                            Previous_Waterbody_2_Province_Or_State,
                                            Previous_Waterbody_3_Province_Or_State)) ~ T,
      T ~ F
    ))

  # 2. Filter out inspections where the destination WB province is
  #    not BC (but keep records with 'unknown' as WB province/state).
  # In addition, ~ 8,000 boats are headed to waterbodies that have been
  # incorrectly classified as BC water bodies. These are flagged by "No Match"
  # in the Destination 1 Name field; let's remove these as well.

  bc_waterbodies = name_corr[name_corr$Corrected_Name != "No Match",]$Corrected_Name

  # Quick correction of province/state for some known BC waterbodies.
  dat[dat$Destination_Waterbody_1_Name %in% bc_waterbodies,]$Destination_Waterbody_1_Province_Or_State = "BC"
  dat[dat$Destination_Waterbody_1_Closest_City %in% c("Chemainus,",
                                                      "Vernon",
                                                      "Qualicum Beach",
                                                      "Langley") ,]$Destination_Waterbody_1_Province_Or_State = "BC"

  dat[dat$Destination_Waterbody_1_Closest_City %in% c("Chemainus,",
                                                      "Qualicum Beach",
                                                      "Langley") ,]$Destination_Waterbody_1_Name = "Pacific Ocean"

  # Data partition 2: Boats heading for WBs not in BC.
  dat = dat |>
    dplyr::mutate(DestNotBC = dplyr::case_when(
      Destination_Waterbody_1_Province_Or_State != "BC" |
        Destination_Waterbody_1_Name == "No Match" ~ T,
      T ~ F
    ))

  # Data partition 3 part 1: boats headed for dry storage.
  dat = dat |>
    dplyr::mutate(DryStorage = dplyr::case_when(
      Destination_Waterbody_1_Name == "Dry Storage" | Destination_Dry_Storage_Ind == "true" ~ T,
      T ~ F
    ))

  # Data partition 3 part 2: Unknown destination waterbody.
  dat = dat |> dplyr::mutate(UnknownDest = dplyr::case_when(
    Unknown_Destination_Waterbody_Ind == "true" ~ T,
    T ~ F))

  # Data partition 4: Boats headed for the Pacific (or Arctic) Ocean
  dat = dat |>
    dplyr::mutate(OceanBoat = dplyr::case_when(
      stringr::str_detect(Destination_Waterbody_1_Name, "Ocean") ~ T,
      T ~ F
    ))

  #Correct some columns from character ("true" or "false") to actual T or F.
  dat = dat |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("Ind"), ~.x == T | .x == "true"))

  #We need to clean up the field(s) that talk about which province/state a given inspection is from.
  dat = dat |>
    #Make the name shorter for now...
    dplyr::rename(Source = Previous_Waterbody_1_Province_Or_State) |>
    #Replace NA in Source with previous major city, if we have it. If not, use the province_code of the boat. If we don't have any of those, or if the Source field wasn't blank, keep it as it was.
    dplyr::mutate(NewSource = dplyr::case_when(
      (Source == "Unknown" | is.na(Source)) & !is.na(Previous_Major_City) & Previous_Major_City != "None" ~ Previous_Major_City,
      (Source == "Unknown" | is.na(Source)) & (is.na(Previous_Major_City) | Previous_Major_City == "None") ~ Province_Code,
      T ~ Source)) |>
    #If we replaced the Source field with the closest major city, we need to pull the province/state name out of the string.
    dplyr::mutate(NewSource = dplyr::case_when(
      stringr::str_detect(NewSource, ",.*,") ~ stringr::str_extract(NewSource, "(?<=, ).*(?=,)"),
      T ~ NewSource)) |>
    #Finally, change the long form (e.g. "Alberta") to abbreviated form for names (e.g. "AB").
    dplyr::left_join(abbrev |> dplyr::rename(NewSource = Province_or_State),
                     by = dplyr::join_by(NewSource)) |>
    dplyr::mutate(NewSource = dplyr::coalesce(Abbrev,NewSource)) |>
    dplyr::select(-Abbrev,-Source) |>
    dplyr::rename(Previous_Waterbody_1_Province_Or_State = NewSource)
  # #Homogenize the start time, end time, and raw timestamp columns.
  # # Didn't we do this already?
  # dat = dat |>
  #   dplyr::mutate(Start_Time = dplyr::case_when(
  #     stringr::str_detect(Start_Time, "T") ~ lubridate::as_datetime(Start_Time),
  #     stringr::str_detect(Start_Time,"^[0-9]{5}") ~ openxlsx::convertToDateTime(Start_Time),
  #     stringr::str_detect(Start_Time,"^[0-9]{4}-") ~ openxlxs::convertToDateTime(Start_Time))) |>
  #   dplyr::mutate(End_Time = dplyr::case_when(
  #     stringr::str_detect(End_Time, "T") ~ lubridate::as_datetime(End_Time),
  #     stringr::str_detect(End_Time,"^[0-9]{5}") ~ openxlsx::convertToDateTime(End_Time),
  #     stringr::str_detect(End_Time,"^[0-9]{4}-") ~ openxlsx::convertToDateTime(End_Time))) |>
  #   dplyr::mutate(TimeOfInspection = openxlsx::convertToDateTime(TimeOfInspection))

  #Cleaning up previous waterbody 1 prov/state field...
  dat = dat |>
    dplyr::rename(ABBR = Previous_Waterbody_1_Province_Or_State) |>
    dplyr::mutate(ABBR = dplyr::case_when(
      ABBR == "unknown" ~ "Unknown",
      ABBR == "Calgary" ~ "AB",
      ABBR == "Mexico" ~ "BN",
      ABBR == "Wi" ~ "WI",
      ABBR == "Radium" ~ "BC",
      ABBR == "Sk" ~ "SK",
      ABBR == "SA" ~ "SK",
      ABBR == "Ab" ~ "AB",
      ABBR == "QB" ~ "QC",
      ABBR == "Yukon Territory" ~ "YT",
      ABBR == "YK" ~ "YT",
      ABBR == 'Yukon Territory' ~ "YT",
      ABBR == 'Sonora' ~ 'SO',
      ABBR == 'Baja California Sur' ~ "BS",
      ABBR == "Northwest Territories" ~ "NT",
      ABBR == "NWT" ~ "NT",
      ABBR == "W" ~ "WA",
      ABBR == "LI" ~ "LA",
      ABBR == "KA" ~ "KS",
      T ~ ABBR
    )) |>
    dplyr::rename(Previous_Waterbody_1_Province_Or_State = ABBR)

  # I noticed that the field 'Number_Of_People_In_Party' should be made numeric
  # and we should use floor() on it, since some rows have reported "2.34 people".
  dat = dat |>
    dplyr::mutate(Number_Of_People_In_Party = floor(as.numeric(Number_Of_People_In_Party)))

  #ii) backup of all high-risk boats.
  highrisk_dat = dat |>
    dplyr::filter(High_Risk_AIS_Ind == T)

  #iii) backup of all mussel-fouled boats.
  musselfouled_dat = dat |>
    dplyr::filter(MusselsFound_Ind == T)

  dat_select_columns = dat |>
    dplyr::select(Year,
           Watercraft_Risk_Assessment_ID,
           MusselsFound_Ind,
           High_Risk_AIS_Ind,
           High_Risk_Area_Ind,
           Station,
           Start_Time,
           End_Time,
           TimeOfInspection,
           Shift_Start_Comment,
           Shift_End_Comment,
           Previous_Waterbody_1_Name,
           Previous_Waterbody_1_Closest_City,
           Previous_Waterbody_1_Province_Or_State,
           Previous_Waterbody_1_Other_Details,
           Unknown_Previous_Water_Body_Ind,
           Previous_Major_City,
           Destination_Waterbody_1_Name,
           Destination_Waterbody_1_Closest_City,
           Destination_Waterbody_1_Province_Or_State,
           Destination_Waterbody_1_Other_Details,
           Destination_Major_City,
           Non_Motorized_Counter,
           Simple_Counter,
           Complex_Counter,
           Very_Complex_Counter,
           Number_Of_People_In_Party,
           Non_Motorized_Blow_Bys_Counter,
           Motorized_Blow_Bys_Counter,
           Total_BlowBys,
           Province_Code,
           Decontamination_Performed_Ind,
           Decontamination_order_issued_Ind,
           Quarantine_Period_Issued_Ind,
           Commercially_Hauled_Ind,
           New_Passport_Issued_Ind,
           Passport_Holder_Ind,
           Previous_Inspection_Ind,
           Dreissenid_mussels_found_during_previous_inspection,
           Previous_Inspection_Days_Count,
           Previous_AIS_Knowledge_Ind,
           Previous_AIS_Knowledge_Source_Code_ID,
           Clean_Drain_Dry_After_Inspection_Ind,
           Seal_Issued_Ind,
           Quarantine_Period_Issued_Ind,
           General_Comment,
           UnknownSourceBoat,
           DestNotBC,
           DryStorage,
           Unknown_Destination_Waterbody_Ind,
           OceanBoat)

  return(list(dat, dat_select_columns, highrisk_dat, musselfouled_dat))
}
