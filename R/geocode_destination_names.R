geocode_destination_names = function(
    dat,
    my.data.folder,
    redo_geocoding,
    verbose){
  if(file.exists(paste0(my.data.folder,"DestinationPlacenames.xlsx")) & redo_geocoding == F) {
    if(verbose) cat("\nExcel file of destination placenames detected - reading in...")
    DestinationPlacenames = readxl::read_excel(paste0(my.data.folder,"DestinationPlacenames.xlsx"))
  } else {
    if(verbose) cat("\nMaking excel sheet of destination placenames - will need to use BC Geocoder; this step takes 10 - 15 minutes.")

    estimated_finish_time = Sys.time() + lubridate::minutes(20)

    suppressWarnings(
      cat(paste0("\nRough ETA for geocoding completion: ",lubridate::hour(estimated_finish_time),":",lubridate::minute(estimated_finish_time)))
    )

    #List of all unique place names to which boats are headed.
    DestinationPlacenames = data.frame(names = unique(c(unique(dat$Destination_Waterbody_1_Closest_City),
                                                        unique(dat$Destination_Waterbody_2_Closest_City),
                                                        unique(dat$Destination_Waterbody_3_Closest_City),
                                                        unique(dat$Destination_Major_City))))

    #Create new variables that we will fill with the loop below.
    DestinationPlacenames$lon = 0
    DestinationPlacenames$lat = 0

    #This loop uses the BC geocoder to find the most likely coordinates
    # for each of the unique place names.

    # There is a max number of searches allowed in each call to the
    # BC Geocoder... so we need to split things up here.
    destination_placenames_pieces = DestinationPlacenames |>
      dplyr::mutate(piece_id = ceiling(dplyr::row_number() / 100)) |>
      dplyr::group_by(piece_id) |>
      dplyr::group_split()

    number_pieces = length(destination_placenames_pieces)

    piece_percentage = ceiling(100 / number_pieces)

    cat('\n|')
    DestinationPlacenames = lapply(
      destination_placenames_pieces, \(x) ~ {

        cat(paste0('=',piece_percentage,'%=|'))

        for(i in 1:nrow(x)){

          # Pull out place name.
          my.name = DestinationPlacenames[i,]$names
          #Clean up names. Remove anything in brackets.
          my.name = stringr::str_remove_all(my.name, " \\(.*\\)")
          #Add spaces to names.
          my.name = stringr::str_replace_all(my.name, " ", "%20")

          url = paste0('https://geocoder.api.gov.bc.ca/addresses.json?addressString=',
                       my.name,'&maxResults=1&outputSRS=4326')

          my.coords = jsonlite::fromJSON(url)$features$geometry |>
            dplyr::summarise(lon = stringr::str_extract(coordinates, "(?<=c\\().*(?=\\,)"),
                             lat = stringr::str_extract(coordinates, "(?<=\\,).*(?=\\))"))

          x[i,]$lon = my.coords$lon
          x[i,]$lat = my.coords$lat
        }
      }
    ) |>
      dplyr::bind_rows() |>
      dplyr::filter(!is.na(names))

    openxlsx::write.xlsx(DestinationPlacenames,
                         paste0(my.data.folder,"DestinationPlacenames.xlsx"),
                         overwrite = T)
  }
  return(DestinationPlacenames)
}
