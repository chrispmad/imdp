#' Title Update the older data's watercraft type.
#'
#' @param dat table of IMDP inspections
#'
#' @return table of IMDP inspections, old records with watercraft complexity types
#'
#' @examples \dontrun
update_old_data_watercraft_type = function(dat,my_opts){

  #Split watercraft type for older data at each comma, put those in
  #new rows. Convert watercraft type to one of the four types used
  #in later years. Bring those up into one row per inspection.

  #Grabbing a helpful excel doc that classifies boats into new categories.
  boattypes = readxl::read_excel(paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Multiyear data/UniqueWCTypes.xlsx")) |>
    dplyr::left_join(readxl::read_excel(paste0(my_opts$zqm_operations_data_folder,"Watercraft Inspection Data/Multiyear data/watercraft categories_06Aug2021.xlsx")) |>
                       dplyr::rename(BoatType = `watercraft type`,
                                     NewCat = `New Category`) |> dplyr::select(-Comments),
                     by = dplyr::join_by(BoatType)) |>
    dplyr::add_row(WatercraftType = "Unknown",
                   BoatType = "Unknown",
                   NewCat = "Unknown")

  dat_recent = dat |>
    dplyr::filter(Year %in% c(2019:max(Year))) |>
    dplyr::select(-WatercraftType)

  #Get the older data with many watercraft types
  dat_older = dat |>
    dplyr::filter(Year %in%
                    c("2015","2016","2017","2018"))

  #Split any entries for watercraft type that listed multiple boats
  #into separate columns.
  dat_older = suppressWarnings(dat_older |>
                                 tidyr::separate(WatercraftType, c("Type1","Type2","Type3"), ","))

  # Join Martina's boat classifier excel sheet and convert boat type to
  # how it's recorded in more recent years.
  dat_older = dat_older |>
    dplyr::select(-Non_Motorized_Counter,-Simple_Counter,
                  -Complex_Counter,-Very_Complex_Counter) |>
    #Label any NA values for first boat type column - we want to keep these.
    dplyr::mutate(Type1 = tidyr::replace_na(Type1, "Unknown")) |>
    dplyr::mutate(Type1 = replace(Type1, Type1 == "other", "Unknown")) |>
    #Join on boat type table for first boat type column.
    dplyr::left_join(boattypes |>
                       dplyr::rename(Type1 = WatercraftType,
                                     NewCat1 = NewCat) |>
                       dplyr::select(-BoatType),
                     by = dplyr::join_by(Type1)) |>
    dplyr::left_join(boattypes |>
                       dplyr::rename(Type2 = WatercraftType,
                                     NewCat2 = NewCat) |>
                       dplyr::select(-BoatType),
                     by = dplyr::join_by(Type2)) |>
    dplyr::left_join(boattypes |>
                       dplyr::rename(Type3 = WatercraftType,
                                     NewCat3 = NewCat) |>
                       dplyr::select(-BoatType),
                     by = dplyr::join_by(Type3)) |>
    tidyr::pivot_longer(cols = tidyr::starts_with("NewCat")) |>
    dplyr::filter(is.na(value)==F) |>
    dplyr::rename(BoatType = value) |>
    dplyr::group_by(Watercraft_Risk_Assessment_ID,BoatType) |>
    dplyr::mutate(NumberCraft = as.numeric(dplyr::n())) |>
    dplyr::select(-name) |>
    dplyr::distinct() |>
    tidyr::pivot_wider(names_from = "BoatType",values_from = "NumberCraft", values_fill = 0) |>
    dplyr::select(-Type1, -Type2, -Type3) |>
    dplyr::rename(Non_Motorized_Counter = `Non-motorized`,
                  Simple_Counter = Simple,
                  Complex_Counter = Complex,
                  Very_Complex_Counter = `Very complex`)

  #Recombine older and more recent datasets
  dat = dat_recent |>
    dplyr::mutate(Non_Motorized_Counter = as.numeric(Non_Motorized_Counter),
                  Simple_Counter = as.numeric(Simple_Counter),
                  Complex_Counter = as.numeric(Complex_Counter),
                  Very_Complex_Counter = as.numeric(Very_Complex_Counter)) |>
    dplyr::bind_rows(dat_older)

  rm(dat_recent);rm(dat_older);rm(boattypes)

  return(dat)
}
