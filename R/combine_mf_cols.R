combine_mf_cols = function(dat){
  dat |>
    dplyr::mutate(MusselsFound_Ind = dplyr::case_when(
      Adult_Dressenidae_Found_Ind == "true" | Adult_Dreissenidae_Mussel_Found_Ind == "true" ~ T,
      T ~ F)) |>
    dplyr::select(-Adult_Dressenidae_Found_Ind, -Adult_Dreissenidae_Mussel_Found_Ind)
}
