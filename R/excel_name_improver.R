excel_name_improver = function(x){
  names = colnames(x)
  names = stringr::str_replace_all(names," ","_")
  names = stringr::str_replace_all(names,"\\/","_")
  names = stringr::str_remove_all(names, "\\?")
  names = stringr::str_remove_all(names, "\\#")
  names = stringr::str_replace(names, 'Destination_Province_State', 'State_Province_of_Destination_Waterbody')
  names = stringr::str_replace(names, 'Province_State_of_Destination_Waterbody', 'State_Province_of_Destination_Waterbody')
  colnames(x) <- names
  x
}
