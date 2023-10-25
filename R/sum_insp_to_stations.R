sum_insp_to_stations = function(dat){
  # sum to stations
  dat_stations = sum_to_stations(dat)

  sf::write_sf(dat_stations, 'dat_stations.gpkg')
}
