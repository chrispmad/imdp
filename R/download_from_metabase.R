download_from_metabase = function(){
  # download data from metabase
  dat = download()

  # Write out as .csv
  readr::write_csv(dat, 'new_metabase_dat.csv')
}
