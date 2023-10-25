append_imdp_dat = function(){

  # Check metabase for new data
  new_data = check_metabase()

  if(new_data){
    new_dat = download_new_metabase_data()

    # Clean data
    new_dat_c = clean_imdp_dat(new_dat)

    # Read in data to which we join new data.
    dat = read_dat('data_file.xlsx')

    # Join data together.
    dat_to_write = dplyr::bind_rows(
      dat,
      new_dat_c
    )

    #Write out cleaned data as excel file.
    openxlsx::write.xlsx(dat_to_write, 'data_file.xlsx')
  }
}
