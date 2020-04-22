## Utility functions

percent_yield <- function(pre, post){
  yield = ((pre - post)/ pre) * 100
}

write_datafile <- function(file, outdir, outfilename){
  outpath <- file.path(outdir, outfilename)
  print(outpath)
  write.csv(x = file, file = outpath, row.names = FALSE)
}

append_newdata <- function(labdat, new_data){
  df <- bind_rows(labdat, new_data)
  return(df)
}

append_calc_values <- function(labdat, labdat_calcs){
  
  df <- bind_rows(labdat, labdat_calcs)
  return(df)
}