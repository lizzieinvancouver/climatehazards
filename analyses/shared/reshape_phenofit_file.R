# Script to reshape Phenofit climate datafile
# in a dataframe of five columns: lat, lon, value, year, DOY

wd <- "D:/climate/ERA5-Land/phenofit_format"
varname <- "tmp" # variable name in phenofit format

data <- do.call(rbind,lapply(list.files(path = wd, pattern = varname, full.names = TRUE), function(i){
  yr_data <- read.table(i, skip = 4) # read data, skipping Phenofit header
  nsites <- nrow(yr_data)
  ndays <- ncol(yr_data)-2
  yr_data <- yr_data %>%  
    tidyr::gather(key = "variable", value = "value", -c(1,2))%>% # collapse columns into rows
    dplyr::select(-variable) %>%
    dplyr::rename(lat = "V1", lon = "V2")
  yr_data$year <- as.numeric(strsplit(i, split= '_')[[1]][4]) # get the year (from the filename)
  yr_data$doy <- rep(1:ndays, each = nsites) # get the day of the year (1:365/366)
  return(yr_data)
}))