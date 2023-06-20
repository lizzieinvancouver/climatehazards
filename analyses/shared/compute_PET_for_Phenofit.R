
# function to estimate Penman-Monteith potential evapotranspiration
# according to FAO standards

# adapted from the Evapotranspiration R package (created by Danlu Guo)

# years: years to compute, e.g. 1950:2000
# data_folder: folder for reading and writing data, e.g. "D:/climate/ERA5-Land/phenofit_format/climatehazards"
# climate_name: name of climate dataset, e.g. "ERA5LAND"

compute_PET_for_Phenofit <- function(years, data_folder, climate_name){
  
  cat("Processing potential evapotranspiration for Phenofit model\n")
  
  # File description
  file_description <- "Variable : potential evapotranspiration (mm) calculated with Penman-Monteith formulation (FAO-56 hypothetical short grass method)"
  
  # Fixed constants
  alpha <- 0.23 # albedo in FAO method
  lambda <- 2.45 # latent heat of vaporisation (MJ kg-1)
  Gsc <- 0.082 # solar constant (MJ m-2 min-1)
  sigma <- 4.903e-09 # Stefan-Boltzmann constant (MJ K-4 m-2 day-1)
  G <- 0 # soil heat flux (approx. = 0 with daily time step)
  z <- 10 # height of wind instrument in m (= 10m in ERA5-Land data)
  
  # Load fixed variables
  alt <- unlist(read.table(paste0(data_folder, "/", climate_name, "_Altitude.fit" ), skip = 4, sep = "\t")[-c(1,2)]) # altitude
  latlon <- as.matrix(read.table(paste0(data_folder, "/", climate_name, "_Altitude.fit" ), skip = 4, sep = "\t")[-c(3)]) # latitude/longitude
  lat <- unlist(read.table(paste0(data_folder, "/", climate_name, "_Altitude.fit" ), skip = 4, sep = "\t")[-c(2,3)]*pi/180) # latitude in radians
  
  for(yr in years){
    
    # Load yearly variables
    tmn <- as.matrix(read.table(paste0(data_folder, "/", climate_name, "_tmn_", yr, "_dly.fit" ), skip = 4, sep = "\t")[-c(1,2)])
    tmx <- as.matrix(read.table(paste0(data_folder, "/", climate_name, "_tmx_", yr, "_dly.fit" ), skip = 4, sep = "\t")[-c(1,2)])
    tmp <- as.matrix(read.table(paste0(data_folder, "/", climate_name, "_tmp_", yr, "_dly.fit" ), skip = 4, sep = "\t")[-c(1,2)])
    dtm <- as.matrix(read.table(paste0(data_folder, "/", climate_name, "_dtm_", yr, "_dly.fit" ), skip = 4, sep = "\t")[-c(1,2)])
    glo_rad <- as.matrix(read.table(paste0(data_folder, "/", climate_name, "_glo_", yr, "_dly.fit" ), skip = 4, sep = "\t")[-c(1,2)])
    wind_spd <- as.matrix(read.table(paste0(data_folder, "/", climate_name, "_wnd_", yr, "_dly.fit" ), skip = 4, sep = "\t")[-c(1,2)])
    
    doy <- matrix()
    
    doy <- t(matrix(rep(1:ncol(tmn),nrow(tmn)), ncol = nrow(tmn)))
    
    # Saturated vapour pressure
    es_tmn <- 0.6108 * exp(17.27 * tmn/(tmn + 237.3))
    es_tmx <- 0.6108 * exp(17.27 * tmx/(tmx + 237.3))
    es <- (es_tmn + es_tmx)/2
    
    # Actual vapour pressure
    ea <- 0.6108 * exp(17.27 * dtm/(dtm + 237.3))
    
    # Average value of atmospheric pressure 
    P <- 101.3 * ((293 - 0.0065 * alt)/293)^5.26
    
    # Slope of vapour pressure curve
    delta <- 4098 * (0.6108 * exp((17.27 * tmp)/(tmp + 237.3)))/((tmp + 237.3)^2)
    
    # Psychrometric constant
    gamma <- 0.00163 * P/lambda 
    
    # Solar declination
    d_r2 <- 1 + 0.033 * cos(2 * pi/365 * doy) # inverse relative distance Earth-Sun
    solar_dec <- 0.409 * sin(2 * pi/365 * doy - 1.39) 
    
    # Check: particular cases of polar days/nights
    aux <- -tan(lat) * tan(solar_dec) 
    aux[aux > 1] <- 1
    aux[aux < -1] <- -1
    
    # Sunset hour angle
    w_s <- acos(aux) 
	
	# Daylight hours 
    N <- 24/pi * w_s
    
    # Estimated TOA radiation (extraterrestrial)
    TOA_rad <- (1440/pi) * d_r2 * Gsc * (w_s * sin(lat) * sin(solar_dec) + cos(lat) * cos(solar_dec) * sin(w_s))
    
    # Estimated clear-sky global radiation
    glo_rad0 <- (0.75 + (2 * 10^-5) * alt) * TOA_rad
    
    # Estimated net outgoing longwave radiation
    nl_rad <- sigma * (0.34 - 0.14 * sqrt(ea)) * ((tmx + 273.2)^4 + (tmn + 273.2)^4)/2 * (1.35 * glo_rad/glo_rad0 - 0.35)
    
    # Net incoming shortwave radiation - evaporative surface with specified albedo
    ns_rad <- (1 - alpha) * glo_rad
    
    # Net radiation
    net_rad <- ns_rad - nl_rad
    
    # Wind speed at 2 meters
    wind_spd_2m <- wind_spd * 4.87/log(67.8 * wind_z - 5.42)
    
    # Potential evapotranspiration
    PET <- (0.408 * delta * (net_rad - G) + gamma * 900 * wind_spd_2m * (es - ea)/(tmp + 273))/(delta + gamma * (1 + 0.34 * wind_spd_2m))
    
    # Write data
    filename <- paste0(data_folder, "/", climate_name, "_pet_", yr, "_dly.fit")
    con <- file(filename, open="wt")
    writeLines("Climate datafile for Phenofit model", con)
    writeLines(paste("Created on RStudio, by user", Sys.getenv("USERNAME") ,", on the", Sys.Date()), con)
    writeLines(file_description, con)
    writeLines(" ", con)
    write.table(cbind(latlon, PET), file = con, sep = "\t",
                row.names = FALSE, col.names= FALSE)
    close(con)
    
    cat(".")
    
  }
  
  cat("\nDone!\n")
  
}
