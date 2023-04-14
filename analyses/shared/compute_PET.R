
# Function adapted by V. Van der Meersch from Evapotranspiration package (created by Danlu Guo)
# Added a modification to correct polar day/night effect

# Penman Monteith method (FAO standard)
# physically based PenmanMonteith (PM) approach is presently considered as the state-of-the art 

# data: list of needed data: j (DOY), Tmin, Tmax, Tdew, rs (global radiation), uz (wind speed)
# elev : ground elevation above mean sea level in m
# lat_rad: latitude in radians
# alpha: albedo (0.23 = FAO reference crop)

# needed constants:
PET_constants <- data.frame(lambda = 2.45, Gsc = 0.082, sigma = 4.903e-09, G = 0, z = 10)
# lambda : latent heat of vaporisation 
# Gsc : solar constant (MJ m-2 min-1)
# sigma : Stefan-Boltzmann constant
# G : soil heat flux (= 0 with daily time step)
# z : height of wind instrument in m (= 10m in ERA5-Land data)


compute_PET <- function(data, elev, lat_rad, constants = PET_constants, alpha = 0.23){
    
  Tmean <- (data$Tmax + data$Tmin)/2 # following FAO
    
  # Saturated vapour pressure (approximation)
  es_Tmax <- 0.6108 * exp(17.27 * data$Tmax/(data$Tmax + 237.3))
  es_Tmin <- 0.6108 * exp(17.27 * data$Tmin/(data$Tmin + 237.3))
  es <- (es_Tmax + es_Tmin)/2
    
  # Actual vapour pressure
  ea <- 0.6108 * exp(17.27 * data$Tdew/(data$Tdew + 237.3))
    
  P <- 101.3 * ((293 - 0.0065 * elev)/293)^5.26 # atmospheric pressure
  # effect of atmospheric pressure (i.e. evaporation at high altitudes promoted due to low atmospheric pressure) is small
  # thus, the average value of atmospheric pressure for a location is sufficient
    
  delta <- 4098 * (0.6108 * exp((17.27 * Tmean)/(Tmean + 237.3)))/((Tmean + 237.3)^2) # slope of vapour pressure curve
  gamma <- 0.00163 * P/constants$lambda # psychrometric constant
    
    
  # Approximate net radiation
  d_r2 <- 1 + 0.033 * cos(2 * pi/365 * data$J) # dr is the inverse relative distance Earth-Sun
  delta2 <- 0.409 * sin(2 * pi/365 * data$J - 1.39) # solar dedication
    
  # Added by V. V. (correct NA values)
  aux <- -tan(lat_rad) * tan(delta2) 
  aux[aux > 1] <- 1
  aux[aux < -1] <- -1
  
  # sunset hour angle
  w_s <- acos(aux) 
  N <- 24/pi * w_s
  
  # extraterrestrial radiation
  R_a <- (1440/pi) * d_r2 * constants$Gsc * (w_s * sin(lat_rad) * 
                                                 sin(delta2) + cos(lat_rad) * cos(delta2) * 
                                                 sin(w_s))
  # clear sky radiation
  R_so <- (0.75 + (2 * 10^-5) * elev) * R_a 
    
  # global radiation (input data)
  R_s <- data$rs
    
  # estimated net outgoing longwave radiation
  R_nl <- constants$sigma * (0.34 - 0.14 * sqrt(vabar)) * 
      ((data$Tmax + 273.2)^4 + (data$Tmin + 273.2)^4)/2 * 
      (1.35 * R_s/R_so - 0.35) 
    
  # net incoming shortwave radiation - water or other evaporative surface with specified albedo
  R_nsg <- (1 - alpha) * R_s 
    
  # net radiation (net incoming shortwave - net outgoing longwave)
  R_ng <- R_nsg - R_nl
    
  # calculate wind speed at 2m height
  u2 <- data$uz * 4.87/log(67.8 * constants$z - 5.42)
    
  # potential evapotranspiration (FAO equation)
  PET  <- (0.408 * delta * (R_ng - constants$G) + gamma * 900 * u2 * (vas - vabar)/(Tmean + 273))/(delta + gamma * (1 + 0.34 * u2))
    
  
  return(unlist(PET))
  
}




