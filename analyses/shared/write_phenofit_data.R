
#var: variable name in Phenofit (e.g. "tmp")
#yr: year considered
#data: table already formatted (lat, lon, 365/366 values)

write_phenofit_data <- function(var, yr, data, 
                                folder, prefix = "ERA5LAND"){
  
  # File name
  processed_file <- file.path(folder, paste0(prefix, "_", var, "_", yr, "_dly.fit"))
  
  # Write data
  con <- file(processed_file, open="wt")
  writeLines("Climate datafile for Phenofit model", con)
  writeLines(paste0("Created on RStudio, by user ", Sys.getenv("USERNAME"),", on the ", Sys.Date()), con)
  comments <- get_comments(var = var) # metadata
  writeLines(comments, con)
  writeLines(" ", con)
  write.table(data, file = con, sep = "\t",
              row.names = FALSE, col.names= FALSE)
  close(con)
  
}


.get_comments <- function(var){
  if(var=='glo'){
    return("Variable : daily global radiation (MJ/m²)")
  }
  else if(var=='pre'){
    return("Variable : daily precipitation, comprising rain and snow (mm)")
  }
  else if(var=='RH'){
    return("Variable : daily mean 2m relative humidity (%) calculated with vapor pressure ratio (Clausius-Clapeyron relation)")
  }
  else if(var=='tmn'){
    return("Variable : daily mimimal 2m temperature (°C)")
  }
  else if(var=='tmp'){
    return("Variable : daily mean 2m temperature (°C)")
  }
  else if(var=='tmx'){
    return("Variable : daily maximal 2m temperature (°C)")
  }
  else if(var=='wnd'){
    return("Variable : daily mean 10m wind speed (m/s) - used only to compute evapotranspiration")
  }
  else if(var=='RHmin'){
    return("Variable : daily miminum 2m relative humidity (%) - used only to compute evapotranspiration")
  }
  else if(var=='RHmax'){
    return("Variable : daily maximum 2m relative humidity (%) - used only to compute evapotranspiration")
  }
  else if(var=='pet'){
    return("Variable : potential evapotranspiration (mm) calculated with Penman-Monteith formulation (FAO-56 hypothetical short grass method) ")
  }
}