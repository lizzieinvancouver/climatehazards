## Started 29 March 2023 ##
## By Lizzie ##

## f(x)s for climate data manipulation in the climate hazards project


if(FALSE){
filelistmin <-  paste("input/ERA5LAND/ERA5LAND_tmn_", c(1970:2000), "_dly.fit", sep="")
filelistmax <-   paste("input/ERA5LAND/ERA5LAND_tmx_", c(1970:2000), "_dly.fit", sep="")
yearlist <- c(1970:2000)
}

# This funtion calculates the mean of min and max files then makes into a long format
fromfiletotmeanalldays <- function(filelistmin, filelistmax, yearlist){
resultz <- data.frame()
# loop through each file in the list
for (i in c(1:length(filelistmin))) {
  # read in the data file
    datamin <- fread(filelistmin[i])
    datamin <- as.matrix(datamin)
    datamax <- fread(filelistmax[i])
    datamax <- as.matrix(datamax)
  for (j in 1:nrow(datamin)) {
    # get the latitude and longitude for the site
      lat <- datamin[j,1]
      lon <- datamin[j,2]
      latmax <-  datamax[j,1]
      if(identical(lat, latmax)==FALSE) {print("Ahh! Latitudes not the same across files!")}
      dataminsm <- datamin[,-c(1:2)]
      datamaxsm <- datamax[,-c(1:2)]
      # now get the mean ...
      datamean <- (dataminsm[j,]+datamaxsm[j,])/2
      # now get the date and flip to long format
      dayz  <- format(as.Date(paste(yearlist[i],  1:length(datamean), sep="-"), "%Y-%j"))
      # add the results to the data frame
      resultz <- rbind(resultz, data.frame(lat=rep(lat, length(datamean)), lon=rep(lon, length(datamean)),
          year=rep(yearlist[i], length(datamean)), date=dayz, tmean=datamean))
     }
   }
return(resultz)
}



