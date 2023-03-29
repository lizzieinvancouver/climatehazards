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




## Read in files and calculating mean and SD for every month of each year
## These I made as my very first files, but since we want to work across the data for all years for a month I am not sure I will ever need them 

# This function does it for tmin and tmax files
fromfilestomeansd <- function(filelist, yearlist){  
resultz <- data.frame()
# loop through each file in the list
for (i in c(1:length(filelist))) {
  # read in the data file
    data <- fread(filelist[i])
    data <- as.matrix(data)
    # calculate the mean and variance by month for each site
  for (j in 1:nrow(data)) {
    # get the latitude and longitude for the site
      lat <- data[j,1]
      lon <- data[j,2]
      datasm <- data[,-c(1:2)]
     # loop through each month
      for (month in 1:12) {
        # calculate the mean and variance for the month doing some gymnastics to make chatGPT's idea work)
        monthsdays  <- which(format(as.Date(paste0(yearlist[i], "-01-01"), "%Y-%m-%d") + (0:364), "%m") == sprintf("%02d", month))
        monthstartday <- monthsdays[1]
        monthendday <- monthsdays[length(monthsdays)]
        monthdata <- datasm[j, monthstartday:monthendday]
        meanhere <- mean(monthdata, na.rm=TRUE)
        sdhere <- sd(monthdata, na.rm=TRUE)
      # add the results to the data frame
      resultz <- rbind(resultz, data.frame(lat=lat, lon=lon, month=month, year=yearlist[i], mean=meanhere, sd=sdhere))
     }
   }
}
return(resultz)
}


# This funtion calculates the mean of those files and then does it.
fromfilestomeansdtmean <- function(filelistmin, filelistmax, yearlist){
resultz <- data.frame()
# loop through each file in the list
for (i in c(1:length(filelistmin))) {
  # read in the data file
    datamin <- fread(filelistmin[i])
    datamin <- as.matrix(datamin)
    datamax <- fread(filelistmax[i])
    datamax <- as.matrix(datamax)
  # calculate the mean and variance by month for each site
  for (j in 1:nrow(datamin)) {
    # get the latitude and longitude for the site
      lat <- datamin[j,1]
      lon <- datamin[j,2]
      latmax <-  datamax[j,1]
      if(identical(lat, latmax)==FALSE) {print("Ahh! Latitudes not the same across files!")}
      dataminsm <- datamin[,-c(1:2)]
      datamaxsm <- datamax[,-c(1:2)]
      # now get the mean ...
      datamean <- (dataminsm+datamaxsm)/2
     # loop through each month
      for (month in 1:12) {
        # calculate the mean and variance for the month doing some gymnastics to make chatGPT's idea work)
        monthsdays  <- which(format(as.Date(paste0(yearlist[i], "-01-01"), "%Y-%m-%d") + (0:364), "%m") == sprintf("%02d", month))
        monthstartday <- monthsdays[1]
        monthendday <- monthsdays[length(monthsdays)]
        monthdata <- datamean[j, monthstartday:monthendday]
        meanhere <- mean(monthdata, na.rm=TRUE)
        sdhere <- sd(monthdata, na.rm=TRUE)
      # add the results to the data frame
      resultz <- rbind(resultz, data.frame(lat=lat, lon=lon, month=month, year=yearlist[i], mean=meanhere, sd=sdhere))
     }
   }
}
return(resultz)
}
