## Started 29 March 2023 ##
## By Lizzie ##

## f(x)s for climate data manipulation in the climate hazards project

if(FALSE){  # stuff for testing 
filelistmin <-  paste("input/ERA5LAND/ERA5LAND_tmn_", c(1970:2000), "_dly.fit", sep="")
filelistmax <-   paste("input/ERA5LAND/ERA5LAND_tmx_", c(1970:2000), "_dly.fit", sep="")
filelist <-  paste("input/ERA5LAND/ERA5LAND_tmn_", c(1970:2000), "_dly.fit", sep="")
yearlist <- c(1970:2000) # note to self for checking: 1972 is leap year
i <- 3
j <- 1
k <- 2
}


# Function to take tmin or tmax files and make into a 12 month list
readfilestolist <- function(filelist, yearlist){
resultz <- vector(mode='list', length=12)
# loop through each file in the list
for (i in c(1:length(filelist))) {
  # read in the data file
    dfhere <- fread(filelist[i])
    mathere <- as.matrix(dfhere)
  for (j in 1:nrow(mathere)) {
    # get the latitude and longitude for the site
      lat <- mathere[j,1]
      lon <- mathere[j,2]
      matherenolat <- mathere[,-c(1:2)]
      # now get the date and flip to long format
      # to get date instead: format(as.Date(paste(yearlist[i],  1:ncol(matherenolat), sep="-"), "%Y-%j")
      monthz  <- as.numeric(format(as.Date(paste(yearlist[i],  1:ncol(matherenolat), sep="-"), "%Y-%j"), "%m"))
      dfnext <- data.frame(month=monthz, temp=matherenolat[j,])
      for (k in 1:12){
          dfagain <- subset(dfnext, month==k)
          dftolist <- data.frame(lat=rep(lat, nrow(dfagain)), lon=rep(lon, nrow(dfagain)),
                                 year=rep(yearlist[i], nrow(dfagain)), day=c(1:nrow(dfagain)), tempC=dfagain$temp)
          dftolist$latlon <- paste(dftolist$lat, dftolist$lon)
          # add the results to the list
          resultz[[k]] <- rbind(resultz[[k]], dftolist)
         }
      }
    }
return(resultz)
}

# Function to take tmin or tmax files and make into a 12 month list, then anomalize them by that year x month
readfilestolistanomalies <- function(filelist, yearlist){
resultz <- vector(mode='list', length=12)
# loop through each file in the list
for (i in c(1:length(filelist))) {
  # read in the data file
    dfhere <- fread(filelist[i])
    mathere <- as.matrix(dfhere)
  for (j in 1:nrow(mathere)) {
    # get the latitude and longitude for the site
      lat <- mathere[j,1]
      lon <- mathere[j,2]
      matherenolat <- mathere[,-c(1:2)]
      # now get the date and flip to long format
      # to get date instead: format(as.Date(paste(yearlist[i],  1:ncol(matherenolat), sep="-"), "%Y-%j")
      monthz  <- as.numeric(format(as.Date(paste(yearlist[i],  1:ncol(matherenolat), sep="-"), "%Y-%j"), "%m"))
      dfnext <- data.frame(month=monthz, temp=matherenolat[j,])
      for (k in 1:12){
          dfagain <- subset(dfnext, month==k)
          anomalieshere <- dfagain$temp-mean(dfagain$temp)
          dftolist <- data.frame(lat=rep(lat, nrow(dfagain)), lon=rep(lon, nrow(dfagain)),
                                 year=rep(yearlist[i], nrow(dfagain)), day=c(1:nrow(dfagain)), tempanom=anomalieshere)
          dftolist$latlon <- paste(dftolist$lat, dftolist$lon)
          # add the results to the list
          resultz[[k]] <- rbind(resultz[[k]], dftolist)
         }
      }
    }
return(resultz)
}


# Function to get the mean and SD (from daily data) across years by month and site
# You need this to do detrending 
getmeansdbysitemonth <- function(climatelist, sitevector, startyear, endyear){
resultz <- data.frame()
for(i in 1:12){
    dfhere <- climatelist[[i]]
    dfheresm <- subset(dfhere, year<=endyear & year>=startyear)
    meanhere <- aggregate(dfheresm["tempC"], dfheresm[c("latlon", "lat", "lon")], FUN=mean)
    sdhere <- aggregate(dfheresm["tempC"], dfheresm[c("latlon", "lat", "lon")], FUN=sd)
    names(sdhere)[names(sdhere)=="tempC"] <- "sd"
    dfbuild <- cbind(meanhere, sdhere$sd, month=rep(i, nrow(meanhere)))
    names(dfbuild) <- c("latlon", "lat", "lon", "mean", "sd", "month")
    resultz <- rbind(resultz, dfbuild)
  }
return(resultz)
}

# Function to get the mean and SD (from daily data) by month and site and year
# You need this to do detrending 
getmeansdbysiteyrmonth <- function(climatelist, sitevector, startyear, endyear){
resultz <- data.frame()
for(i in 1:12){
    dfhere <- climatelist[[i]]
    dfheresm <- subset(dfhere, year<=endyear & year>=startyear)
    meanhere <- aggregate(dfheresm["tempC"], dfheresm[c("latlon", "lat", "lon", "year")], FUN=mean)
    sdhere <- aggregate(dfheresm["tempC"], dfheresm[c("latlon", "lat", "lon", "year")], FUN=sd)
    names(sdhere)[names(sdhere)=="tempC"] <- "sd"
    dfbuild <- cbind(meanhere, sdhere$sd, month=rep(i, nrow(meanhere)))
    names(dfbuild) <- c("latlon", "lat", "lon", "year", "mean", "sd", "month")
    resultz <- rbind(resultz, dfbuild)
  }
return(resultz)
}


if(FALSE){ # stuff for testing below function
i <- 3
j <- 1
sitevector <- unique(tminlist[[1]]["latlon"])
meanzbyyear <- tminsumm
meanzbysiteyearlist <- tminsummyr
}


# This function monthly mean x year and long-term mean data, detrends it by linear regression and then adds in a mean
# It writes out the means and slopes in the last dataframe (13)
detrendmonthlyclimatelist <- function(meanzbysiteyearlist, meanzbyyear, sitevector){
resultz <- vector(mode='list', length=13)            
for(i in 1:12){
    meansiteyearhereall <- subset(meanzbysiteyearlist, month==i)
    meansitehereall <- subset(meanzbyyear, month==i)
    for(j in c(1:nrow(sitevector))){
        meansiteyearhere <- subset(meansiteyearhereall, latlon==sitevector[j,])
        meanheresite <-  subset(meansitehereall, latlon==sitevector[j,])
        fithere <- lm(meansiteyearhere$mean~meansiteyearhere$year)
        # plot(meansiteyearhere$mean~meansiteyearhere$year, xlab="Daily anomalies (done by month within year)", ylab="Year")
        # abline(fithere, col="red")
        # Add long-term monthly mean to the detrended monthly temperatures 
        detrendedtemp <- as.vector(fithere$residuals)+meanheresite$mean
        dftolist <- data.frame(meansiteyearhere, detrendmean=detrendedtemp)
        somebasics <- data.frame(latlon=sitevector[j,], month=i, mean=meanheresite$mean, slope=coef(fithere)[2])
        resultz[[i]] <- rbind(resultz[[i]], dftolist)
        resultz[[13]] <- rbind(resultz[[13]], somebasics)
       }
    }
return(resultz)
}

if(FALSE){ # stuff for testing below function
detrendedmonthly <- testingpart1
climatelistofanomalies <- tminlistanom
sitevector <- unique(tminlist[[1]]["latlon"])
yearlist <- c(1950:2020)
i <- 3
j <- 1
k <- 1
}
        
getdailydetrendedlist <- function(detrendedmonthly, climatelistofanomalies, sitevector, yearlist){
resultz <- vector(mode='list', length=12)            
for(i in 1:12){
    detmonth <- detrendedmonthly[[i]]
    anommonth <- climatelistofanomalies[[i]]
    for(j in c(1:nrow(sitevector))){
        detmonthsite <- subset(detmonth, latlon==sitevector[j,])
        anommonthsite <- subset(anommonth, latlon==sitevector[j,])
        for (k in c(1:length(yearlist))){
            detmonthsiteyr <- subset(detmonthsite, year==yearlist[k])
            anommonthsiteyr <- subset(anommonthsite, year==yearlist[k])
            newdata <- anommonthsiteyr$tempanom + detmonthsiteyr$mean ## CHECK
            dftolist <- cbind(anommonthsiteyr, newdata)
            names(dftolist)[names(dftolist)=="newdata"] <- "tempC"
            resultz[[i]] <- rbind(resultz[[i]], dftolist)
        }
    }
}    
return(resultz)
}


if(FALSE){ # stuff for testing below function
i <- 3
j <- 1
sitevector <- unique(tminlist[[1]]["latlon"])
startyear <- 1950
endyear <- 2020
climatelistofanomalies <- tminlistanom
meanzbyyear <- tminsumm
}

# This function takes anomalized data (above), detrends it by linear regression and then adds in a mean
# It writes out the means and slopes in the last dataframe (13)
# OLD version ... does the whole thing on anomalies, which seems pretty WRONG
detrendclimatelistOLD <- function(climatelistofanomalies, sitevector, meanzbyyear){
resultz <- vector(mode='list', length=13)            
for(i in 1:12){
    dfhere <- climatelistofanomalies[[i]]
    meanhere <- subset(meanzbyyear, month==i)
    for(j in c(1:nrow(sitevector))){
        dfsite <- subset(dfhere, latlon==sitevector[j,])
        meanheresite <-  subset(meanhere, latlon==sitevector[j,])
        fithere <- lm(dfsite$tempanom~dfsite$year)
        # plot(dfsite$tempanom~dfsite$year, xlab="Daily anomalies (done by month within year)", ylab="Year")
        # abline(fithere, col="red")
        detrendedtemp <- as.vector(fithere$residuals)+meanheresite$mean
        dftolist <- data.frame(dfsite, tempC=detrendedtemp)
        somebasics <- data.frame(latlon=sitevector[j,], month=i, mean=meanheresite$mean, slope=coef(fithere)[2])
        resultz[[i]] <- rbind(resultz[[i]], dftolist)
        resultz[[13]] <- rbind(resultz[[13]], somebasics)
       }
    }
return(resultz)
}



# This funtion calculates the mean of min and max files then makes into a long format
# It's the first one I made, so may want to double-check
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



