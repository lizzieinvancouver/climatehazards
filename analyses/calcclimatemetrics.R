## Started 21 mars 2023 ##
## By Lizzie ##

## Whoo-hoo, spring! Time to finally get some analyses going on Phenofit project ##
## This codes reads in historical climate data and calculates mean and SD by month ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(data.table)
library(ggplot2)

## set working directory
setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses")

## To do!
##

justone <- fread("input/ERA5LAND/ERA5LAND_tmn_1970_dly.fit")

## Read in files and calculating mean and SD:
# First function does it for tmin and tmax files
# Second funtion calculates the mean of those files and then does it.

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


resultzmin <- fromfilestomeansd(paste("input/ERA5LAND/ERA5LAND_tmn_", c(1970:2000), "_dly.fit", sep=""), c(1970:2000))
resultzmax <- fromfilestomeansd(paste("input/ERA5LAND/ERA5LAND_tmx_", c(1970:2000), "_dly.fit", sep=""), c(1970:2000))
resultztmean <- fromfilestomeansdtmean(paste("input/ERA5LAND/ERA5LAND_tmn_", c(1970:2000), "_dly.fit", sep=""), paste("input/ERA5LAND/ERA5LAND_tmx_", c(1970:2000), "_dly.fit", sep=""), c(1970:2000))




##############
## Plotting ##
##############

resultzmin$counter <- rep(seq(1:(12*length(c(1970:2000)))), nrow(justone))

ggplot(resultzmin, aes(x=counter, y=mean, color=lat)) +
    geom_line() 

ggplot(resultzmin, aes(x=year, y=mean, group=lat, color=lat)) +
    geom_line() +
    # geom_smooth(method=lm) +
    facet_wrap(month~.)

ggplot(resultzmin, aes(x=year, y=sd, group=lat, color=lat)) +
    geom_line() +
    # geom_smooth(method=lm) +
    facet_wrap(month~.)

ggplot(resultztmean, aes(x=year, y=mean, group=lat, color=lat)) +
    geom_line() +
    # geom_smooth(method=lm) +
    facet_wrap(month~.)





# save the results to a CSV file
# write.csv(results, "results.csv", row.names=FALSE)


if(FALSE){ ## Test my code above ...
library(tidyr)
colnameshere <- c("lat", "lon", as.character(1:365))
names(justone) <- colnameshere

testing  <- gather(justone, day, value,
           -lat, -lon)

# now I need month
# so I use DOY to get a sequence of dates
nhemi <- c(1:365)
nhemidate <- as.Date(paste(nhemi, 1970, sep="-"), format="%j-%Y") # I checked: non-leap year
doylookup <- data.frame(date=nhemidate, month=format(nhemidate, "%b"), doy=as.character(nhemi))
testmerge <- merge(doylookup, testing, by.x="doy", by.y="day")
meantest <- aggregate(testmerge["value"], testmerge[c("lat", "lon", "month")], FUN=mean)

subset(resultzmin, year=="1970" & month=="4")
subset(meantest, month=="Apr")
## Good news, it works. 
}
