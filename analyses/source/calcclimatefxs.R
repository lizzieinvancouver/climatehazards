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

if(FALSE){ # stuff for testing below functions
i <- 2
sitevector <- unique(tminlist[[1]]["latlon"])
climatelist <- tminlist
startyear <- 1950
endyear <- 2020
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
        # Add long-term monthly mean to the detrended monthly temperatures 
        detrendedtemp <- as.vector(fithere$residuals)+meanheresite$mean
        dftolist <- data.frame(meansiteyearhere, detrendmean=detrendedtemp)
        somebasics <- data.frame(latlon=sitevector[j,], month=i, mean=meanheresite$mean, slope=coef(fithere)[2])
        resultz[[i]] <- rbind(resultz[[i]], dftolist)
        resultz[[13]] <- rbind(resultz[[13]], somebasics)
        if(FALSE){
        par(mfrow=c(3,1))
        plot(meansiteyearhere$mean~meansiteyearhere$year, ylab="Monthly means", xlab="Year")
        abline(fithere, col="red")
        plot(as.vector(fithere$residuals)~meansiteyearhere$year, ylab="Residuals", xlab="Year")
        plot(detrendedtemp~meansiteyearhere$year, ylab="Detrended monthly temperature", xlab="Year")
        }
       }
    }
return(resultz)
}

if(FALSE){ # stuff for testing below function
detrendedmonthly <- tmindetmonths
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
            newdata <- anommonthsiteyr$tempanom + detmonthsiteyr$detrendmean 
            dftolist <- cbind(anommonthsiteyr, newdata)
            names(dftolist)[names(dftolist)=="newdata"] <- "tempC"
            resultz[[i]] <- rbind(resultz[[i]], dftolist)
        }
    }
}    
return(resultz)
}


### Plotting f(x)s ###

if(FALSE){ # testing below f(x)
    climatedata <- tminlist
    filename <- "tmin"
    sitevector <- sitez
    xlabhere <- "min C"
    i <- 1
    j <- 8
    colz <- c("skyblue1", "tan1", "red4")
    lwdhere  <- 2
    }

# This f(x) plots data by month across three 20 year periods (one page per site)
# It needs some work to be use too much further because you have to define latlonhere outside first, which seems bad
# See notes in the main file
plotPDFsbymonthsite <- function(climatedata, filename, sitevector, xlabhere){
colz <- c("skyblue1", "tan1", "red4")
lwdhere  <- 2
pdf(paste("graphs/pdf", filename, ".pdf", sep=""), width=14, height=10)
par(mfrow=c(3, 4))
for (i in c(1:nrow(sitevector))){
    latlonhere <- sitevector[i,]
    # onesite <- lapply(climatedata, subset, latlon==latlonhere, env = parent.frame()) # totally does not work
    onesite <- lapply(climatedata, function(x) subset(x, latlon == latlonhere))
    for (j in 1:12){
        dfhere <- onesite[[j]]
        plot(density(dfhere$tempC), type="n", main=paste("month", j, "- site", latlonhere, sep=" "), 
             xlab=xlabhere)
        legend("topleft", c("1951-1970", "1981-2000", "2001-2020"), lty=rep(1, 3), col=colz, bty="n")
        dfheretime1 <- subset(dfhere, year>=1951 & year <=1970)
        dfheretime2 <- subset(dfhere, year>=1981 & year <=2000)
        dfheretime3 <- subset(dfhere, year>=2001 & year <=2020)
        lines(density(dfheretime1$tempC), col=colz[1], lwd=lwdhere)
        lines(density(dfheretime2$tempC), col=colz[2], lwd=lwdhere)
        lines(density(dfheretime3$tempC), col=colz[3], lwd=lwdhere)
        }
     }
    dev.off()
}




