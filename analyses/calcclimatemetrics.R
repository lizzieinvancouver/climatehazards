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
library(viridis)


## set working directory
setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses")

## To do!
## 

source("source/calcclimatefxs.R")


# each list item should be 71 years * days in month * 9 sites
# So, for January: 19809 rows
tminlist <- readfilestolist(paste("input/ERA5LAND/ERA5LAND_tmn_", c(1950:2020), "_dly.fit", sep=""), c(1950:2020))
tminlistanom <- readfilestolistanomalies(paste("input/ERA5LAND/ERA5LAND_tmn_", c(1950:2020), "_dly.fit", sep=""), c(1950:2020))

# Should be 12 months x 9 sites 
tminsumm <- getmeansdbysitemonth(tminlist, unique(tminlist[[1]]["latlon"]), 1950, 2020)
# Should be 12 months x 9 sites x 71 years
tminsummyr <- getmeansdbysiteyrmonth(tminlist, unique(tminlist[[1]]["latlon"]), 1950, 2020)

tmindetmonths <- detrendmonthlyclimatelist(tminsummyr, tminsumm, unique(tminlist[[1]]["latlon"]))
tminlistdet <- getdailydetrendedlist(tmindetmonths, tminlistanom, unique(tminlist[[1]]["latlon"]), c(1950:2020))

# Tmax!
tmaxlist <- readfilestolist(paste("input/ERA5LAND/ERA5LAND_tmx_", c(1950:2020), "_dly.fit", sep=""), c(1950:2020))
tmaxlistanom <- readfilestolistanomalies(paste("input/ERA5LAND/ERA5LAND_tmx_", c(1950:2020), "_dly.fit", sep=""), c(1950:2020))
tmaxsumm <- getmeansdbysitemonth(tmaxlist, unique(tmaxlist[[1]]["latlon"]), 1950, 2020)
tmaxsummyr <- getmeansdbysiteyrmonth(tmaxlist, unique(tmaxlist[[1]]["latlon"]), 1950, 2020)
tmaxdetmonths <- detrendmonthlyclimatelist(tmaxsummyr, tmaxsumm, unique(tmaxlist[[1]]["latlon"]))
tmaxlistdet <- getdailydetrendedlist(tmaxdetmonths, tmaxlistanom, unique(tmaxlist[[1]]["latlon"]), c(1950:2020))


##############
## Plotting ##
##############

if(FALSE){ # Sort of slow and was just a safety check
# Compare the original data to detrended data
colz <- viridis(9)
par(mfrow=c(1,4))
for(i in c(1,4,7,11)){ # just doing a few months as this is slow
    dfplain <- tminlist[[i]]
    dfdet <- tminlistdet[[i]]
    plot(x=dfplain$tempC, y=dfdet$tempC, type="n", xlab="Min C", ylab="detrended Min C")
    for(j in c(1,4,9)){ # 1:nrow(unique(tminlist[[1]]["latlon"]))
        dfplainsite <- subset(dfplain, latlon==unique(tminlist[[1]]["latlon"])[j,])
        dfdetsite <- subset(dfdet, latlon==unique(tminlist[[1]]["latlon"])[j,])
        points(x=dfplainsite$tempC, y=dfdetsite$tempC, col=colz[j])
        abline(lm(dfdetsite$tempC~dfplainsite$tempC), col=colz[j])
     }
}
} 


# PDFs by month and site
sitez <- unique(tminlist[[1]]["latlon"])

# NEED to fix ... not sure why the function cannot find the latlonhere starting at i=2
# ChatGPT tells me: This error is likely occurring because the variable "latlonhere" is defined inside a loop in the function plotPDFsbymonthsite. The variable is initialized as latlonhere <- sitedf$latlon[i] inside the loop, but it may not exist outside the loop when the lapply function is called.
# Tried add , envir=parent.frame() to apply command, but did not fix it.
# Without the f(x) -- see below -- it is okay; within the function it is the same data across sites
colz <- c("skyblue1", "tan1", "red4")
lwdhere  <- 2
sitevector <- sitez
climatedata <- tminlistdet


pdf(paste("graphs/pdfTEST.pdf", sep=""), width=14, height=10)
par(mfrow=c(3, 4))
for (i in c(1:nrow(sitevector))){
    latlonhere <- sitevector[i,]
    onesite <- lapply(climatedata, subset, latlon==latlonhere, env = parent.frame())
    for (j in 1:12){
        dfhere <- onesite[[j]]
        plot(density(dfhere$tempC), type="n", main=paste("month", j, "- site", latlonhere, sep=" "), 
             xlab="detrended temperature")
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

plotPDFsbymonthsite(tminlistdet, "tmindet", sitez, "min C detrended")
plotPDFsbymonthsite(tminlist, "tmin", sitez, "min C")

plotPDFsbymonthsite(tmaxlistdet, "tmaxdet", sitez, "max C detrended")
plotPDFsbymonthsite(tmaxlist, "tmax", sitez, "max C")





# save the results to a CSV file
# write.csv(results, "results.csv", row.names=FALSE)


if(FALSE){ ## Test some code at some early point ...
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
