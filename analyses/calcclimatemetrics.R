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

justone <- fread("input/ERA5LAND/ERA5LAND_tmn_1970_dly.fit")

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
    


##############
## Plotting ##
##############
# BELOW is NOT updated yet ... 

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
