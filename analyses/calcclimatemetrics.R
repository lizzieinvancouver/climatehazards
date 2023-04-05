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


######################################
## Grab historical data and detrend ##
######################################

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

sitez <- unique(tminlist[[1]]["latlon"])

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

## PDFs by month and site

if(FALSE){ # checking my code with some quick comparisons 
testpdffx <- subset(tminlist[[8]], latlon==sitez[2,])
testpdffxtime1 <- subset(testpdffx, year>=1951 & year <=1970)
testpdffxtime2 <- subset(testpdffx, year>=1981 & year <=2000)
testpdffxtime3 <- subset(testpdffx, year>=2001 & year <=2020)
plot(density(testpdffxtime1$tempC), type="n")
lines(density(testpdffxtime1$tempC))
lines(density(testpdffxtime2$tempC), col="orange")
lines(density(testpdffxtime3$tempC), col="red")
}

plotPDFsbymonthsite(tminlistdet, "tmindet", sitez, "min C detrended")
plotPDFsbymonthsite(tminlist, "tmin", sitez, "min C")

plotPDFsbymonthsite(tmaxlistdet, "tmaxdet", sitez, "max C detrended")
plotPDFsbymonthsite(tmaxlist, "tmax", sitez, "max C")

## Look at SD and mean over time ...
yearschange <- data.frame(startyear=seq(from=1950, to=2000, by=10), endyear=seq(from=1970, to=2020, by=10))

changez <- list()
for(i in c(1:nrow(yearschange))){
    meanzhere <- getmeansdbysitemonth(tminlist, unique(tminlist[[1]]["latlon"]), yearschange$startyear[i], yearschange$endyear[i])
    changez[[i]] <- data.frame(meanzhere, year=yearschange$startyear[i])
}
changezdf <- do.call("rbind", changez)

meanplotovetime <- ggplot(changezdf, aes(y=mean, x=year, color=latlon)) +
    geom_line() + 
    facet_wrap(month~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

sdplotovetime <- ggplot(changezdf, aes(y=sd, x=year, color=latlon)) +
    geom_line() + 
    facet_wrap(month~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename="graphs/historicaltrendsmean.pdf", plot=meanplotovetime)
ggsave(filename="graphs/historicaltrendssd.pdf", plot=sdplotovetime)


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
