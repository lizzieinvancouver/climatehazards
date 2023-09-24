## Started 14 juillet 2023 ##
## Bastille Day! ##
## By Lizzie ##

## Format and plot the climate projections from Victor ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(data.table)
library(ggplot2)
library(viridis)


## set working directory
setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses")

# source useful f(x)s
source("source/calcclimatefxs.R")


#######################
## Grab future data ##
######################
modelshere <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL") # I don't use this, but I wish that I did

gtmn <- readfilestolist(paste("input/CMIP6_adjust/ssp245/GFDL-ESM4/GFDL-ESM4_tmn_", c(1976:2100), "_dly.fit", sep=""), c(1976:2100))
gtmx <- readfilestolist(paste("input/CMIP6_adjust/ssp245/GFDL-ESM4/GFDL-ESM4_tmx_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))

itmn <- readfilestolist(paste("input/CMIP6_adjust/ssp245/IPSL-CM6A-LR/IPSL-CM6A-LR_tmn_", c(1976:2100), "_dly.fit", sep=""), c(1976:2100))
itmx <- readfilestolist(paste("input/CMIP6_adjust/ssp245/IPSL-CM6A-LR/IPSL-CM6A-LR_tmx_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))

mhtmn <- readfilestolist(paste("input/CMIP6_adjust/ssp245/MPI-ESM1-2-HR/MPI-ESM1-2-HR_tmn_", c(1976:2100), "_dly.fit", sep=""), c(1976:2100))
mhtmx <- readfilestolist(paste("input/CMIP6_adjust/ssp245/MPI-ESM1-2-HR/MPI-ESM1-2-HR_tmx_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))

# This one model not updated yet ... 
mrtmn <- readfilestolist(paste("input/CMIP6_adjust/ssp245/MRI-ESM2-0/MRI-ESM2-0_tmn_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))
mrtmx <- readfilestolist(paste("input/CMIP6_adjust/ssp245/MRI-ESM2-0/MRI-ESM2-0_tmx_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))

utmn <- readfilestolist(paste("input/CMIP6_adjust/ssp245/UKESM1-0-LL/UKESM1-0-LL_tmn_", c(1976:2100), "_dly.fit", sep=""), c(1976:2100))
utmx <- readfilestolist(paste("input/CMIP6_adjust/ssp245/UKESM1-0-LL/UKESM1-0-LL_tmx_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))

###########################
## Plotting future data ##
##########################

# The below code does not run now since you need the same years of min and max data (see above) - Lizzie on 24 Sep 2023

## Look at SD and mean over time ...
yearschange <- data.frame(startyear=seq(from=1976, to=2100, by=10), endyear=seq(from=1976, to=2100, by=10))

makefutureplots <- function(yearschange, minlist, maxlist, modelname){
	changeztmin <- list()
	for(i in c(1:nrow(yearschange))){
	    meanzhere <- getmeansdbysitemonth(minlist, unique(minlist[[1]]["latlon"]), yearschange$startyear[i], yearschange$endyear[i])
	    changeztmin[[i]] <- data.frame(meanzhere, year=yearschange$startyear[i])
	}
	changeztmindf <- do.call("rbind", changeztmin)

	changeztmax <- list()
	for(i in c(1:nrow(yearschange))){
	    meanzhere <- getmeansdbysitemonth(maxlist, unique(maxlist[[1]]["latlon"]), yearschange$startyear[i], yearschange$endyear[i])
	    changeztmax[[i]] <- data.frame(meanzhere, year=yearschange$startyear[i])
	}
	changeztmaxdf <- do.call("rbind", changeztmax)

	meantminplotovetime <- ggplot(changeztmindf, aes(y=mean, x=year, color=latlon)) +
	    geom_line() + 
	    facet_wrap(month~., scales="free") +
	    ggtitle(paste0("Trends in mean of tmin for model ", modelname)) + 
	    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
	                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

	sdtminplotovetime <- ggplot(changeztmindf, aes(y=sd, x=year, color=latlon)) +
	    geom_line() + 
	    facet_wrap(month~., scales="free") +
	    ggtitle(paste0("Trends in SD of tmin for model ", modelname)) + 
	    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
	                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

	meantmaxplotovetime <- ggplot(changeztmaxdf, aes(y=mean, x=year, color=latlon)) +
	    geom_line() + 
	    facet_wrap(month~., scales="free") +
	    ggtitle(paste0("Trends in mean of tmax for model ", modelname)) + 
	    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
	                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

	sdtmaxplotovetime <- ggplot(changeztmaxdf, aes(y=sd, x=year, color=latlon)) +
	    geom_line() + 
	    facet_wrap(month~., scales="free") +
	    ggtitle(paste0("Trends in SD of tmax for model ", modelname)) + 
	    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
	                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
	
	plotshere <- list(meantminplotovetime, sdtminplotovetime, meantmaxplotovetime, sdtmaxplotovetime)

	pdf(paste0("graphs/futuretrends", modelname, ".pdf"), height=8, width=12)
	invisible(lapply(plotshere, print))
	dev.off()
}


makefutureplots(yearschange, gtmn, gtmx, "GFDL-ESM4")
makefutureplots(yearschange, itmn, itmx, "IPSL-CM6A-LR")
makefutureplots(yearschange, mhtmn, mhtmx, "MPI-ESM1-2-HR")
makefutureplots(yearschange, mrtmn, mrtmx, "MRI-ESM2-0")
makefutureplots(yearschange, utmn, utmx, "UKESM1-0-LL")


###########################################
## Plotting future with historical data ##
##########################################

# Below works on ONE model mostly, but could update the 'makehistoricalvsfuturedf' f(x) eventually to do all models and min/max

# Only tmin for now ... 

# First, get the historical data and format to one df
load(file="output/detclimate/tminlist.Rdata")
# Below should be 5-year moving average ... I think/hope
yearschange <- data.frame(startyear=seq(from=1950, to=2016, by=1), endyear=seq(from=1954, to=2020, by=1))
histtmin <- list()
	for(i in c(1:nrow(yearschange))){
	    meanzhere <- getmeansdbysitemonth(tminlist, unique(tminlist[[1]]["latlon"]), yearschange$startyear[i], yearschange$endyear[i])
	    histtmin[[i]] <- data.frame(meanzhere, year=yearschange$startyear[i])
	}
histtmindf <- do.call("rbind", histtmin)

# Merge them and then plot ... 
makehistoricalvsfuturedf <- function(yearschange, historicaldf, futureminlist, modelname){
	futuretmin <- list()
		for(i in c(1:nrow(yearschange))){
		    meanzhere <- getmeansdbysitemonth(futureminlist, unique(futureminlist[[1]]["latlon"]), yearschange$startyear[i], yearschange$endyear[i])
		    futuretmin[[i]] <- data.frame(meanzhere, year=yearschange$startyear[i])
		}
	futuretmindf <- do.call("rbind", futuretmin)
	tryme <- merge(histtmindf, futuretmindf, by=c("latlon", "lat", "lon", "year", "month"), all.x=TRUE, all.y=TRUE, suffixes=c(".hist", ".future"))
	return(tryme)
}

yearschange <- data.frame(startyear=seq(from=1976, to=2096, by=1), endyear=seq(from=1980, to=2100, by=1))
gtmnhistdf <- makehistoricalvsfuturedf(yearschange, histtmindf, gtmn, "gtmn")


# Here's what could be updated to work across models ....

# To make plotting easier, I want long format with a column for 'future' or 'historical'
# Could do the below, but don't get quite what we want so doing it manually ... 
# library(reshape2)
# gtmnhistdflong <- melt(goo, id.var=c("latlon", "lat", "lon", "year", "month"))
gtmnhistdfparthist <- gtmnhistdf[,c(1:7)]
names(gtmnhistdfparthist) <- c("latlon", "lat", "lon", "year", "month", "mean", "sd")
gtmnhistdfparthist$when <- "historical"
gtmnhistdfpartfuture <- gtmnhistdf[,c(1:5, 8:9)]
names(gtmnhistdfpartfuture) <- c("latlon", "lat", "lon", "year", "month", "mean", "sd")
gtmnhistdfpartfuture$when <- "future"

gtmnhistdflong <- rbind(gtmnhistdfparthist, gtmnhistdfpartfuture)

gtmnmean <- ggplot(gtmnhistdflong, aes(y=mean, x=year, color=latlon)) +
    geom_line(aes(linetype=when)) + 
    facet_wrap(month~., scales="free") +
    ggtitle(paste0("Trends in mean of tmin for model ", "GTMN")) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

gtmnsd <- ggplot(gtmnhistdflong, aes(y=sd, x=year, color=latlon)) +
    geom_line(aes(linetype=when)) + 
    facet_wrap(month~., scales="free") +
    ggtitle(paste0("Trends in SD of tmin for model ", "GTMN")) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

gtmnhistdflong1site <- subset(gtmnhistdflong, latlon=="41.9 2.7")
gtmnhistdflong2site <- subset(gtmnhistdflong, latlon=="53.1 10.3")

gtmnmean1site <- ggplot(gtmnhistdflong1site, aes(y=mean, x=year)) +
    geom_line(aes(linetype=when)) + 
    facet_wrap(month~., scales="free") +
    ggtitle(paste0("Trends in mean of tmin for southern-most site for model ", "GTMN")) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

gtmnmean2site <- ggplot(gtmnhistdflong1site, aes(y=mean, x=year)) +
    geom_line(aes(linetype=when)) + 
    facet_wrap(month~., scales="free") +
    ggtitle(paste0("Trends in mean of tmin for northern-most site  for model ", "GTMN")) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plotshere <- list(gtmnmean, gtmnmean1site, gtmnmean2site, gtmnsd)
pdf(paste0("graphs/futurevshistoricalGTMN.pdf"), height=8, width=12)
invisible(lapply(plotshere, print))
dev.off()




