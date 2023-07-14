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

gtmn <- readfilestolist(paste("input/CMIP6_adjust/ssp245/GFDL-ESM4/GFDL-ESM4_tmn_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))
gtmx <- readfilestolist(paste("input/CMIP6_adjust/ssp245/GFDL-ESM4/GFDL-ESM4_tmx_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))

itmn <- readfilestolist(paste("input/CMIP6_adjust/ssp245/IPSL-CM6A-LR/IPSL-CM6A-LR_tmn_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))
itmx <- readfilestolist(paste("input/CMIP6_adjust/ssp245/IPSL-CM6A-LR/IPSL-CM6A-LR_tmx_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))

mhtmn <- readfilestolist(paste("input/CMIP6_adjust/ssp245/MPI-ESM1-2-HR/MPI-ESM1-2-HR_tmn_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))
mhtmx <- readfilestolist(paste("input/CMIP6_adjust/ssp245/MPI-ESM1-2-HR/MPI-ESM1-2-HR_tmx_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))

mrtmn <- readfilestolist(paste("input/CMIP6_adjust/ssp245/MRI-ESM2-0/MRI-ESM2-0_tmn_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))
mrtmx <- readfilestolist(paste("input/CMIP6_adjust/ssp245/MRI-ESM2-0/MRI-ESM2-0_tmx_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))

utmn <- readfilestolist(paste("input/CMIP6_adjust/ssp245/UKESM1-0-LL/UKESM1-0-LL_tmn_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))
utmx <- readfilestolist(paste("input/CMIP6_adjust/ssp245/UKESM1-0-LL/UKESM1-0-LL_tmx_", c(2020:2100), "_dly.fit", sep=""), c(2020:2100))

###########################
## Plotting future data ##
##########################

## Look at SD and mean over time ...
yearschange <- data.frame(startyear=seq(from=2020, to=2100, by=10), endyear=seq(from=2020, to=2100, by=10))

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

