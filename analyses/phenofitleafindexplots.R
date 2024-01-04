## Started 2 January 2024 ##
## By Lizzie ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(data.table)
library(plyr)
library(ggplot2)
library(gridExtra)
library(viridis)

## set working directory
setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses")
source("source/plotsimsfxs.R")

###
###
# The below works for all sims for Fagus ... 
# I suggest that I functionalize this for the other species
# Be sure to include in the f(x) FHminfe and daysafter
### 

# Took some code from phenofitplotsims.R to use later, perhaps
allsims <- c("sims1sd41", "sims1sd47", "sims1sd53") #     "sims2mean41", "sims2mean47", "sims2mean53"

# Write out dataframe to fill in ... 
frosts <- data.frame(sim=character(), sp=character(), lat=numeric(), lon=numeric(), 
	year=numeric(), leafoutdoy=numeric(), frostdays=numeric(), meantempinwindow=numeric(), leafindex=c())
lateleafout <- data.frame(sim=character(), sp=character(), lat=numeric(), lon=numeric(), latedays=numeric())

for(i in allsims){
	whichsim <- i 

	# Get the climate data
	# Reminder that tminsims is 12 dataframes (1 month per df) in a list with...
	# all the different treatments (here, SD) for all the different years...
	load(paste0("output/simsRformat/tmin", whichsim, ".Rdata"))

	# Now try to get the phenofit data
	# Now get the sim data for one species to start
	sitezsimsmessy <- read.delim(paste0("input/phenofit/sims/", whichsim, "/fagsyl/Fitness.txt"), nrows=3, header=FALSE)

	row.names(sitezsimsmessy) <- sitezsimsmessy$V1
	sitezsimsmessy$V1 <- NULL
	sitezsims <- as.data.frame(t(sitezsimsmessy))
	names(sitezsims) <- c("loc", "lat", "lon")

	phenofitfiles <- c("LeafUnfoldingDate") # "LeafIndex"
	fsfitsims <- cleanphenofitdata(phenofitfiles, paste0("sims/", whichsim, "/fagsyl/"), sitezsims)
	leafindexsims <- cleanphenofitdata(c("LeafIndex") , paste0("sims/", whichsim, "/fagsyl/"), sitezsims)
	leafindexdf <- do.call("rbind", leafindexsims)
	fsdf <- do.call("rbind", fsfitsims)
	fsdf$sp <- "Fagus"

	# Now, get the number of days in a period AFTER leaf unfolding 
	FHminfe <- -5.3 # I made this 3 and got up to 8 frost days, so I think it is working.
	daysafter <- 7

	for(i in c(1:nrow(fsdf))){ # i <- 1
		timsimsubby <- lapply(tminsims, function(x) subset(x, fakelon == fsdf[i,"lon"] & year == fsdf[i,"year"]))
		df <- do.call("rbind", timsimsubby) 
		df$counter <- 1:nrow(df)
		# df$date <- as.Date(paste(df$counter, df$year, sep="-"), format="%j-%Y")
		tempcinwindow <- df$tempC[which(df$counter>=round(fsdf[i, "value"]) & df$counter<=(round(fsdf[i, "value"])+daysafter))]
		leafindexhere <- subset(leafindexdf, lat==fsdf[i,"lat"] & lon==fsdf[i, "lon"] & year==fsdf[i,"year"])
	    frostsadd <- data.frame(sim=whichsim, sp=fsdf[i,"sp"],
	    	lat=fsdf[i,"lat"], lon=fsdf[i,"lon"], year=fsdf[i,"year"], 
	    	leafoutdoy=round(fsdf[i, "value"]),
	    	frostdays=length(tempcinwindow[which(tempcinwindow<FHminfe)]),
	    	meantempinwindow=mean(tempcinwindow),
	    	leafindex=leafindexhere$value)
	    frosts <- rbind(frosts, frostsadd)
	}

	for(lonhere in unique(fsdf$lon)){ # lonhere <- 1
		subby <- subset(fsdf, lon==lonhere)
		lateleafoutadd <- data.frame(sim=whichsim, sp=subby[1,"sp"],
	    	lat=subby[1,"lat"], lon=subby[1,"lon"], latedays=nrow(subset(subby, value>364)))
		lateleafout <- rbind(lateleafout, lateleafoutadd)
	}
} 

# Check that I am getting what we wanted ... 
table(frosts$lon, frosts$sim)

# plot results
par(mfrow=c(1,2))
plot(leafoutdoy~meantempinwindow, data=frosts, xlab="Mean temperature after leafout", ylab="Leafout day")
plot(leafindex~leafoutdoy, data=frosts, xlab="Leafout day", ylab="Leaf Index (frost days in blue)")
points(frostdays~leafoutdoy, data=frosts, pch=16, col="skyblue")
subset(frosts, frostdays>0)
