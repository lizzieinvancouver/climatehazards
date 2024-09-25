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

# Make a lookup table for species leafcold and maxcold
lookupcoldspp <- data.frame(species=c("Fagus", "Pinus", "Quercus"), pathname=c("fagsyl", "pinsyl", "querob"), 
	leafcold=c(-5.3, -5, -7), maxcold=c(-25.3, -70.5, -37))

# Took some code from phenofitplotsims.R to use later, perhaps
allsims <- c("sims1sd41", "sims1sd47", "sims1sd53") #     "sims2mean41", "sims2mean47", "sims2mean53"

getcoldtimes <- function(allsims, specieshere, daysbeforehere, daysafterhere){
	# Write out dataframe to fill in ... 
	frosts <- data.frame(sim=character(), sp=character(), lat=numeric(), lon=numeric(), 
		year=numeric(), leafoutdoy=numeric(), dormbreakdoy=numeric(), frostdays=numeric(), 
		maxcolddays=numeric(), meantempinwindow=numeric(), leafindex=numeric())
	lateleafout <- data.frame(sim=character(), sp=character(), lat=numeric(), lon=numeric(), latedays=numeric())

	for(onesim in allsims){ # whichsim  <- "sims1sd53"
		whichsim <- onesim
		whichsp <- specieshere
		pathhere <- lookupcoldspp$pathname[which(lookupcoldspp$species==whichsp)]

		# Get the climate data
		# Reminder that tminsims is 12 dataframes (1 month per df) in a list with...
		# all the different treatments (here, SD) for all the different years...
		load(paste0("output/simsRformat/tmin", whichsim, ".Rdata"))

		# Now try to get the phenofit data
		# Now get the sim data for one species to start
		sitezsimsmessy <- read.delim(paste0("input/phenofit/sims/", whichsim, "/", pathhere, "/Fitness.txt"), nrows=3, header=FALSE)

		row.names(sitezsimsmessy) <- sitezsimsmessy$V1
		sitezsimsmessy$V1 <- NULL
		sitezsims <- as.data.frame(t(sitezsimsmessy))
		names(sitezsims) <- c("loc", "lat", "lon")

		phenofitfiles <- c("LeafUnfoldingDate") # "LeafIndex"
		fsfitsims <- cleanphenofitdata(phenofitfiles, paste0("sims/", whichsim, "/", pathhere, "/"), sitezsims)
		leafindexsims <- cleanphenofitdata(c("LeafIndex") , paste0("sims/", whichsim, "/", pathhere, "/"), sitezsims)
		leafindexdf <- do.call("rbind", leafindexsims)
		dormbreaksims <- cleanphenofitdata(c("LeafDormancyBreakDate") , paste0("sims/", whichsim, "/", pathhere, "/"), sitezsims)
		dormbreakdf <- do.call("rbind", dormbreaksims)
		fsdf <- do.call("rbind", fsfitsims)
		fsdf$sp <- whichsp

		# Now, get the number of days in a period AFTER leaf unfolding 

		FHminfe <- lookupcoldspp$leafcold[which(lookupcoldspp$species==whichsp)] # I made this 3 and got up to 8 frost days, so I think it is working.
		maxcold <- lookupcoldspp$maxcold[which(lookupcoldspp$species==whichsp)] # see calcs in issue #10
		daysbefore <- daysbeforehere
		daysafter <- daysafterhere

		for(i in c(1:nrow(fsdf))){ # i <- 1
			timsimsubbythisyr <- lapply(tminsims, function(x) subset(x, fakelon == fsdf[i,"lon"] & year == fsdf[i,"year"]))
			timsimsubbybeforeyr <- lapply(tminsims, function(x) subset(x, fakelon == fsdf[i,"lon"] & year == (fsdf[i,"year"]-1)))
			dfthisyr <- do.call("rbind", timsimsubbythisyr) 
			dfbeforeyr <- do.call("rbind", timsimsubbybeforeyr) 
			dfbeforeyr <- dfbeforeyr[275:nrow(dfbeforeyr),] # Get Oct 1 onward only
			addthesedays <- nrow(dfbeforeyr) # we need to add these to leafout day below
			df <- rbind(dfbeforeyr, dfthisyr)
			df$counterfromOct1 <- 1:nrow(df)
			df$counter <- df$counterfromOct1-addthesedays
			# df$date <- as.Date(paste(df$counter, df$year, sep="-"), format="%j-%Y")
			tempcinwindow <- df$tempC[which(df$counter>=(round(fsdf[i, "value"])-daysbefore) & df$counter<=(round(fsdf[i, "value"])+daysafter))]
			daysbelowmaxcoldwindow <- df$tempC[1:which(df$counter==(round(fsdf[i, "value"])))]
			leafindexhere <- subset(leafindexdf, lat==fsdf[i,"lat"] & lon==fsdf[i, "lon"] & year==fsdf[i,"year"])
			dormbreakhere <- subset(dormbreakdf, lat==fsdf[i,"lat"] & lon==fsdf[i, "lon"] & year==fsdf[i,"year"])
		    frostsadd <- data.frame(sim=whichsim, sp=fsdf[i,"sp"],
		    	lat=fsdf[i,"lat"], lon=fsdf[i,"lon"], year=fsdf[i,"year"], 
		    	leafoutdoy=round(fsdf[i, "value"]),
		    	dormbreakdoy=dormbreakhere$value,
		    	frostdays=length(tempcinwindow[which(tempcinwindow<FHminfe)]),
		    	maxcolddays=length(daysbelowmaxcoldwindow[daysbelowmaxcoldwindow<maxcold]),
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
	return(frosts)
} # End of getcoldtimes f(x)

# Check that I am getting what we wanted ... 
frostsFagus <- getcoldtimes(allsims, "Fagus", 7, 7)
frostsQuercus <- getcoldtimes(allsims, "Quercus", 7, 7)
frostsPinus <- getcoldtimes(allsims, "Pinus", 7, 7)

plotfrosts <- function(df, specieshere){
	pdf(paste0("graphs/phenofit/sims/extras/leafindex_decompose/leafindex_decompose_", specieshere, ".pdf"), width=8, height=6)
	par(mfrow=c(1,3))
	plot(leafoutdoy~meantempinwindow, data=df, xlab="Mean temperature after leafout", ylab="Leafout day")
	plot(leafindex~leafoutdoy, data=df, xlab="Leafout day", ylab="Leaf Index (frost days in blue and maxcold in red)")
	points(leafindex~leafoutdoy, data=subset(df, frostdays>0), pch=16, col="skyblue")
	points(leafindex~leafoutdoy, data=subset(df, maxcolddays>0), pch=3, col="darkred")
	plot(leafindex~dormbreakdoy, data=df, xlab="LeafDormancyBreakDate day", ylab="Leaf Index (frost days in blue)")
	points(leafindex~dormbreakdoy, data=subset(df, frostdays>0), pch=16, col="skyblue")
	dev.off()
}

plotfrosts(frostsFagus, "Fagus")
plotfrosts(frostsQuercus, "Quercus")
plotfrosts(frostsPinus, "Pinus")