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
# Okay, let's stumble through doing this for ONE sim and go from there ...

# Get one sim
whichsim <-  "sims1sd41" #  sims1sd41 sims1sd47 sims1sd53 sims2mean41 sims2mean47 sims2mean53

# Saving some code from phenofitplotsims.R to use later, perhaps
if(FALSE){
allsims <- c("sims1sd41", "sims1sd47", "sims1sd53", 
    "sims2mean41", "sims2mean47", "sims2mean53")
for(i in allsims){
	whichsim <- i 
	}
}

# Get the climate data
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

fsdf <- do.call("rbind", fsfitsims)
fsdf$sp <- "Fagus"


# Now, get the number of days in a period AFTER leaf unfolding 
FHminfe <- -5.3
daysafter <- 7

# Reminder that tminsims is 12 dataframes (1 month per df) in a list with all the different treatments (here, SD) for all the different years...

# START HERE ... I have extracted the days so far ... 
# for(i in c(1:nrow(fsdf))){ 
i <- 1
	timsimsubby <- lapply(tminsims, function(x) subset(x, fakelon == fsdf[i,"lon"] & year == fsdf[i,"year"]))
	df <- do.call("rbind", timsimsubby) 
	df$counter <- 1:nrow(df)
	# df$date <- as.Date(paste(df$counter, df$year, sep="-"), format="%j-%Y")
	df$tempC[which(df$counter>=round(fsdf[i, "value"]) & df$counter<=(round(fsdf[i, "value"])+daysafter))]
# }



## Probably do not need 
# Get the month and day of leaf unfolding ... back when I thought that I needed this ... 
fsdf$month <- as.numeric(format(fsdf$date , "%m"))
fsdf$day <- as.numeric(format(fsdf$date , "%d"))