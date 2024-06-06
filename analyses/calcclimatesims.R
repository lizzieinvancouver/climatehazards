## Started 22 June 2023 ##
## By Lizzie ##

## Reads in detrended climate files from calcclimatemetrics.R ##
## It then makes the simiulated climate data ## 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(data.table)
library(ggplot2)
library(viridis)


## set working directory
setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses")


###################################################
## Build simulation data based on detrended data ##
###################################################

# Load the detrended data
load("output/detclimate/tminlist.Rdata")
load("output/detclimate/tmaxlist.Rdata")
load("output/detclimate/tminlistdet.Rdata")
load("output/detclimate/tmaxlistdet.Rdata")

simstorun <- "sims3meansd" # sims2mean sims1sd sims3meansd
sitetouse <- 9 # 1 5 9 
# Picked site at latitude 47.5 (5) based on 20 Apr 2023 with Isabelle
# We now also plan to try the most southern and northern site ... which are 1 and 9

if(simstorun=="sims1sd"){
    varchanges <- c(-0.5, -0.25, 0, 0.25, 0.5) # c(0, -0.1, 0.1, 0.2)
    tempchanges <- c(0) # c(0, 0.5, 1, 2)
    simshere <- c("sd -50%", "sd -25%", "sd 0%", "sd + 25", "sd + 50%")
}

if(simstorun=="sims2mean"){
    varchanges <- c(0) # c(0, -0.1, 0.1, 0.2)
    tempchanges <- c(0, 1, 2, 3, 4, 5) # c(0, 0.5, 1, 2)
    simshere <- c("0C", "+1C", "+2C", "+3C", "+4C", "+5")
}

if(simstorun=="sims3meansd"){
    varchanges <- c(-0.5, 0.5) 
    tempchanges <- c(1, 2, 3, 4, 5) # Deleted 0 degrees since we have that run above (sims1sd)
    simshere <- c("+1C x sd -50%", "+1C x sd +50%", "+2C x sd -50%", "+2C x sd +50%", 
        "+3C x sd -50%", "+3C x sd +50%", "+4C x sd -50%", "+4C x sd +50%", 
        "+5C x sd -50%", "+5C x sd +50%")
}

allchanges <- expand.grid(varchanges, tempchanges)
names(allchanges) <- c("sd", "mean")
allchanges$fakelon <- seq(1:nrow(allchanges))

whichsite <- unique(tmaxlist[[1]]["latlon"])[sitetouse,] 
whichsitewrite <- substr(whichsite, 1, 2)
whichsiteotherfiles <- substr(whichsite, 1, 4)
onesitetmin <- lapply(tminlistdet, function(x) subset(x, latlon == whichsite))
onesitetmax <- lapply(tmaxlistdet, function(x) subset(x, latlon == whichsite))


if(FALSE){
# Does SD vary based on whether you subtract mean first? No.
testingdf <- tmaxlist[[1]]
aggregate(testingdf["tempC"], testingdf["latlon"], FUN=sd)
for(i in c(1:length(unique(testingdf$latlon)))){
    subby <- testingdf[which(testingdf$latlon==unique(testingdf$latlon)[i]),]
    subby$newtemp <- subby$tempC-mean(subby$tempC, na.rm=TRUE)
    print(sd(subby$newtemp))
}
}

source("source/calcclimatesimsfxs.R")

tminsims <- makesimdata(onesitetmin, 1950, 2000, allchanges)
tmaxsims <- makesimdata(onesitetmax, 1950, 2000, allchanges)

plotsimdata(onesitetmin, tminsims, 1950, 2000, paste0(simstorun, whichsitewrite), simshere)
plotsimdataPDF(onesitetmin, tminsims, 1950, 2000, paste0(simstorun, whichsitewrite), simshere)


#################################
## Write out Phenofit sim data ##
#################################

listofyearshere <- unlist(unique(tminsims[[1]]["year"]))
writeoutdata(listofyearshere, tminsims, "tmn_")
writeoutdata(listofyearshere, tmaxsims, "tmx_")

# Also need the mean (tmp), which I will caculate from the sims
# There is likely a smarter way to do this, but I am just doing a loop
tmeansims <- list()
for(i in c(1:length(tminsims))){
    tminhere <- tminsims[[i]]
    tmaxhere <- tmaxsims[[i]]
    # Remarkably the below seems to work! 
    # Added benefit of showing up any impossible error where tmax and tmin formatting is not the same
    tmeanhere <- (tminhere + tmaxhere)/2 
    tmeansims[[i]] <- tmeanhere
}
writeoutdata(listofyearshere, tmeansims, "tmp_")

if(simstorun=="sims1sd"){
source("source/flowerdormplots.R") # for checking what is up with Pinus endodormancy
}

# Also write out the full lists to have for plotting
save(tminsims, file=paste0("output/simsRformat/tmin", simstorun, whichsitewrite, ".Rdata"))
save(tmaxsims, file=paste0("output/simsRformat/tmax", simstorun, whichsitewrite, ".Rdata"))
write.csv(allchanges, file=paste0("output/simsRformat/", simstorun, whichsitewrite, ".csv"), row.names=FALSE)
# write.csv(allchanges, file="output/phenofitsims/sims2run475mean.csv", row.names=FALSE)

# Now write out the other files too... 
source("source/otherfilesformatforsims.R")
# And then update PET ...
source("shared/compute_PET_for_Phenofit.R")
# source("shared/compute_PETnodtm_for_Phenofit.R") # version using tmn (min temp) in place of dtm (dewpoint temp)
compute_PET_for_Phenofit(1950:2000, "output/phenofitsims", "ERA5LAND")