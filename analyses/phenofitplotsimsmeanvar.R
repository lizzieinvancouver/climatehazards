## Started 6 June 2024 ##
## By Lizzie ##
## Plot the sims for mean x variance changes ##

## You know what would classy and useful? Tidy code that is adapted to handle all the scenarios we tried:
# Mean changes only
# Variance changes only
# Mean x variance changes
## But I did not do that! I just made this new file to handle the mean x variance sims. 


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


####
####
whichsim <-  "sims1sd41" # sims3meansd41 sims3meansd47 sims3meansd53
allsims <- c("sims3meansd41", "sims3meansd47", "sims3meansd53")


for(i in allsims){ # i  <- "sims3meansd41"
whichsim <- i 
# Now get the sim data for one species to start
sitezsimsmessy <- read.delim(paste0("input/phenofit/sims/", whichsim, "/fagsyl/Fitness.txt"), nrows=3, header=FALSE)

row.names(sitezsimsmessy) <- sitezsimsmessy$V1
sitezsimsmessy$V1 <- NULL
sitezsims <- as.data.frame(t(sitezsimsmessy))
names(sitezsims) <- c("loc", "lat", "lon")

phenofitfiles <- c("Fitness", "Survival", "FruitIndex", "MaturationIndex",
    "CarbonSurvival", "LeafIndex", "LeafDormancyBreakDate", "LeafUnfoldingDate",
    "FlowerDormancyBreakDate", "FloweringDate", "FruitMaturationDate", "LeafSenescenceDate")
phenofitfilesdull <- c("DroughtSurvival", "TempSurvival")

fsfitsims <- cleanphenofitdata(phenofitfiles, paste0("sims/", whichsim, "/fagsyl/"), sitezsims)
qrfitsims <- cleanphenofitdata(phenofitfiles, paste0("sims/", whichsim, "/querob/"), sitezsims)
psfitsims <- cleanphenofitdata(phenofitfiles, paste0("sims/", whichsim, "/pinsyl/"), sitezsims)

fsdf <- do.call("rbind", fsfitsims)
fsdf$sp <- "Fagus"
qrdf <- do.call("rbind", qrfitsims)
qrdf$sp <- "Quercus"
psdf <- do.call("rbind", psfitsims)
psdf$sp <- "Pinus"

alldf <- rbind(fsdf, qrdf, psdf)

fsfitsimsdull <- cleanphenofitdata(phenofitfilesdull, paste0("sims/", whichsim, "/fagsyl/"), sitezsims)
qrfitsimsdull <- cleanphenofitdata(phenofitfilesdull, paste0("sims/", whichsim, "/querob/"), sitezsims)
psfitsimsdull <- cleanphenofitdata(phenofitfilesdull, paste0("sims/", whichsim, "/pinsyl/"), sitezsims)

# Alert! Reusing names above
fsdf <- do.call("rbind", fsfitsimsdull)
fsdf$sp <- "Fagus"
qrdf <- do.call("rbind", qrfitsimsdull)
qrdf$sp <- "Quercus"
psdf <- do.call("rbind", psfitsimsdull)
psdf$sp <- "Pinus"

# Temp and drought survival are dull, so checking them and moving on
# See quick notes in daily log on 3 June 2024. 
alldulldf <- rbind(fsdf, qrdf, psdf)
lessthanoneds <- subset(alldulldf, value<1 & metric=="DroughtSurvival")
lessthanonets <- subset(alldulldf, value<1 & metric=="TempSurvival")
print(paste0(nrow(lessthanoneds), " of ", nrow(subset(alldulldf, metric=="DroughtSurvival")), 
    " rows of drought survival are less than one with a minimum of ",
    min(lessthanoneds$value)))
print(paste0(nrow(lessthanonets), " of ", nrow(subset(alldulldf, metric=="TempSurvival")), 
    " rows of temp survival are less than one with a minimum of ",
    min(lessthanonets$value)))


# Get the simulation runs here ...
treatz <- read.csv(paste0("output/simsRformat/", whichsim, ".csv"))

# Count years with issues (dates greater than 365 or 0 values)
fsbadyrs <- countbadyrs(fsfitsims, treatz)
psbadyrs <- countbadyrs(psfitsims, treatz)
qrbadyrs <- countbadyrs(qrfitsims, treatz)
}

################################
## Make a file with latitudes ##
################################


# Make a giant file of all mean x SD sims
listmeansd <- list()
for(i in allsims){
whichsim <- i 
# Now get the sim data for one species to start
sitezsimsmessy <- read.delim(paste0("input/phenofit/sims/", whichsim, "/fagsyl/Fitness.txt"), nrows=3, header=FALSE)

row.names(sitezsimsmessy) <- sitezsimsmessy$V1
sitezsimsmessy$V1 <- NULL
sitezsims <- as.data.frame(t(sitezsimsmessy))
names(sitezsims) <- c("loc", "lat", "lon")

phenofitfiles <- c("Fitness", "Survival", "FruitIndex", "MaturationIndex",
    "CarbonSurvival", "LeafIndex", "LeafDormancyBreakDate", "LeafUnfoldingDate",
    "FlowerDormancyBreakDate", "FloweringDate", "FruitMaturationDate", "LeafSenescenceDate")

fsfitsims <- cleanphenofitdata(phenofitfiles, paste0("sims/", whichsim, "/fagsyl/"), sitezsims)
qrfitsims <- cleanphenofitdata(phenofitfiles, paste0("sims/", whichsim, "/querob/"), sitezsims)
psfitsims <- cleanphenofitdata(phenofitfiles, paste0("sims/", whichsim, "/pinsyl/"), sitezsims)


fsdf <- do.call("rbind", fsfitsims)
fsdf$sp <- "Fagus"
qrdf <- do.call("rbind", qrfitsims)
qrdf$sp <- "Quercus"
psdf <- do.call("rbind", psfitsims)
psdf$sp <- "Pinus"

alldathere <- rbind(fsdf, qrdf, psdf)
alldathere$latforsim <- rep(substr(whichsim, 12,14), nrow(alldathere))

listmeansd[[i]] <- alldathere
onemeansddf <- do.call("rbind", listmeansd)
}

# And get columns fro mean warming versus SD
simsiran<- c("+1C x sd -50%", "+1C x sd +50%", "+2C x sd -50%", "+2C x sd +50%", 
    "+3C x sd -50%", "+3C x sd +50%", "+4C x sd -50%", "+4C x sd +50%", 
    "+5C x sd -50%", "+5C x sd +50%")
onemeansddf$var <- NA
onemeansddf$var[which(onemeansddf$lon %in% c(1, 3, 5, 7, 9))] <- "50perlower"
onemeansddf$var[which(onemeansddf$lon %in% c(2, 4, 6, 8, 10))] <- "50perhigher"
onemeansddf$warming <- onemeansddf$lon
onemeansddf$warming[which(onemeansddf$lon==2)] <- 1
onemeansddf$warming[which(onemeansddf$lon==3)] <- 2
onemeansddf$warming[which(onemeansddf$lon==4)] <- 2
onemeansddf$warming[which(onemeansddf$lon==5)] <- 3
onemeansddf$warming[which(onemeansddf$lon==6)] <- 3
onemeansddf$warming[which(onemeansddf$lon==7)] <- 4
onemeansddf$warming[which(onemeansddf$lon==8)] <- 4
onemeansddf$warming[which(onemeansddf$lon==9)] <- 5
onemeansddf$warming[which(onemeansddf$lon==10)] <- 5
unique(paste(onemeansddf$var, onemeansddf$warming)) # just checking!

##########################################
## Make plots from file with latitudes ##
##########################################

# Quick and dirty plots of all metrics.... 
# Violin plots showing showing the variance on top of the warming

for(whichlat in unique(onemeansddf$latforsim)){
dfhere <- subset(onemeansddf, latforsim==whichlat)

fsdiffplot <- ggplot(subset(dfhere, sp=="Fagus"), aes(y=value, x=as.character(warming), col=var)) +
    geom_violin(trim=FALSE, aes(fill=var), alpha = 0.3, col = NA) + 
  	stat_summary(
    position = position_dodge(width=0.9),
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", lwd=1
 	 ) +
    facet_wrap(factor(metric, levels=c("Fitness", "Survival", "FruitIndex", "MaturationIndex",
    "CarbonSurvival", "LeafIndex", "LeafDormancyBreakDate", "LeafUnfoldingDate",
    "FlowerDormancyBreakDate", "FloweringDate", "FruitMaturationDate", "LeafSenescenceDate"))~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

qrdiffplot <- ggplot(subset(dfhere, sp=="Quercus"), aes(y=value, x=as.character(warming), col=var)) +
	geom_violin(trim=FALSE, aes(fill=var), alpha = 0.3, col = NA) + 
  	stat_summary(
    position = position_dodge(width=0.9),
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", lwd=1
 	 ) +    facet_wrap(factor(metric, levels=c("Fitness", "Survival", "FruitIndex", "MaturationIndex",
    "CarbonSurvival", "LeafIndex", "LeafDormancyBreakDate", "LeafUnfoldingDate",
    "FlowerDormancyBreakDate", "FloweringDate", "FruitMaturationDate", "LeafSenescenceDate"))~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

psdiffplot <- ggplot(subset(dfhere, sp=="Pinus"), aes(y=value, x=as.character(warming), col=var)) +
	geom_violin(trim=FALSE, aes(fill=var), alpha = 0.3, col = NA) + 
  	stat_summary(
    position = position_dodge(width=0.9),
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", lwd=1
 	 ) +
    facet_wrap(factor(metric, levels=c("Fitness", "Survival", "FruitIndex", "MaturationIndex",
    "CarbonSurvival", "LeafIndex", "LeafDormancyBreakDate", "LeafUnfoldingDate",
    "FlowerDormancyBreakDate", "FloweringDate", "FruitMaturationDate", "LeafSenescenceDate"))~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename=paste0("graphs/phenofit/sims/meansdsimallmetricsFSlat", whichlat, ".pdf"), plot=fsdiffplot, height=10, width=14)
ggsave(filename=paste0("graphs/phenofit/sims/meansdsimallmetricsQRlat", whichlat, ".pdf"), plot=qrdiffplot, height=10, width=14)
ggsave(filename=paste0("graphs/phenofit/sims/meansdsimallmetricsPSlat", whichlat, ".pdf"), plot=psdiffplot, height=10, width=14)
}

