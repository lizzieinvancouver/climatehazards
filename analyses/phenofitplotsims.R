## Started 23 June 2023 ##
## By Lizzie ##
## Plotting PHENOFIT4 output from simulated climate data ##

## Currently pretty messy; should clean up how we do this as we move forward... ##
## Would be good to always plot the climate data and some basics of the fitness perhaps? ##

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

whichsim <-  "sims1sd41" #  sims1sd41 sims1sd47 sims1sd53 sims2mean41 sims2mean47 sims2mean53
allsims <- c("sims1sd41", "sims1sd47", "sims1sd53", 
    "sims2mean41", "sims2mean47", "sims2mean53")

sdlabels <- c("-50%", "-25%", "0%", "25", "50%")


for(i in allsims){
whichsim <- i 
# Now get the sim data for one species to start
sitezsimsmessy <- read.delim(paste0("input/phenofit/sims/", whichsim, "/fagsyl/Fitness.txt"), nrows=3, header=FALSE)

row.names(sitezsimsmessy) <- sitezsimsmessy$V1
sitezsimsmessy$V1 <- NULL
sitezsims <- as.data.frame(t(sitezsimsmessy))
names(sitezsims) <- c("loc", "lat", "lon")

phenofitfiles <- c("Fitness", "FruitIndex", "FruitMaturationDate", "LeafIndex", "LeafSenescenceDate",
                   "LeafDormancyBreakDate", "LeafUnfoldingDate", "MaturationIndex", "Survival")
phenofitfilesdull <- c("CarbonSurvival", "DroughtSurvival", "TempSurvival")

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

alldulldf <- rbind(fsdf, qrdf, psdf)


# Get the simulation runs here ...
treatz <- read.csv(paste0("output/simsRformat/", whichsim, ".csv"))
controlrun <- treatz[which(treatz$mean==0 & treatz$sd==0), 3] # identidy the control run's fakelon

# Count years with issues ...
fsbadyrs <- countbadyrs(fsfitsims, treatz)
psbadyrs <- countbadyrs(psfitsims, treatz)
qrbadyrs <- countbadyrs(qrfitsims, treatz)



#####################
## Plots for diffs ##
#####################

# And diff the files from from no change treatment
fsdiff <- getdiffsims(fsfitsims, treatz)
psdiff <- getdiffsims(psfitsims, treatz)
qrdiff <- getdiffsims(qrfitsims, treatz)

if(FALSE){
uniquemeanz <- length(unique(fsbadyrs$mean))
uniquedsz <- length(unique(fsbadyrs$sd))
ncolhere <- if(uniquemeanz>1){
    uniquemeanz} else {
        1
    }
nrowhere <- if(uniquedsz>1){
    uniquedsz} else {
        1
    }
}

fsdiffdf <- do.call("rbind", fsdiff)
fsdiffdf$sp <- "Fagus"
qrdiffdf <- do.call("rbind", qrdiff)
qrdiffdf$sp <- "Quercus"
psdiffdf <- do.call("rbind", psdiff)
psdiffdf$sp <- "Pinus"

alldiff <- rbind(fsdiffdf, qrdiffdf, psdiffdf)

if(grepl("sd", whichsim)){
fsdiffplot <- ggplot(fsdiffdf, aes(y=value, x=as.character(sd))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

qrdiffplot <- ggplot(qrdiffdf, aes(y=value, x=as.character(sd))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

psdiffplot <- ggplot(psdiffdf, aes(y=value, x=as.character(sd))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsFS.pdf"), plot=fsdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsQR.pdf"), plot=qrdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsPS.pdf"), plot=psdiffplot, height=8, width=12)
}

if(grepl("mean", whichsim)){
fsdiffplot <- ggplot(fsdiffdf, aes(y=value, x=as.character(mean))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

qrdiffplot <- ggplot(qrdiffdf, aes(y=value, x=as.character(mean))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

psdiffplot <- ggplot(psdiffdf, aes(y=value, x=as.character(mean))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsFS.pdf"), plot=fsdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsQR.pdf"), plot=qrdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsPS.pdf"), plot=psdiffplot, height=8, width=12)
}
}
## End plots for diffs ##

################################
## Make a file with latitudes ##
################################

whichsim <-  "sims1sd41" #  sims1sd41 sims1sd47 sims1sd53 sims2mean41 sims2mean47 sims2mean53
allsimssd <- c("sims1sd41", "sims1sd47", "sims1sd53")
allsimsmean <- c("sims2mean41", "sims2mean47", "sims2mean53")

# Make a giant file of all SD sims
listsd <- list()
for(i in allsimssd){
whichsim <- i 
# Now get the sim data for one species to start
sitezsimsmessy <- read.delim(paste0("input/phenofit/sims/", whichsim, "/fagsyl/Fitness.txt"), nrows=3, header=FALSE)

row.names(sitezsimsmessy) <- sitezsimsmessy$V1
sitezsimsmessy$V1 <- NULL
sitezsims <- as.data.frame(t(sitezsimsmessy))
names(sitezsims) <- c("loc", "lat", "lon")

phenofitfiles <- c("Fitness", "FruitIndex", "FruitMaturationDate", "LeafIndex", "LeafSenescenceDate",
                   "LeafDormancyBreakDate", "LeafUnfoldingDate", "MaturationIndex", "Survival", 
                   "CarbonSurvival", "DroughtSurvival", "TempSurvival")

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
alldathere$latforsim <- rep(substr(whichsim, 8,10), nrow(alldathere))

listsd[[i]] <- alldathere
onesddf <- do.call("rbind", listsd)
}



# Make a giant file of all mean sims
listmean <- list()
for(i in allsimsmean){
whichsim <- i 
# Now get the sim data for one species to start
sitezsimsmessy <- read.delim(paste0("input/phenofit/sims/", whichsim, "/fagsyl/Fitness.txt"), nrows=3, header=FALSE)

row.names(sitezsimsmessy) <- sitezsimsmessy$V1
sitezsimsmessy$V1 <- NULL
sitezsims <- as.data.frame(t(sitezsimsmessy))
names(sitezsims) <- c("loc", "lat", "lon")

phenofitfiles <- c("Fitness", "FruitIndex", "FruitMaturationDate", "LeafIndex", "LeafSenescenceDate",
                   "LeafDormancyBreakDate", "LeafUnfoldingDate", "MaturationIndex", "Survival", 
                   "CarbonSurvival", "DroughtSurvival", "TempSurvival")

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
alldathere$latforsim <- rep(substr(whichsim, 10, 12), nrow(alldathere))

listmean[[i]] <- alldathere
onemeandf <- do.call("rbind", listmean)
}


##########################################
## Make plots from  file with latitudes ##
##########################################

# Quick and dirty plots of all metrics.... 
# onemeandfnolate <- onemeandf[onemeandf<360,]

for(whichlat in unique(onesddf$latforsim)){
    dfhere <- subset(onesddf, latforsim==whichlat)

fsdiffplot <- ggplot(subset(dfhere, sp=="Fagus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    scale_x_discrete(labels = sdlabels) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

qrdiffplot <- ggplot(subset(dfhere, sp=="Quercus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    scale_x_discrete(labels = sdlabels) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

psdiffplot <- ggplot(subset(dfhere, sp=="Pinus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    scale_x_discrete(labels = sdlabels) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename=paste0("graphs/phenofit/sims/sdsim", whichlat, "_allmetricsFS.pdf"), plot=fsdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/sdsim", whichlat, "_allmetricsQR.pdf"), plot=qrdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/sdsim", whichlat, "_allmetricsPS.pdf"), plot=psdiffplot, height=8, width=12)
}

for(whichlat in unique(onemeandf$latforsim)){
    dfhere <- subset(onemeandf, latforsim==whichlat)

fsdiffplot <- ggplot(subset(dfhere, sp=="Fagus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

qrdiffplot <- ggplot(subset(dfhere, sp=="Quercus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

psdiffplot <- ggplot(subset(dfhere, sp=="Pinus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename=paste0("graphs/phenofit/sims/meansim", whichlat, "_allmetricsFS.pdf"), plot=fsdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/meansim", whichlat, "_allmetricsQR.pdf"), plot=qrdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/meansim", whichlat, "_allmetricsPS.pdf"), plot=psdiffplot, height=8, width=12)
}


# Next, also quick and dirty ... 
# Make up fitness, and its three components where ...
# each component is a row and each column is a latitude
fourmajormetrics <- c("Fitness", "Survival", "FruitIndex", "MaturationIndex")

onesddf4 <- onesddf[which(onesddf$metric %in% fourmajormetrics),]

# https://www.datanovia.com/en/lessons/ggplot-violin-plot/
fs4metricsplot <- ggplot(subset(onesddf4, sp=="Fagus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    scale_x_discrete(labels = sdlabels) + 
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

qr4metricsplot <- ggplot(subset(onesddf4, sp=="Quercus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    scale_x_discrete(labels = sdlabels) + 
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ps4metricsplot <- ggplot(subset(onesddf4, sp=="Pinus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    scale_x_discrete(labels = sdlabels) + 
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename=paste0("graphs/phenofit/sims/metrics4/sdsim_4metricsFS.pdf"), plot=fs4metricsplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/metrics4/sdsim_4metricsQR.pdf"), plot=qr4metricsplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/metrics4/sdsim_4metricsPS.pdf"), plot=ps4metricsplot, height=8, width=12)
# losing rows here due to ylim(-0.25, 1.2) code

onemeandf4 <- onemeandf[which(onemeandf$metric %in% fourmajormetrics),]


fs4metricsplotm <- ggplot(subset(onemeandf4, sp=="Fagus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

qr4metricsplotm <- ggplot(subset(onemeandf4, sp=="Quercus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ps4metricsplotm <- ggplot(subset(onemeandf4, sp=="Pinus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE) + 
    stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "black"
    ) +
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename=paste0("graphs/phenofit/sims/metrics4/meansim_4metricsFS.pdf"), plot=fs4metricsplotm, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/metrics4/meansim_4metricsQR.pdf"), plot=qr4metricsplotm, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/metrics4/meansim_4metricsPS.pdf"), plot=ps4metricsplotm, height=8, width=12)
# losing rows here due to ylim(-0.25, 1.2) code

# Fitness component building plots 
# Finally, also quick and dirty ... 
# Make up fitness, survival, survival x fruitindex file ... 
onemeandf3 <- onemeandf4[which(onemeandf4$metric!="MaturationIndex"),]
onemeandf3wide <- reshape(onemeandf3, idvar=c("lat", "lon", "year", "sp", "latforsim"), timevar="metric", 
    direction = "wide")
onemeandf3wide$SurvFruitIndex <- onemeandf3wide$value.Survival*onemeandf3wide$value.FruitIndex
onemeandf3wide$value.FruitIndex <- NULL

# I need to get this to preserve names: https://stackoverflow.com/questions/47928328/converting-data-from-wide-to-long-using-multiple-columns
# Here's a hack for now ...
# MEAN 
onemeandf3long <- reshape(onemeandf3wide, idvar=c("lat", "lon", "year", "sp", "latforsim"), 
    varying=list(c("value.Survival", "SurvFruitIndex","value.Fitness")), v.names="value",
    direction = "long")
names(onemeandf3long)[which(names(onemeandf3long)=="time")] <- "metric"
onemeandf3long$metric[which(onemeandf3long$metric==2)] <- "2_value.SurvFruitIndex"
onemeandf3long$metric[which(onemeandf3long$metric==1)] <- "1_value.Survival"
onemeandf3long$metric[which(onemeandf3long$metric==3)] <- "3_value.Fitness"


fs3metricsplotm <- ggplot(subset(onemeandf3long, sp=="Fagus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

qr3metricsplotm <- ggplot(subset(onemeandf3long, sp=="Quercus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ps3metricsplotm <- ggplot(subset(onemeandf3long, sp=="Pinus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename=paste0("graphs/phenofit/sims/metrics3/meansim_3metricsFS.pdf"), plot=fs3metricsplotm, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/metrics3/meansim_3metricsQR.pdf"), plot=qr3metricsplotm, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/metrics3/meansim_3metricsPS.pdf"), plot=ps3metricsplotm, height=8, width=12)
# losing rows here due to ylim(-0.25, 1.2) code


# SD 
onesddf3 <- onesddf4[which(onesddf4$metric!="MaturationIndex"),]
onesddf3wide <- reshape(onesddf3, idvar=c("lat", "lon", "year", "sp", "latforsim"), timevar="metric", 
    direction = "wide")
onesddf3wide$SurvFruitIndex <- onesddf3wide$value.Survival*onesddf3wide$value.FruitIndex
onesddf3wide$value.FruitIndex <- NULL

onesddf3long <- reshape(onesddf3wide, idvar=c("lat", "lon", "year", "sp", "latforsim"), 
    varying=list(c("value.Survival", "SurvFruitIndex","value.Fitness")), v.names="value",
    direction = "long")
names(onesddf3long)[which(names(onesddf3long)=="time")] <- "metric"
onesddf3long$metric[which(onesddf3long$metric==2)] <- "2_value.SurvFruitIndex"
onesddf3long$metric[which(onesddf3long$metric==1)] <- "1_value.Survival"
onesddf3long$metric[which(onesddf3long$metric==3)] <- "3_value.Fitness"


fs3metricsplotsd <- ggplot(subset(onesddf3long, sp=="Fagus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    scale_x_discrete(labels = sdlabels) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

qr3metricsplotsd <- ggplot(subset(onesddf3long, sp=="Quercus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    scale_x_discrete(labels = sdlabels) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ps3metricsplotsd <- ggplot(subset(onesddf3long, sp=="Pinus"), aes(y=value, x=as.character(lon))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    ylim(-0.25, 1.2) + 
    facet_wrap(as.character(latforsim)~metric, scales="free") +
    scale_x_discrete(labels = sdlabels) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename=paste0("graphs/phenofit/sims/metrics3/sdsim_3metricsFS.pdf"), plot=fs3metricsplotsd, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/metrics3/sdsim_3metricsQR.pdf"), plot=qr3metricsplotsd, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/metrics3/sdsim_3metricsPS.pdf"), plot=ps3metricsplotsd, height=8, width=12)
# losing rows here due to ylim(-0.25, 1.2) code



if(FALSE){
# Started 22 June 2023 
# Need to break down and write in base R, otherwise the code is too ugly. 
# However, this is super slow going so maybe best to do later ... 
phenofitfilesdate <- c("FruitMaturationDate", "LeafSenescenceDate", "LeafDormancyBreakDate", "LeafUnfoldingDate")
phenofitfiles0to1 <- c("Fitness", "FruitIndex", "LeafIndex", "MaturationIndex", "Survival")
phenofilesnames  <-  phenofitfilesdate
df <- alldf

for(uniquesp in c(unique(df[["sp"]]))){
    par(mfrow=c(3, 4))
    dfsp <- df[which(df[["sp"]]==uniquesp),]
    print(uniquesp)
    for(i in c(1:length(phenofilesnames))){
        dfhere <- dfsp[which(dfsp[["metric"]]==phenofilesnames[i]),]
        dfherelate <- dfhere[which(dfhere$metric>359),]
        dfherenotlate <- dfhere[which(dfhere$metric<360),]
        summlate <-
            ddply(dfherelate, c("lon"), summarise,
            mean = mean(value),
            sd = sd(value),
            sem = sd(value)/sqrt(length(value)))
        summnotlate <-
            ddply(dfherenotlate, c("lon"), summarise,
            mean = mean(value),
            sd = sd(value),
            sem = sd(value)/sqrt(length(value)))    
        plot(x=summlate$lon, y=summlate$mean, xlab="Sim", ylab="Value", pch=16)
        arrows(x0=(summlate$lon), 
            y0=(summlate$mean-summlate$sd), 
            x1=(summlate$lon),
            y1=(summlate$mean+summlate$sd), code=3,angle=180,length=0.05)
        plot(x=summnotlate$lon, y=summnotlate$mean, xlab="Sim", ylab="Value", pch=16)
        arrows(x0=(summnotlate$lon), 
            y0=(summnotlate$mean-summnotlate$sd), 
            x1=(summnotlate$lon),
            y1=(summlate$mean+summnotlate$sd), code=3,angle=180,length=0.05)
    }
}
}







# Need to review below ...
if(FALSE){ 
##
## SIDE BAR: Historical versus 0% change output
## get the data for one species from historical data to compare
sitezmessy <- read.delim("input/phenofit/fagsyl_19512020/Fitness.txt", nrows=3, header=FALSE)
row.names(sitezmessy) <- sitezmessy$V1
sitezmessy$V1 <- NULL
sitez <- as.data.frame(t(sitezmessy))
names(sitez) <- c("loc", "lat", "lon")

fsfit <- cleanphenofitdata(phenofitfiles, "fagsyl_19512020/", sitez)

fsfitsims <- cleanphenofitdata(phenofitfiles, "sims/sims2/fagsyl/", sitezsims)

# Now compare what should be the same ...
fssimscompare <- fsfitsims[[1]][which(fsfitsims[[1]]["lon"]==2),]
fshistcompare <- fsfit[[1]][which(fsfit[[1]]["lat"]=="47.5"),]

fssimscompareyrs <- subset(fssimscompare, year>1951)
fshistcompareyrs <- subset(fshistcompare, year<2001)
plot(fssimscompareyrs$value ~ fshistcompareyrs$value)

# Hmm, check the temperature data ... 
# But the simulated data is from detrended data so perhaps I need to think more on this comparison ...
dhist <- fread("input/ERA5LAND/ERA5LAND_tmn_1952_dly.fit")
dhist475 <- dhist[5,]
dsims <- fread("output/phenofitsims/ERA5LAND_tmn_1952_dly.fit")
dsims2 <- dsims[2,]
## END SIDE BAR: Historical versus 0% change output
## 


# plot the data 



makequickplotsyrs <- function(fitdf, leafdf, matdf, filename, ylimhere, xlimhere, sitezdf){
    colz <- viridis(nrow(sitezdf))
    pdf(paste("graphs/phenofit/", filename, ".pdf", sep=""), width=12, height=4)
    par(mfrow=c(1, 3))
    plot(fitdf[["value"]] ~ fitdf[["year"]], type="n",
     xlab="year", ylab="fitness", ylim=ylimhere, xlim=xlimhere)
    legend("topleft", legend=sitezdf[["treat"]], lty=rep(1, nrow(sitezdf)), col=colz, bty="n")
    for(i in c(1:nrow(sitezdf))){
        dfhere <- subset(fitdf, lat==sitezdf[["lat"]][i] & lon==sitezdf[["lon"]][i])
        lines(dfhere[["value"]] ~ dfhere[["year"]], col=colz[i])
}
    plot(leafdf[["value"]] ~ leafdf[["year"]], type="n",
     xlab="year", ylab="leaf infex", ylim=ylimhere, xlim=xlimhere)
    legend("topleft", legend=sitezdf[["treat"]], lty=rep(1, nrow(sitezdf)), col=colz, bty="n")

    for(i in c(1:nrow(sitezdf))){
        dfhere <- subset(leafdf, lat==sitezdf[["lat"]][i] & lon==sitezdf[["lon"]][i])
        lines(dfhere[["value"]] ~ dfhere[["year"]], col=colz[i])
}
    plot(matdf[["value"]] ~ matdf[["year"]], type="n",
         xlab="year", ylab="maturation index", ylim=ylimhere, xlim=xlimhere)
    legend("topleft", legend=sitezdf[["treat"]], lty=rep(1, nrow(sitezdf)), col=colz, bty="n")

    for(i in c(1:nrow(sitezdf))){
        dfhere <- subset(matdf, lat==sitezdf[["lat"]][i] & lon==sitezdf[["lon"]][i])
        lines(dfhere[["value"]] ~ dfhere[["year"]], col=colz[i])
}
dev.off()
}

makequickplotsyrs(fsfitsims[["Fitness"]], fsfitsims[["LeafIndex"]], fsfitsims[["MaturationIndex"]],
               "sims2_3metricsSitesOverlayFagus", c(0,1.1), c(1951, 2000), sitezsims)

fsfitdf <- do.call("rbind", fsfitsims)
fsfitdf$sp <- "Fagus"
qrfitdf <- do.call("rbind", qrfitsims)
qrfitdf$sp <- "Quercus"
psfitdf <- do.call("rbind", psfitsims)
psfitdf$sp <- "Pinus"

alldat <- rbind(fsfitdf, qrfitdf, psfitdf)
alldatwide <- reshape(alldat, idvar=c("lat", "lon", "year", "sp"), timevar="metric", direction = "wide")

surv <- ggplot(alldatwide, aes(x=value.Survival, y=value.Fitness, col=as.character(lon))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Survival") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

leafdate <- ggplot(alldatwide, aes(x=value.LeafUnfoldingDate, y=value.Fitness, col=as.character(lon))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Leaf Unfolding Date") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

frmatdate <- ggplot(alldatwide, aes(x=value.FruitMaturationDate, y=value.Fitness, col=as.character(lon))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Fruit Maturation Date") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

frleafdate <- ggplot(alldatwide, aes(x=value.LeafUnfoldingDate, y=value.FruitMaturationDate, col=as.character(lon))) +
    geom_line() +
    xlab("Leaf Unfolding Date") +
    ylab("Fruit Maturation Date") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

leafind <- ggplot(alldatwide, aes(x=value.LeafIndex, y=value.Fitness, col=as.character(lon))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Leaf Index") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


frind <- ggplot(alldatwide, aes(x=value.FruitIndex, y=value.Fitness, col=as.character(lon))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Fruit Index") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


matind <- ggplot(alldatwide, aes(x=value.MaturationIndex, y=value.Fitness, col=as.character(lon))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Maturation Index") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p <- list(leafdate, frmatdate, frleafdate, leafind, frind, matind)

pdf("graphs/phenofit/sims2_fitness1951.pdf", height=4, width=10)
for (i in seq(length(p))) {
  print(p[[i]])
}
dev.off()
}
