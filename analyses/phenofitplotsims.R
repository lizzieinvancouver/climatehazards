## Started 23 June 2023 ##
## By Lizzie ##
## Plotting PHENOFIT4 output from simulated climate data ##

## Currently pretty messy; should clean up how we do this as we move forward... ##
## Would be good to always plot the climate data and some basics of the fitness perhaps? ##

## CURRENTLY set to sims2 ## 

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

whichsim <-  "sims1sd41" #  sims1sd41 sims1sd47 sims1sd53 sims2mean41 sims2mean47 sims2mean53
allsims <- c("sims1sd41", "sims1sd47", "sims1sd53", 
    "sims2mean41", "sims2mean47", "sims2mean53")

for(i in allsims){
whichsim <- i 
# Now get the sim data for one species to start
sitezsimsmessy <- read.delim(paste0("input/phenofit/sims/", whichsim, "/fagsyl/Fitness.txt"), nrows=3, header=FALSE)

row.names(sitezsimsmessy) <- sitezsimsmessy$V1
sitezsimsmessy$V1 <- NULL
sitezsims <- as.data.frame(t(sitezsimsmessy))
names(sitezsims) <- c("loc", "lat", "lon")

phenofitfiles <- c("Fitness", "FruitIndex", "FruitMaturationDate", "LeafIndex", "LeafSenescenceDate",
                   "LeafUnfoldingDate", "MaturationIndex", "Survival", "TempSurvival")

fsfitsims <- cleanphenofitdata(phenofitfiles, paste0("sims/", whichsim, "/fagsyl/"), sitezsims)
psfitsims <- cleanphenofitdata(phenofitfiles, paste0("sims/", whichsim, "/pinsyl/"), sitezsims)
qrfitsims <- cleanphenofitdata(phenofitfiles, paste0("sims/", whichsim, "/querob/"), sitezsims)

# Get the simulation runs here ...
treatz <- read.csv(paste0("output/simsRformat/", whichsim, ".csv"))
controlrun <- treatz[which(treatz$mean==0 & treatz$sd==0), 3] # identidy the control run's fakelon


# And diff the files from from no change treatment
fsdiff <- getdiffsims(fsfitsims, treatz)
psdiff <- getdiffsims(psfitsims, treatz)
qrdiff <- getdiffsims(qrfitsims, treatz)
# Count years with issues ...
fsbadyrs <- countbadyrs(fsfitsims, treatz)
psbadyrs <- countbadyrs(psfitsims, treatz)
qrbadyrs <- countbadyrs(qrfitsims, treatz)


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
    geom_violin() + 
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

qrdiffplot <- ggplot(qrdiffdf, aes(y=value, x=as.character(sd))) +
    geom_violin() + 
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

psdiffplot <- ggplot(psdiffdf, aes(y=value, x=as.character(sd))) +
    geom_violin() + 
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsFS.pdf"), plot=fsdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsQR.pdf"), plot=qrdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsPS.pdf"), plot=psdiffplot, height=8, width=12)
}

if(grepl("mean", whichsim)){
fsdiffplot <- ggplot(fsdiffdf, aes(y=value, x=as.character(mean))) +
    geom_violin() + 
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

qrdiffplot <- ggplot(qrdiffdf, aes(y=value, x=as.character(mean))) +
    geom_violin() + 
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

psdiffplot <- ggplot(psdiffdf, aes(y=value, x=as.character(mean))) +
    geom_violin() + 
    facet_wrap(metric~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsFS.pdf"), plot=fsdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsQR.pdf"), plot=qrdiffplot, height=8, width=12)
ggsave(filename=paste0("graphs/phenofit/sims/", whichsim, "_diffmetricsPS.pdf"), plot=psdiffplot, height=8, width=12)
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
