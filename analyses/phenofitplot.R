## Started 6 avril 2023 ##
## By Lizzie ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(data.table)
library(ggplot2)
library(viridis)

## set working directory
setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses")

## This is in progress!
# Need to think through how best to do this ...
# May also want to think on bash scripts to automate some of the file stuff

## get the data
sitezmessy <- read.delim("input/phenofit/fagsyl_19512020/Fitness.txt", nrows=3, header=FALSE)

row.names(sitezmessy) <- sitezmessy$V1
sitezmessy$V1 <- NULL
sitez <- as.data.frame(t(sitezmessy))
names(sitez) <- c("loc", "lat", "lon")

cleanphenofitdata <- function(filename, sitedf){
    d <- read.delim(paste("input/phenofit/", filename, ".txt", sep=""),
                    skip=3, header=FALSE)
    dfhere <- data.frame(lat=numeric(), lon=numeric(), year=numeric(),
       year=numeric(), value=numeric())
    for(i in c(1:nrow(sitedf))){
       dftobind <- data.frame(lat=rep(sitedf[["lat"]][i], nrow(d)),
                           lon=rep(sitedf[["lon"]][i], nrow(d)),
                           year=as.numeric(unlist(d["V1"])),
                           value=as.numeric(unlist(d[,i+1])))
       dfhere <- rbind(dfhere, dftobind)
   }
   return(dfhere)
}

fsfit <- cleanphenofitdata("fagsyl_19512020/Fitness", sitez)
psfit <- cleanphenofitdata("pinsyl_19512020/Fitness", sitez)
qrfit <- cleanphenofitdata("querob_19512020/Fitness", sitez)

fsleafi <- cleanphenofitdata("fagsyl_19512020/LeafIndex", sitez)
psleafi <- cleanphenofitdata("pinsyl_19512020/LeafIndex", sitez)
qrleafi <- cleanphenofitdata("querob_19512020/LeafIndex", sitez)

fsmati <- cleanphenofitdata("fagsyl_19512020/MaturationIndex", sitez)
psmati <- cleanphenofitdata("pinsyl_19512020/MaturationIndex", sitez)
qrmati <- cleanphenofitdata("querob_19512020/MaturationIndex", sitez)


makequickplots <- function(fitdf, leafdf, matdf, filename, ylimhere, xlimhere){
    colz <- viridis(nrow(sitez))
    pdf(paste("graphs/phenofit/", filename, ".pdf", sep=""), width=12, height=4)
    par(mfrow=c(1, 3))
    plot(fitdf[["value"]] ~ fitdf[["year"]], type="n",
     xlab="year", ylab="fitness", ylim=ylimhere, xlim=xlimhere)
    legend("topleft", legend=sitez[["lat"]], lty=rep(1, nrow(sitez)), col=colz, bty="n")
    for(i in c(1:nrow(sitez))){
        dfhere <- subset(fitdf, lat==sitez[["lat"]][i] & lon==sitez[["lon"]][i])
        lines(dfhere[["value"]] ~ dfhere[["year"]], col=colz[i])
}
    plot(leafdf[["value"]] ~ leafdf[["year"]], type="n",
     xlab="year", ylab="leaf infex", ylim=ylimhere, xlim=xlimhere)
    legend("topleft", legend=sitez[["lat"]], lty=rep(1, nrow(sitez)), col=colz, bty="n")

    for(i in c(1:nrow(sitez))){
        dfhere <- subset(leafdf, lat==sitez[["lat"]][i] & lon==sitez[["lon"]][i])
        lines(dfhere[["value"]] ~ dfhere[["year"]], col=colz[i])
}
    plot(matdf[["value"]] ~ matdf[["year"]], type="n",
         xlab="year", ylab="maturation infex", ylim=ylimhere, xlim=xlimhere)
    legend("topleft", legend=sitez[["lat"]], lty=rep(1, nrow(sitez)), col=colz, bty="n")

    for(i in c(1:nrow(sitez))){
        dfhere <- subset(matdf, lat==sitez[["lat"]][i] & lon==sitez[["lon"]][i])
        lines(dfhere[["value"]] ~ dfhere[["year"]], col=colz[i])
}
dev.off()
}

makequickplots(fsfit, fsleafi, fsmati, "historical3metricsSitesOverlayFagus", c(0,1.1), c(1945, 2021))
makequickplots(qrfit, qrleafi, qrmati, "historical3metricsSitesOverlayQuercus", c(0,1.1), c(1945, 2021))
makequickplots(psfit, psleafi, psmati, "historical3metricsSitesOverlayPinus", c(0,1.1), c(1945, 2021))

fsfit$sp <- "Fagus"
qrfit$sp <- "Quercus"
psfit$sp <- "Pinus"

fsleafi$sp <- "Fagus"
qrleafi$sp <- "Quercus"
psleafi$sp <- "Pinus"

fsmati$sp <- "Fagus"
qrmati$sp <- "Quercus"
psmati$sp <- "Pinus"

allsppfithack <- rbind(fsfit, qrfit, psfit)
allsppleafhack <- rbind(fsleafi, qrleafi, psleafi)
allsppmathack <- rbind(fsmati, qrmati, psmati)


ggfit <- ggplot(allsppfithack, aes(x=year, y=value, color=sp)) +
    geom_line() +
    ylab("fitness") + 
    facet_wrap(lat~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggleaf <- ggplot(allsppleafhack, aes(x=year, y=value, color=sp)) +
    geom_line() +
    ylab("leaf index") + 
    facet_wrap(lat~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    

ggmat <- ggplot(allsppmathack, aes(x=year, y=value, color=sp)) +
    geom_line() +
    ylab("maturation index") + 
    facet_wrap(lat~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    

ggsave(filename="graphs/phenofit/historicalggfitness1951.pdf", plot=ggfit, height=8, width=12)
ggsave(filename="graphs/phenofit/historicalggleaf1951.pdf", plot=ggleaf, height=8, width=12)
ggsave(filename="graphs/phenofit/historicalggmat1951.pdf", plot=ggmat, height=8, width=12)
