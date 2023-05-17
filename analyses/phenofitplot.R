## Started 6 avril 2023 ##
## By Lizzie ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(data.table)
library(ggplot2)
library(gridExtra)
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

phenofitfiles <- c("Fitness", "FruitIndex", "FruitMaturationDate", "LeafIndex", "LeafSenescenceDate",
                   "LeafUnfoldingDate", "MaturationIndex", "Survival", "TempSurvival")

cleanphenofitdata <- function(phenofitfilelist, filename, sitedf){
    df <- list()
    for (phenometric in phenofitfilelist){     
        d <- read.delim(paste("input/phenofit/", filename, phenometric, ".txt", sep=""),
                    skip=3, header=FALSE)
        dfhere <- data.frame(metric=character(), lat=numeric(), lon=numeric(), year=numeric(),
                             year=numeric(), value=numeric())
        for(i in c(1:nrow(sitedf))){
            dftobind <- data.frame(metric=rep(phenometric, nrow(d)),
                           lat=rep(sitedf[["lat"]][i], nrow(d)),
                           lon=rep(sitedf[["lon"]][i], nrow(d)),
                           year=as.numeric(unlist(d["V1"])),
                           value=as.numeric(unlist(d[,i+1])))
            dfhere <- rbind(dfhere, dftobind)
        }
       df[[phenometric]] <- dfhere
        }
   return(df)
}


fsfit <- cleanphenofitdata(phenofitfiles, "fagsyl_19512020/", sitez)
psfit <- cleanphenofitdata(phenofitfiles, "pinsyl_19512020/", sitez)
qrfit <- cleanphenofitdata(phenofitfiles, "querob_19512020/", sitez)



## plot the data

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
         xlab="year", ylab="maturation index", ylim=ylimhere, xlim=xlimhere)
    legend("topleft", legend=sitez[["lat"]], lty=rep(1, nrow(sitez)), col=colz, bty="n")

    for(i in c(1:nrow(sitez))){
        dfhere <- subset(matdf, lat==sitez[["lat"]][i] & lon==sitez[["lon"]][i])
        lines(dfhere[["value"]] ~ dfhere[["year"]], col=colz[i])
}
dev.off()
}

makequickplots(fsfit[["Fitness"]], fsfit[["LeafIndex"]], fsfit[["MaturationIndex"]],
               "historical3metricsSitesOverlayFagus", c(0,1.1), c(1945, 2021))
makequickplots(qrfit[["Fitness"]], qrfit[["LeafIndex"]], qrfit[["MaturationIndex"]],
               "historical3metricsSitesOverlayQuercus", c(0,1.1), c(1945, 2021))
makequickplots(psfit[["Fitness"]], psfit[["LeafIndex"]], psfit[["MaturationIndex"]],
               "historical3metricsSitesOverlayPinus", c(0,1.1), c(1945, 2021))

fsfitdf <- do.call("rbind", fsfit)
fsfitdf$sp <- "Fagus"
qrfitdf <- do.call("rbind", qrfit)
qrfitdf$sp <- "Quercus"
psfitdf <- do.call("rbind", psfit)
psfitdf$sp <- "Pinus"

alldat <- rbind(fsfitdf, qrfitdf, psfitdf)

ggfit <- ggplot(subset(alldat, metric=="Fitness"), aes(x=year, y=value, color=sp)) +
    geom_line() +
    ylab("fitness") + 
    facet_wrap(lat~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggleaf <- ggplot(subset(alldat, metric=="LeafIndex"), aes(x=year, y=value, color=sp)) +
    geom_line() +
    ylab("leaf index") + 
    facet_wrap(lat~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    

ggmat <- ggplot(subset(alldat, metric=="MaturationIndex"), aes(x=year, y=value, color=sp)) +
    geom_line() +
    ylab("maturation index") + 
    facet_wrap(lat~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    

ggsave(filename="graphs/phenofit/historicalggfitness1951.pdf", plot=ggfit, height=8, width=12)
ggsave(filename="graphs/phenofit/historicalggleaf1951.pdf", plot=ggleaf, height=8, width=12)
ggsave(filename="graphs/phenofit/historicalggmat1951.pdf", plot=ggmat, height=8, width=12)

# Interpreting the reasons for output
# TempSurvival is fine, survival seems not super informative ... 

phenofitfiles <- c("Fitness", "FruitIndex", "FruitMaturationDate", "LeafIndex", "LeafSenescenceDate",
                   "LeafUnfoldingDate", "MaturationIndex", "Survival", "TempSurvival")

alldatwide <- reshape(alldat, idvar=c("lat", "lon", "year", "sp"), timevar="metric", direction = "wide")


surv <- ggplot(alldatwide, aes(x=value.Survival, y=value.Fitness, col=as.character(lat))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Survival") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

leafdate <- ggplot(alldatwide, aes(x=value.LeafUnfoldingDate, y=value.Fitness, col=as.character(lat))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Leaf Unfolding Date") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

frmatdate <- ggplot(alldatwide, aes(x=value.FruitMaturationDate, y=value.Fitness, col=as.character(lat))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Fruit Maturation Date") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

frleafdate <- ggplot(alldatwide, aes(x=value.LeafUnfoldingDate, y=value.FruitMaturationDate, col=as.character(lat))) +
    geom_line() +
    xlab("Leaf Unfolding Date") +
    ylab("Fruit Maturation Date") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

leafind <- ggplot(alldatwide, aes(x=value.LeafIndex, y=value.Fitness, col=as.character(lat))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Leaf Index") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


frind <- ggplot(alldatwide, aes(x=value.FruitIndex, y=value.Fitness, col=as.character(lat))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Fruit Index") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


matind <- ggplot(alldatwide, aes(x=value.MaturationIndex, y=value.Fitness, col=as.character(lat))) +
    geom_line() +
    ylab("Fitness") +
    xlab("Maturation Index") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


p <- list(leafdate, frmatdate, frleafdate, leafind, frind, matind)

pdf("graphs/phenofit/historicalfitness1951.pdf", height=4, width=10)
for (i in seq(length(p))) {
  print(p[[i]])
}
dev.off()


# by species...
ggplot(subset(alldatwide, sp="Fagus"), aes(x=value.LeafUnfoldingDate, y=value.Fitness)) +
    geom_line() +
    ylab("Fitness") +
    xlab("Leaf Unfolding Date") +
    facet_wrap(lat~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
