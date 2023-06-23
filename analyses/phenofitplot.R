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
## by years is not super useful ... 
makequickplotsyrs <- function(fitdf, leafdf, matdf, filename, ylimhere, xlimhere){
    colz <- viridis(nrow(sitez))
    pdf(paste("graphs/phenofit/historical/", filename, ".pdf", sep=""), width=12, height=4)
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

makequickplotsyrs(fsfit[["Fitness"]], fsfit[["LeafIndex"]], fsfit[["MaturationIndex"]],
               "byyears_historical3metricsSitesOverlayFagus", c(0,1.1), c(1945, 2021))
makequickplotsyrs(qrfit[["Fitness"]], qrfit[["LeafIndex"]], qrfit[["MaturationIndex"]],
               "byyears_historical3metricsSitesOverlayQuercus", c(0,1.1), c(1945, 2021))
makequickplotsyrs(psfit[["Fitness"]], psfit[["LeafIndex"]], psfit[["MaturationIndex"]],
               "byyears_historical3metricsSitesOverlayPinus", c(0,1.1), c(1945, 2021))

fsfitdf <- do.call("rbind", fsfit)
fsfitdf$sp <- "Fagus"
qrfitdf <- do.call("rbind", qrfit)
qrfitdf$sp <- "Quercus"
psfitdf <- do.call("rbind", psfit)
psfitdf$sp <- "Pinus"

alldat <- rbind(fsfitdf, qrfitdf, psfitdf)

## Working on getting the most limiting factor
library(matrixStats)

threemetrics <- c("Fitness", "Survival",  "FruitIndex", "MaturationIndex")
fsfit3prep<-  fsfitdf[which(fsfitdf$metric %in% threemetrics),]
fsfit3 <- reshape(fsfit3prep, idvar=c("lat", "lon", "year", "sp"), timevar="metric", direction = "wide")
test <- as.matrix(fsfit3[1:3,c(8,6,7)])
test[2,2] <-   0.999
test[1,]  <- 1.0
rowRanks(test, ties.method = c("first"))
# Need to pull out the value that gets ranked '1' ... if I want this mwau of plotting

#
# For now, just plot them building up
onedf4 <- alldat[which(alldat$metric %in% threemetrics),]
onedf3 <- onedf4[which(onedf4$metric!="MaturationIndex"),]
onedf3wide <- reshape(onedf3, idvar=c("lat", "lon", "year", "sp"), timevar="metric", 
    direction = "wide")
onedf3wide$SurvFruitIndex <- onedf3wide$value.Survival*onedf3wide$value.FruitIndex
onedf3wide$value.FruitIndex <- NULL

onedf3long <- reshape(onedf3wide, idvar=c("lat", "lon", "year", "sp"), 
    varying=list(c("value.Survival", "SurvFruitIndex","value.Fitness")), v.names="value",
    direction = "long")
names(onedf3long)[which(names(onedf3long)=="time")] <- "metric"
onedf3long$metric[which(onedf3long$metric==2)] <- "2_value.SurvFruitIndex"
onedf3long$metric[which(onedf3long$metric==1)] <- "1_value.Survival"
onedf3long$metric[which(onedf3long$metric==3)] <- "3_value.Fitness"

threemetricsplot <- ggplot(onedf3long, aes(y=value, x=as.character(lat))) +
    geom_violin(trim=FALSE, col="lightgray") + 
    stat_summary(
        fun.data = "mean_sdl",  fun.args = list(mult = 1), 
        geom = "pointrange", color = "dodgerblue", lwd=1
    ) +
    facet_wrap(sp~metric, scales="free") +
    ylim(-0.25, 1.2) + 
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename="graphs/phenofit/historical/fitnessBuildup.pdf", plot=threemetricsplot, height=8, width=12)
# losing rows here due to ylim(-0.25, 1.2) code, see fitnessBuildup_varyingylim.pdf for what it looks like without

## End working on getting the most limiting factor




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
    

ggsave(filename="graphs/phenofit/historical/byyears_ggfitness1951.pdf", plot=ggfit, height=8, width=12)
ggsave(filename="graphs/phenofit/historical/byyears_ggleaf1951.pdf", plot=ggleaf, height=8, width=12)
ggsave(filename="graphs/phenofit/historical/byyears_ggmat1951.pdf", plot=ggmat, height=8, width=12)

# Interpreting the reasons for output
# TempSurvival is fine, survival seems not super informative ... 

phenofitfiles <- c("Fitness", "FruitIndex", "FruitMaturationDate", "LeafIndex", "LeafSenescenceDate",
                   "LeafUnfoldingDate", "MaturationIndex", "Survival", "TempSurvival")

alldatwidewNA <- reshape(alldat, idvar=c("lat", "lon", "year", "sp"), timevar="metric", direction = "wide")
alldatwide <- alldatwidewNA
alldatwide[alldatwide == -999] <- 366


## Doing the plots with base R
if(FALSE){
    par(mfrow=c(1,3))
    for(sphere in c("Fagus", "Pinus", "Quercus")){
        dhere <- alldatwide[which(alldatwide$sp==sphere),]
        plot(value.Fitness~value.LeafUnfoldingDate, data=dhere, type="n", 
            main=paste0("Historical data results for ", sphere))
        percentz <- c()
        for(i in c(1:length(unique(dhere$lat)))){
            subby <- dhere[which(dhere$lat==unique(dhere$lat)[i]),]
            subbywleafout <- subset(subby, value.LeafUnfoldingDate<365)
            points(value.Fitness~value.LeafUnfoldingDate, data=subbywleafout, col=colz[i])
            percentnoleafout <- 100*(1-(nrow(subbywleafout)/nrow(subby)))
            percentz[i] <- paste0(unique(dhere$lat)[i], " deg; % no leafout: ", round(percentnoleafout, 1))
        }
        legend("right", legend=percentz, lty=rep(1, length(percentz)), col=colz, bty="n", cex = 0.75)
    }
}

makephenofitxyplots_withlatedatesprinted <- function(df, xcolname, ycolname, filename){
pdf(paste("graphs/phenofit/historical/allspp_xypoints_wprint_", filename, ".pdf", sep=""), width=12, height=4)
par(mfrow=c(1,3))
for(sphere in c("Fagus", "Pinus", "Quercus")){
    dhere <- df[which(df["sp"]==sphere),]
    plot(unlist(dhere[ycolname])~unlist(dhere[xcolname]), type="n", 
        xlab=xcolname, ylab=ycolname,
        main=paste0("Historical data results for ", sphere))
    percentz <- c()
    for(i in c(1:length(unique(dhere$lat)))){
        subby <- dhere[which(dhere$lat==unique(dhere$lat)[i]),]
        subbywleafout <- subby[which(as.numeric(unlist(subby[xcolname]))<365),]
        points(unlist(subbywleafout[ycolname])~unlist(subbywleafout[xcolname]), col=colz[i], pch=16)
        percentnoleafout <- 100*(1-(nrow(subbywleafout)/nrow(subby)))
        percentz[i] <- paste0(unique(dhere$lat)[i], " deg; % no event: ", round(percentnoleafout, 1))
    }
    legend("right", legend=percentz, lty=rep(1, length(percentz)), col=colz, bty="n", cex = 0.75)
}
dev.off()
}


makephenofitxyplots<- function(df, xcolname, ycolname, filename){
pdf(paste("graphs/phenofit/historical/allspp_xypoints_", filename, ".pdf", sep=""), width=12, height=4)
par(mfrow=c(1,3))
for(sphere in c("Fagus", "Pinus", "Quercus")){
    dhere <- df[which(df["sp"]==sphere),]
    plot(unlist(dhere[ycolname])~unlist(dhere[xcolname]), type="n", 
        xlab=xcolname, ylab=ycolname,
        main=paste0("Historical data results for ", sphere))
    for(i in c(1:length(unique(dhere$lat)))){
        subby <- dhere[which(dhere$lat==unique(dhere$lat)[i]),]
        points(unlist(subby[ycolname])~unlist(subby[xcolname]), col=colz[i], pch=16)
    }
    legend("left", legend=unique(dhere$lat), lty=rep(1, length(unique(dhere$lat))), col=colz, bty="n", cex = 0.75)
}
dev.off()
}

colz <- alpha(viridis(nrow(sitez)), 0.5)
makephenofitxyplots_withlatedatesprinted(alldatwide, "value.LeafUnfoldingDate", "value.Fitness", "leafunfolddate_vsfitness")
makephenofitxyplots_withlatedatesprinted(alldatwide, "value.FruitMaturationDate", "value.Fitness", "fruitmatdate_vsfitness")

makephenofitxyplots(alldatwide, "value.Survival", "value.Fitness", "survival_vsfitness")
makephenofitxyplots(alldatwide, "value.LeafIndex", "value.Fitness", "leafindex_vsfitness")
makephenofitxyplots(alldatwide, "value.FruitIndex", "value.Fitness", "fruitindex_vsfitness")
makephenofitxyplots(alldatwide, "value.MaturationIndex", "value.Fitness", "fruitmatindex_vsfitness")

# by species... with ggplot
if(FALSE){
justfagus <- subset(alldatwide, sp="Fagus")
justfagus$lat <- as.character(justfagus$lat)
ggplot(justfagus, aes(x=value.LeafUnfoldingDate, y=value.Fitness, col=lat)) +
    geom_point() +
    ylab("Fitness") +
    xlab("Leaf Unfolding Date") +
    facet_wrap(lat~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
}

## Historgrams, in ggplot (writing a f(x) was not worth the effort as hist command is a pain)
fithist <- ggplot(alldatwide, aes(x=value.Fitness, col=as.character(lat), fill=as.character(lat)))  +
    geom_histogram() +
    xlab("Fitness") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

survhist <- ggplot(alldatwide, aes(x=value.Survival, col=as.character(lat), fill=as.character(lat))) +
    geom_histogram() +
    xlab("Survival") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

leafdatehist <- ggplot(alldatwide, aes(x=value.LeafUnfoldingDate, col=as.character(lat), fill=as.character(lat)))  +
    geom_histogram() +
    xlab("Leaf Unfolding Date") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

frmatdatehist <- ggplot(alldatwide, aes(x=value.FruitMaturationDate, col=as.character(lat), fill=as.character(lat)))  +
    geom_histogram() +
    xlab("Fruit Maturation Date") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

leafindhist <- ggplot(alldatwide, aes(x=value.LeafIndex, col=as.character(lat), fill=as.character(lat)))  +
    geom_histogram() +
    xlab("Leaf Index") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


frindhist <- ggplot(alldatwide, aes(x=value.FruitIndex, col=as.character(lat), fill=as.character(lat)))  +
    geom_histogram() +
    xlab("Fruit Index") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


matindhist <- ggplot(alldatwide, aes(x=value.MaturationIndex, col=as.character(lat), fill=as.character(lat)))  +
    geom_histogram() +
    xlab("Maturation Index") +
    facet_wrap(sp~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

p <- list(fithist, survhist, leafdatehist, frmatdatehist, leafindhist, frindhist, matindhist)

pdf("graphs/phenofit/historical/allspp_histograms.pdf", height=4, width=10)
for (i in seq(length(p))) {
  print(p[[i]])
}
dev.off()

