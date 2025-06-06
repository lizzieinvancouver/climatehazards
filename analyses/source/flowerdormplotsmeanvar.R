## Started 6 June 2024 ##
## By Lizzie ##

## Cribbed from flowerdormplots.R ##

##############################################
## Look at SD x mean versus flower dormancy ##
##############################################

# Look over first 3 months
check <- do.call("rbind", tmeansims[c(1:2,10:12)])
xlimfirstplot <- c(-20,25)
xlimnextplot <- c(-0.1,1.1)


## Re order to keep variance changes together
betterorder <- c(1,3,5,7,9,2,4,6,8,10)
fakelonorder <- betterorder
simshereorder <- simshere[(betterorder)]

### Graphs for Pinus

pdf(paste0("graphs/phenofit/sims/extras/flowerendodorm/sdmeanflowerendodormPinusMeanTemp", substr(whichsite, start = 1, stop = 2), ".pdf"), width=12, height=8)
par(mfrow=c(2,5))

dormlist <- list()
for (i in c(1:length(simshereorder))){
    dfhere <- subset(check, fakelon==fakelonorder[i])
    dormlist[[i]] <- (1 / ( 1+exp(0.06*(dfhere$tempC-6)^2+(dfhere$tempC-6))))
    densityhere <- density(dfhere$tempC)
    densityhere$y = densityhere$y/max(densityhere$y)
    plot(densityhere, xlim=xlimfirstplot, xlab="Mean Temp from Oct-Feb (50 years)", 
        main=paste0("For sim", simstorun, " and site ", substr(whichsite, start = 1, stop = 2), "\n + ", dfhere$meanttreat[1], 
        "deg Var ", dfhere$sdtreat[1]))
    # Below, quick way to show the f(x) on the data, but hacked y value (by multiplying by 0.1) so it shows up on the same graph well ...
    # ... so ignore the height of the f(x) curve
    dormcurvex <- seq(-20,30, length.out=1000)
    dormcurve <- (1 / ( 1+exp(0.06*(dormcurvex-6)^2+(dormcurvex-6))))
    points(dormcurve~dormcurvex, pch=16, col="lightblue")
    abline(h=mean(dormlist[[i]]), col="dodgerblue")
}
dev.off()

pdf(paste0("graphs/phenofit/sims/extras/flowerendodorm/sdmeanflowerendodormPinus", substr(whichsite, start = 1, stop = 2), ".pdf"), width=12, height=8)
par(mfrow=c(2,5))
for (i in c(1:length(simshereorder))){
    plot(density(dormlist[[i]]), xlim=xlimnextplot, xlab="endodormancy result", main=paste0("Variance at ", simshereorder[i]))
}
dev.off()

#######################################
### Graphs for Fagus

pdf(paste0("graphs/phenofit/sims/extras/flowerendodorm/sdmeanflowerendodormFagusMeanTemp", substr(whichsite, start = 1, stop = 2), ".pdf"), width=12, height=8)
par(mfrow=c(2,5))
dormlist <- list()
for (i in c(1:length(simshereorder))){
    dfhere <- subset(check, fakelon==fakelonorder[i])
    # Fagus is simple, it accumulates below Vb = 13.0
    Vb <- 13.0
    dormlist[[i]] <- ifelse(dfhere$tempC<Vb, 1, 0)
    densityhere <- density(dfhere$tempC)
    densityhere$y = densityhere$y/max(densityhere$y)
    plot(densityhere, xlim=xlimfirstplot, xlab="Mean Temp from Oct-Feb (50 years)", 
        main=paste0("For sim", simstorun, " and site ", substr(whichsite, start = 1, stop = 2), "\n MeanVar at ", simshereorder[i]))
    abline(v=Vb,  col="lightblue", lwd=2)
    abline(h=mean(dormlist[[i]]), col="dodgerblue", lwd=2)
}
dev.off()

pdf(paste0("graphs/phenofit/sims/extras/flowerendodorm/sdmeanflowerendodormFagus", substr(whichsite, start = 1, stop = 2), ".pdf"), width=12, height=8)
par(mfrow=c(2,5))
for (i in c(1:length(simshereorder))){
    plot(density(dormlist[[i]]), xlim=xlimnextplot, xlab="endodormancy result", main=paste0("MeanVar at ", simshereorder[i]))
}
dev.off()

#######################################
### Graphs for Quercus

pdf(paste0("graphs/phenofit/sims/extras/flowerendodorm/sdmeanflowerendodormQuercusMeanTemp", substr(whichsite, start = 1, stop = 2), ".pdf"), width=12, height=8)
par(mfrow=c(2,5))
dormlist <- list()
for (i in c(1:length(simshereorder))){
    dfhere <- subset(check, fakelon==fakelonorder[i])
    Topt <- 2.68
    Tmin <- -49.93
    Tmax <- 44.22
    alpha  <- log(2)/(log((Tmax-Tmin)/(Topt-Tmin)))
    dormlist[[i]] <- ((2*(dfhere$tempC-Tmin)^alpha) * ((Topt-Tmin)^alpha) - (dfhere$tempC-Tmin)^(2*alpha))/
        ((Topt-Tmin)^(2*alpha))
    densityhere <- density(dfhere$tempC)
    densityhere$y = densityhere$y/max(densityhere$y)
    plot(densityhere, xlim=xlimfirstplot,  xlab="Mean Temp from Oct-Feb (50 years)", 
        main=paste0("For sim", simstorun, " and site ", substr(whichsite, start = 1, stop = 2), "\n MeanVar at ", simshereorder[i]))
    # Below, quick way to show the f(x) on the data, but hacked y value (by multiplying by 0.1) so it shows up on the same graph well ...
    # ... so ignore the height of the f(x) curve
    dormcurvex <- seq(-20,30, length.out=1000)
    dormcurve <- ((2*(dormcurvex-Tmin)^alpha) * ((Topt-Tmin)^alpha) - (dormcurvex-Tmin)^(2*alpha))/
        ((Topt-Tmin)^(2*alpha))
    points(dormcurve~dormcurvex, pch=16, col="lightblue")
    abline(h=mean(dormlist[[i]]), col="dodgerblue")
}
dev.off()

pdf(paste0("graphs/phenofit/sims/extras/flowerendodorm/sdmeanflowerendodormQuercus", substr(whichsite, start = 1, stop = 2), ".pdf"), width=12, height=8)
par(mfrow=c(2,5))
for (i in c(1:length(simshereorder))){
    plot(density(dormlist[[i]]), xlim=xlimnextplot, xlab="endodormancy result", main=paste0("MeanVar at ", simshereorder[i]))
}
dev.off()