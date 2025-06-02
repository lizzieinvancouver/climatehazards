## Started 25 September 2024 ##
## By Lizzie ##

## Trying to make fruit maturation plots to see if it is too hot in summer for some simulations ##
## Focusing on mean warming x variance FIRST as that is the most likely place to see something ... ##
## Meaning: I am not sure I will make fruitmatplots.R ##

# Look over July to October
check <- do.call("rbind", tmeansims[c(7:10)])
xlimfirstplot <- c(0,40)
xlimnextplot <- c(-0.1,1.1)


## Re order to keep variance changes together
betterorder <- c(1,3,5,7,9,2,4,6,8,10)
fakelonorder <- betterorder
simshereorder <- simshere[(betterorder)]


#######################################
### Graphs for Fagus

pdf(paste0("graphs/phenofit/sims/extras/fruitmat/sdmeanfruitmatFagusMeanTemp", substr(whichsite, start = 1, stop = 2), ".pdf"), width=12, height=8)
par(mfrow=c(2,5))
dormlist <- list()
for (i in c(1:length(simshereorder))){ # i  <- 1
    dfhere <- subset(check, fakelon==fakelonorder[i])
    # Fagus is Wang and Engel, see issue #13 for more details
    Topt <- 5.002
    Tmin <- 0
    Tmax <- 40
    alpha  <- log(2)/(log((Tmax-Tmin)/(Topt-Tmin)))
    dormlist[[i]] <- ((2*(dfhere$tempC-Tmin)^alpha) * ((Topt-Tmin)^alpha) - (dfhere$tempC-Tmin)^(2*alpha))/
        ((Topt-Tmin)^(2*alpha))
    densityhere <- density(dfhere$tempC)
    densityhere$y = densityhere$y/max(densityhere$y)
    plot(densityhere, xlim=xlimfirstplot, xlab="Mean Temp from Jul-Oct (50 years)", 
        main=paste0("For sim", simstorun, " and site ", substr(whichsite, start = 1, stop = 2), "\n MeanVar at ", simshereorder[i]))
    dormcurvex <- seq(0,40, length.out=1000)
    dormcurve <- ((2*(dormcurvex-Tmin)^alpha) * ((Topt-Tmin)^alpha) - (dormcurvex-Tmin)^(2*alpha))/
        ((Topt-Tmin)^(2*alpha))
    points(dormcurve~dormcurvex, pch=16, col="lightblue")    
    abline(h=mean(dormlist[[i]], na.rm=TRUE), col="dodgerblue", lwd=2)
}
dev.off()

pdf(paste0("graphs/phenofit/sims/extras/fruitmat/sdmeanfruitmatFagus", substr(whichsite, start = 1, stop = 2), ".pdf"), width=12, height=8)
par(mfrow=c(2,5))
for (i in c(1:length(simshereorder))){
	dormlistupdated <- dormlist[[i]]
	dormlistupdated[which(is.nan(dormlistupdated)==TRUE)] <- 0
    plot(density(dormlistupdated), xlim=xlimnextplot, xlab="maturation result", main=paste0("MeanVar at ", simshereorder[i]))
}
dev.off()