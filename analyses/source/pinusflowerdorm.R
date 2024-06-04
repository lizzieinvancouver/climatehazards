## Started 4 July 2023 ##
## By Lizzie ##

# Trying to look at what variance shifts do in Pinus ...
# Written to be sourced in calcclimatesims.R 

#######################################
## Look at SD versus flower dormancy ##
#######################################

# Look over first 3 months
check <- do.call("rbind", tmeansims[1:3])

pdf(paste0("graphs/phenofit/sims/extras/pinusflowerendodormMeanTemp", substr(whichsite, start = 1, stop = 2), ".pdf"), width=12, height=4)
par(mfrow=c(1,5))
dormlist <- list()
for (i in c(1:length(varchanges))){
    dfhere <- subset(check, sdtreat==varchanges[i])
    dormlist[[i]] <- (1 / ( 1+exp(0.06*(dfhere$tempC-6)^2+(dfhere$tempC-6))))
    densityhere <- density(dfhere$tempC)
    densityhere$y = densityhere$y/max(densityhere$y)
    plot(densityhere, xlab="Mean Temp from Jan-Mar (50 years)", 
        main=paste0("For sim", simstorun, " and site ", substr(whichsite, start = 1, stop = 2), "\n Variance at ", varchanges[i]))
    # Below, quick way to show the f(x) on the data, but hacked y value (by multiplying by 0.1) so it shows up on the same graph well ...
    # ... so ignore the height of the f(x) curve
    dormcurvex <- seq(-20,30, length.out=1000)
    dormcurve <- (1 / ( 1+exp(0.06*(dormcurvex-6)^2+(dormcurvex-6))))
    points(dormcurve~dormcurvex, pch=16, col="lightblue")
    abline(h=mean(dormlist[[i]]), col="dodgerblue")
}
dev.off()

pdf(paste0("graphs/phenofit/sims/extras/pinusflowerendodorm", substr(whichsite, start = 1, stop = 2), ".pdf"), width=12, height=4)
par(mfrow=c(1,5))
for (i in c(1:length(varchanges))){
    plot(density(dormlist[[i]]), xlab="endodormancy result", main=paste0("Variance at ", varchanges[i]))
}
dev.off()

#######################################