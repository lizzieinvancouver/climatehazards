## Started 4 July 2023 ##
## By Lizzie ##

# Trying to look at what variance shifts do in Pinus ...
# Written to be sourced in calcclimatesims.R 

#######################################
## Look at SD versus flower dormancy ##
#######################################

# Look over first 3 months
check <- do.call("rbind", tmeansims[1:3])

pdf("graphs/phenofit/sims/extras/pinusflowerendodormMeanTemp.pdf", width=12, height=4)
par(mfrow=c(1,5))
dormlist <- list()
for (i in c(1:length(varchanges))){
    dfhere <- subset(check, sdtreat==varchanges[i])
    dormlist[[i]] <- (1 / ( 1+exp(0.06*(dfhere$tempC-6)^2+(dfhere$tempC-6))))
     hist(dfhere$tempC, xlab="Mean Temp from Jan-Mar (50 years)", main=paste0("Variance at ", varchanges[i]))
     dormcurvex <- seq(-20,30, length.out=1000)
     dormcurve <- (1 / ( 1+exp(0.06*(dormcurvex-6)^2+(dormcurvex-6))))
     points(dormcurve*500~dormcurvex, pch=16)
}
dev.off()

pdf("graphs/phenofit/sims/extras/pinusflowerendodorm.pdf", width=12, height=4)
par(mfrow=c(1,5))
for (i in c(1:length(varchanges))){
    hist(dormlist[[i]], xlab="endodormancy result", main=paste0("Variance at ", varchanges[i]))
}
dev.off()

#######################################