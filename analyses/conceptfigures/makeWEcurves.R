## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## libraries
library(ggplot2)

## set working directory

if(length(grep("lizzie", getwd()))>0) { 
  setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses/conceptfigures/")
} else setwd("/Users/Lizzie/Documents/git/projects/vinmisc/vassalphen/analyses/wangengel/")

source("Script_functions_pheno_models.R")

testclim.avg <- seq(-5,42, length.out=200)

# Nacho has an alpha f(x) but I am not sure when to use it
#testalpha <- Alphafx(testclim.min, testclim.max, 29) #do we need Topt first?

# Wang & Engel model seems to want static values for first 4 inputs:
# WangEngelfx <- function(Tmin, Tmax, Topt, Alpha, Tavg)
wangeng24 <- WangEngelfx(0, 40, 26, 2.85, testclim.avg) # Nacho's budburst (with alpha from Inakis other models)
wangeng27 <- WangEngelfx(0, 40, 29, 2.85, testclim.avg) # Nacho's flowering
wangeng26 <- WangEngelfx(0, 40, 22, 2.85, testclim.avg) # Nacho's veraison
#should all Tmax be 40 that's what Inaki and Nacho use - NOPE, graph gets weird

# Now just format and plot
wangeng24clim <- data.frame(we=wangeng24[,1], tempC=testclim.avg,
   temp=testclim.avg)
wangeng27clim <- data.frame(we=wangeng27[,1], tempC=testclim.avg,
   temp=testclim.avg)
wangeng26clim <- data.frame(we=wangeng26[,1], tempC=testclim.avg,
                            temp=testclim.avg)

wangeng24clim.sm <- subset(wangeng24clim, we>=0)
wangeng27clim.sm <- subset(wangeng27clim, we>=0)
wangeng26clim.sm <- subset(wangeng26clim, we>=0)

# Some simple curves 
plot(we~temp, data=wangeng27clim.sm, type="l", xlim=c(0,40))
points(we~temp, data=wangeng24clim.sm, type="l", col="red")
points(we~temp, data=wangeng26clim.sm, type="l", col="blue")

## As above, but in a pdf! 
pdf(file.path("graphs/wengengsimple.pdf"), width = 8, height = 7)
plot(we~tempC, data=wangeng27clim.sm,  xlim=c(-5,42), ylab="Developmental rate",
     xlab=expression(paste("Temperature "( degree~C))), type="n")
points(we~tempC, data=wangeng27clim.sm, type="l", lwd=2, col="gold")
points(we~tempC, data=wangeng24clim.sm, type="l", lwd=2, col="dodgerblue")
points(we~tempC, data=wangeng26clim.sm, type="l", lwd=2, col="darkorchid4")
dev.off()
