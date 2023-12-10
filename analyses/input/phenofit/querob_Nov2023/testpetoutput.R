## Started 16 November 2023 ##
## By Lizzie ##


## We were worried that the results using the new Querob species parameter file for phenofit was weird... 
# so I checked (below) how much the PET changed across warming scenario (+1 vs. +6 C)
# crappy code below but it does the comparison. ## 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(data.table)
library(ggplot2)
library(viridis)


## set working directory
setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses")

# source useful f(x)s
source("source/calcclimatefxs.R")

######################################
## Grab PET data ##
######################################

petlist <- readfilestolist(paste("output/phenofitsims/ERA5LAND_pet_", c(1950:2000), "_dly.fit", sep=""), c(1950:2000))
df <- petlist[[7]]
dfsim1 <- subset(df, latlon=="41.9 1")
dfsim6 <- subset(df, latlon=="41.9 6")

par(mfrow=c(1,2))
hist(dfsim1$tempC)
hist(dfsim6$tempC)

par(mfrow=c(1,1))
plot(dfsim6$tempC~dfsim1$tempC, xlab="PET at +1 C", ylab="PET at +6 C")
abline(0,1)