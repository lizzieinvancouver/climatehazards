## Started 2 June 2025 ##
## Back at CNRS, maybe now I finish this project? ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(data.table)
library(plyr)
library(ggplot2) 

## set working directory
setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses")

# get the data 
onemeandf <- read.csv("output/onemeandf.csv")
onesddf <- read.csv("output/onesddf.csv")
onemeansddf <- read.csv("output/onemeansddf.csv")
meansimslookup <- read.csv("output/simsRformat/sims2mean41.csv")
sdsimslookup <- read.csv("output/simsRformat/sims1sd41.csv")
meansdsimslookup <- read.csv("output/simsRformat/sims3meansd41.csv")

if(FALSE){
onemeandf[["simhere"]] <- substr(sub("\\..*", "", onemeandf[["X"]]), 1, 
	nchar(sub("\\..*", "", onemeandf[["X"]]))-2)
onemeandf[["X"]] <- NULL
names(meansimslookup)[names(meansimslookup)=="fakelon"] <- "lon"
meansims <- merge(onemeandf, meansimslookup, by="lon")
}

cleandatafx <- function(bigsimsdf, lookuptable){
	bigsimsdf[["simhere"]] <- substr(sub("\\..*", "", bigsimsdf[["X"]]), 1, 
		nchar(sub("\\..*", "", bigsimsdf[["X"]]))-2)
	bigsimsdf[["X"]] <- NULL
	names(lookuptable)[names(lookuptable)=="fakelon"] <- "lon"
	simsclean <- merge(bigsimsdf, lookuptable, by="lon")
	print(paste("make sure no rows lost, is next row 1?"))
	print(nrow(simsclean)/nrow(bigsimsdf))
	return(simsclean)
}

meansims <- cleandatafx(onemeandf, meansimslookup)
sdsims <- cleandatafx(onesddf, sdsimslookup)
meansdsims <- cleandatafx(onemeansddf, meansdsimslookup)

allsims <- rbind(meansims, sdsims, meansdsims)

# We're just plotting fitness 
fitty <- subset(allsims, metric=="Fitness")
# And we did not do all sd x mean, so remove the two moderate SD values
fitty <- fitty[which(!fitty[["sd"]] %in% c(-0.25, 0.25)),]

table(fitty$lat, fitty$latforsim)

fitsum <-
      ddply(fitty, c("mean", "sd", "latforsim", "sp"), summarise,
      meanhere = mean(value),
      sdhere = sd(value))


testme <- subset(fitsum, sp=="Fagus" & latforsim=="41" )
testme <- subset(fitsum, latforsim=="41" )

heatmapme <- ggplot(fitsum, aes(mean, sd)) +                          
	geom_tile(aes(fill = meanhere)) +
	scale_fill_viridis_c() + 
	facet_grid(factor(latforsim, levels=c("53", "47", "41"))~
		factor(sp, levels=c("Pinus", "Fagus", "Quercus")))

ggsave(filename="graphs/phenofit/sims/heatmaps/heatmapfitness.pdf", plot=heatmapme, height=5, width=7)