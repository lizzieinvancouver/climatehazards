## Started 21 mars 2023 ##
## By Lizzie ##

## Whoo-hoo, spring! Time to finally get some analyses going on Phenofit project ##
## This codes reads in historical climate data and calculates mean and SD by month ##
## It then makes the simiulated climate data ## 

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(data.table)
library(ggplot2)
library(viridis)


## set working directory
setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses")

## To do!
# (1) Check what I have below and make tmean from sim data
# (2) Remember to keep drafting what we want to say and show in paper, so we (I) do not end up down a rabbit hole

source("source/calcclimatefxs.R")


######################################
## Grab historical data and detrend ##
######################################

# Tmin first, with some extra checking notes
# each list item should be 71 years * days in month * 9 sites
# So, for January: 19809 rows
tminlist <- readfilestolist(paste("input/ERA5LAND/ERA5LAND_tmn_", c(1950:2020), "_dly.fit", sep=""), c(1950:2020))
tminlistanom <- readfilestolistanomalies(paste("input/ERA5LAND/ERA5LAND_tmn_", c(1950:2020), "_dly.fit", sep=""), c(1950:2020))

# Should be 12 months x 9 sites 
tminsumm <- getmeansdbysitemonth(tminlist, unique(tminlist[[1]]["latlon"]), 1950, 2020)
# Should be 12 months x 9 sites x 71 years
tminsummyr <- getmeansdbysiteyrmonth(tminlist, unique(tminlist[[1]]["latlon"]), 1950, 2020)

tmindetmonths <- detrendmonthlyclimatelist(tminsummyr, tminsumm, unique(tminlist[[1]]["latlon"]))
tminlistdet <- getdailydetrendedlist(tmindetmonths, tminlistanom, unique(tminlist[[1]]["latlon"]), c(1950:2020))

# Tmax!
tmaxlist <- readfilestolist(paste("input/ERA5LAND/ERA5LAND_tmx_", c(1950:2020), "_dly.fit", sep=""), c(1950:2020))
tmaxlistanom <- readfilestolistanomalies(paste("input/ERA5LAND/ERA5LAND_tmx_", c(1950:2020), "_dly.fit", sep=""), c(1950:2020))
tmaxsumm <- getmeansdbysitemonth(tmaxlist, unique(tmaxlist[[1]]["latlon"]), 1950, 2020)
tmaxsummyr <- getmeansdbysiteyrmonth(tmaxlist, unique(tmaxlist[[1]]["latlon"]), 1950, 2020)
tmaxdetmonths <- detrendmonthlyclimatelist(tmaxsummyr, tmaxsumm, unique(tmaxlist[[1]]["latlon"]))
tmaxlistdet <- getdailydetrendedlist(tmaxdetmonths, tmaxlistanom, unique(tmaxlist[[1]]["latlon"]), c(1950:2020))

###################################################
## Build simulation data based on detrended data ##
###################################################

simstorun <- "sims2mean" # sims2mean sims1sd
sitetouse <- 5 # Pick 47.5 based on 20 Apr 2023 with Isabelle
# We now also plan to try the most southern and northern site ... which are 1 and 9

if(simstorun=="sims1sd"){
    varchanges <- c(-0.5, -0.25, 0, 0.25, 0.5) # c(0, -0.1, 0.1, 0.2)
    tempchanges <- c(0) # c(0, 0.5, 1, 2)
    simshere <- c("sd -50%", "sd -25%", "sd 0%", "sd + 25", "sd + 50%")
}

if(simstorun=="sims2mean"){
    varchanges <- c(0) # c(0, -0.1, 0.1, 0.2)
    tempchanges <- c(0, 1, 2, 3, 4, 5) # c(0, 0.5, 1, 2)
    simshere <- c("0C", "+1C", "+2C", "+3C", "+4C", "+5")
}


allchanges <- expand.grid(varchanges, tempchanges)
names(allchanges) <- c("sd", "mean")
allchanges$fakelon <- seq(1:nrow(allchanges))

whichsite <- unique(tmaxlist[[1]]["latlon"])[sitetouse,] 
whichsitewrite <- substr(whichsite, 1, 2)
whichsiteotherfiles <- substr(whichsite, 1, 4)
onesitetmin <- lapply(tminlistdet, function(x) subset(x, latlon == whichsite))
onesitetmax <- lapply(tmaxlistdet, function(x) subset(x, latlon == whichsite))


if(FALSE){
# Does SD vary based on whether you subtract mean first? No.
testingdf <- tmaxlist[[1]]
aggregate(testingdf["tempC"], testingdf["latlon"], FUN=sd)
for(i in c(1:length(unique(testingdf$latlon)))){
    subby <- testingdf[which(testingdf$latlon==unique(testingdf$latlon)[i]),]
    subby$newtemp <- subby$tempC-mean(subby$tempC, na.rm=TRUE)
    print(sd(subby$newtemp))
}
}

source("source/calcclimatesimsfxs.R")

tminsims <- makesimdata(onesitetmin, 1950, 2000, allchanges)
tmaxsims <- makesimdata(onesitetmax, 1950, 2000, allchanges)

plotsimdata(onesitetmin, tminsims, 1950, 2000, paste0(simstorun, whichsitewrite), simshere)
plotsimdataPDF(onesitetmin, tminsims, 1950, 2000, paste0(simstorun, whichsitewrite), simshere)

# Now write out the other files too... 
source("source/otherfilesformatforsims.R")

#################################
## Write out Phenofit sim data ##
#################################

listofyearshere <- unlist(unique(tminsims[[1]]["year"]))
writeoutdata(listofyearshere, tminsims, "tmn_")
writeoutdata(listofyearshere, tmaxsims, "tmx_")

# Also need the mean (tmp), which I will caculate from the sims
# There is likely a smarter way to do this, but I am just doing a loop
tmeansims <- list()
for(i in c(1:length(tminsims))){
    tminhere <- tminsims[[i]]
    tmaxhere <- tmaxsims[[i]]
    # Remarkably the below seems to work! 
    # Added benefit of showing up any impossible error where tmax and tmin formatting is not the same
    tmeanhere <- (tminhere + tmaxhere)/2 
    tmeansims[[i]] <- tmeanhere
}
writeoutdata(listofyearshere, tmeansims, "tmp_")

# Also write out the full lists to have for plotting
save(tminsims, file=paste0("output/simsRformat/tmin", simstorun, whichsitewrite, ".Rdata"))
save(tmaxsims, file=paste0("output/simsRformat/tmax", simstorun, whichsitewrite, ".Rdata"))
write.csv(allchanges, file=paste0("output/simsRformat/", simstorun, whichsitewrite, ".csv"), row.names=FALSE)
# write.csv(allchanges, file="output/phenofitsims/sims2run475mean.csv", row.names=FALSE)



##############################
## Plotting historical data ##
##############################

sitez <- unique(tminlist[[1]]["latlon"])

if(FALSE){ # Sort of slow and was just a safety check
# Compare the original data to detrended data
colz <- viridis(9)
par(mfrow=c(1,4))
for(i in c(1,4,7,11)){ # just doing a few months as this is slow
    dfplain <- tminlist[[i]]
    dfdet <- tminlistdet[[i]]
    plot(x=dfplain$tempC, y=dfdet$tempC, type="n", xlab="Min C", ylab="detrended Min C")
    for(j in c(1,4,9)){ # 1:nrow(unique(tminlist[[1]]["latlon"]))
        dfplainsite <- subset(dfplain, latlon==unique(tminlist[[1]]["latlon"])[j,])
        dfdetsite <- subset(dfdet, latlon==unique(tminlist[[1]]["latlon"])[j,])
        points(x=dfplainsite$tempC, y=dfdetsite$tempC, col=colz[j])
        abline(lm(dfdetsite$tempC~dfplainsite$tempC), col=colz[j])
     }
}
} 

## PDFs by month and site

if(FALSE){ # checking my code with some quick comparisons 
testpdffx <- subset(tminlist[[8]], latlon==sitez[2,])
testpdffxtime1 <- subset(testpdffx, year>=1951 & year <=1970)
testpdffxtime2 <- subset(testpdffx, year>=1981 & year <=2000)
testpdffxtime3 <- subset(testpdffx, year>=2001 & year <=2020)
plot(density(testpdffxtime1$tempC), type="n")
lines(density(testpdffxtime1$tempC))
lines(density(testpdffxtime2$tempC), col="orange")
lines(density(testpdffxtime3$tempC), col="red")
}

plotPDFsbymonthsite(tminlistdet, "tmindet", sitez, "min C detrended")
plotPDFsbymonthsite(tminlist, "tmin", sitez, "min C")

plotPDFsbymonthsite(tmaxlistdet, "tmaxdet", sitez, "max C detrended")
plotPDFsbymonthsite(tmaxlist, "tmax", sitez, "max C")

## Look at SD and mean over time ...
yearschange <- data.frame(startyear=seq(from=1950, to=2000, by=10), endyear=seq(from=1970, to=2020, by=10))

changeztmin <- list()
for(i in c(1:nrow(yearschange))){
    meanzhere <- getmeansdbysitemonth(tminlist, unique(tminlist[[1]]["latlon"]), yearschange$startyear[i], yearschange$endyear[i])
    changeztmin[[i]] <- data.frame(meanzhere, year=yearschange$startyear[i])
}
changeztmindf <- do.call("rbind", changeztmin)

changeztmax <- list()
for(i in c(1:nrow(yearschange))){
    meanzhere <- getmeansdbysitemonth(tmaxlist, unique(tmaxlist[[1]]["latlon"]), yearschange$startyear[i], yearschange$endyear[i])
    changeztmax[[i]] <- data.frame(meanzhere, year=yearschange$startyear[i])
}
changeztmaxdf <- do.call("rbind", changeztmax)

meantminplotovetime <- ggplot(changeztmindf, aes(y=mean, x=year, color=latlon)) +
    geom_line() + 
    facet_wrap(month~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

sdtminplotovetime <- ggplot(changeztmindf, aes(y=sd, x=year, color=latlon)) +
    geom_line() + 
    facet_wrap(month~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

meantmaxplotovetime <- ggplot(changeztmaxdf, aes(y=mean, x=year, color=latlon)) +
    geom_line() + 
    facet_wrap(month~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

sdtmaxplotovetime <- ggplot(changeztmaxdf, aes(y=sd, x=year, color=latlon)) +
    geom_line() + 
    facet_wrap(month~., scales="free") +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave(filename="graphs/historicaltrendstminmean.pdf", plot=meantminplotovetime, height=8, width=12)
ggsave(filename="graphs/historicaltrendstminsd.pdf", plot=sdtminplotovetime, height=8, width=12)
ggsave(filename="graphs/historicaltrendstmaxmean.pdf", plot=meantmaxplotovetime, height=8, width=12)
ggsave(filename="graphs/historicaltrendstmaxsd.pdf", plot=sdtmaxplotovetime, height=8, width=12)

if(FALSE){ ## Test some code at some early point ...
library(tidyr)
colnameshere <- c("lat", "lon", as.character(1:365))
names(justone) <- colnameshere

testing  <- gather(justone, day, value,
           -lat, -lon)

# now I need month
# so I use DOY to get a sequence of dates
nhemi <- c(1:365)
nhemidate <- as.Date(paste(nhemi, 1970, sep="-"), format="%j-%Y") # I checked: non-leap year
doylookup <- data.frame(date=nhemidate, month=format(nhemidate, "%b"), doy=as.character(nhemi))
testmerge <- merge(doylookup, testing, by.x="doy", by.y="day")
meantest <- aggregate(testmerge["value"], testmerge[c("lat", "lon", "month")], FUN=mean)

subset(resultzmin, year=="1970" & month=="4")
subset(meantest, month=="Apr")
## Good news, it works. 
}
