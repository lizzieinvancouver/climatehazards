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

varchanges <- c(-0.2, 0, 0.2) # c(0, -0.1, 0.1, 0.2)
tempchanges <- c(0) # c(0, 0.5, 1, 2)
allchanges <- expand.grid(varchanges, tempchanges)
names(allchanges) <- c("sd", "mean")
allchanges$fakelon <- seq(1:nrow(allchanges))

whichsite <-  unique(tmaxlist[[1]]["latlon"])[5,] # Pick 47.5 based on 20 Apr 2023 with Isabelle
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

# calculate mean and SD for each month across time series
# take z-scored data then add back in mean and SD based on values
# output is a list of lists!
# 12 lists (per month) and within EACH list is another list ...
# which is the length of the different sims requested 
makesimdata <- function(climatedatadet, startyear, endyear, treatdf){
    resultz <- vector(mode='list', length=12)
    for (i in c(1:12)) {
        dfhereallyears <- climatedatadet[[i]]
        dfhere <- subset(dfhereallyears, year>=startyear & year<=endyear)
        meanhere <- mean(dfhere$tempC)
        sdhere <- sd(dfhere$tempC)
        dfhere$ztempC <- (dfhere$tempC-meanhere)/sdhere
        resultzwithinmonth <- vector(mode='list', length=nrow(treatdf))
        for (j in c(1:nrow(treatdf))){
            newmean <- meanhere*(1+treatdf[["mean"]][j])
            newsd <- sdhere*(1+treatdf[["sd"]][j])
            newdata <- (dfhere$ztempC*newsd)+newmean
            # plot(dfhere$tempC~newdata)
            # abline(0,1)
            newdf <- data.frame(lat=dfhere$lat, lon=dfhere$lon, year=dfhere$year, day=dfhere$day,
                                meanttreat=rep(treatdf[["mean"]][j], length(newdata)),
                                sdtreat=rep(treatdf[["sd"]][j], length(newdata)),
                                fakelon=rep(treatdf[["fakelon"]][j], length(newdata)),
                                tempC=newdata)
            resultzwithinmonth[[j]] <- rbind(resultzwithinmonth[[j]], newdf)
        }
        resultzwithinmonthdf <- do.call("rbind", resultzwithinmonth)
        resultz[[i]] <- rbind(resultz[[i]], resultzwithinmonthdf)
    }
    return(resultz)
}

tminsims <- makesimdata(onesitetmin, 1970, 2000, allchanges)
tmaxsims <- makesimdata(onesitetmax, 1970, 2000, allchanges)


if(FALSE){ 
# For testing/troubleshooting code above and below
climatedatadet <-  onesitetmin
startyear <- 1970
endyear <- 2000
treatdf <- allchanges
i <- 2
j <- 1
filename <- "tmin3sd"
simdata <- tminsims
simshere <- c("-20%", "sd unchanged", "sd + 20%")
}

# Functions to plot simulated data
plotsimdataPDF <- function(climatedatadet, simdata, startyear, endyear, filename, simshere){
    colz <- viridis(length(simshere))
    pdf(paste("graphs/simdataPDF", filename, ".pdf", sep=""), width=14, height=10)
    par(mfrow=c(3, 4))
    for (i in c(1:12)) {
        dfhereallyears <- climatedatadet[[i]]
        dfhere <- subset(dfhereallyears, year>=startyear & year<=endyear)
        simlist <- as.data.frame(simdata[[i]])
        fakelonlist <- unique(simlist["fakelon"])
        plot(density(dfhere[["tempC"]]), type="n", xlab="Temp C", ylab="Density",
             main=paste("month", i, sep=" "))
        legend("topleft", legend=simshere, pch=rep(16, length(simshere)), col=colz, bty="n")
        for(j in c(1:nrow(fakelonlist))){
            simdatahere <- simlist[which(simlist["fakelon"]==fakelonlist[j,]),]
            lines(density(simdatahere[["tempC"]]), col=colz[j], lwd=2)
        }
    }
    dev.off()
}


plotsimdata <- function(climatedatadet, simdata, startyear, endyear, filename, simshere){
    colz <- viridis(length(simshere))
    pdf(paste("graphs/simdata", filename, ".pdf", sep=""), width=14, height=10)
    par(mfrow=c(3, 4))
    for (i in c(1:12)) {
        dfhereallyears <- climatedatadet[[i]]
        dfhere <- subset(dfhereallyears, year>=startyear & year<=endyear)
        simlist <- as.data.frame(simdata[[i]])
        fakelonlist <- unique(simlist["fakelon"])
        plot(dfhere[["tempC"]], dfhere[["tempC"]], type="n", xlab="Original data", ylab="Sim data",
             main=paste("month", i, sep=" "))
        abline(0, 1, col="black")
        legend("topleft", legend=simshere, pch=rep(16, length(simshere)), col=colz, bty="n")
        for(j in c(1:nrow(fakelonlist))){
            simdatahere <- simlist[which(simlist["fakelon"]==fakelonlist[j,]),]
            points(simdatahere[["tempC"]]~dfhere[["tempC"]], col=colz[j])
            abline(lm(simdatahere[["tempC"]]~dfhere[["tempC"]]), col=colz[j])
        }
    }
    dev.off()
}

simshere <- c("sd -20%", "sd 0%", "sd + 20%")
plotsimdata(onesitetmin, tminsims, 1970, 2000, "tmin3sd", simshere)
plotsimdataPDF(onesitetmin, tminsims, 1970, 2000, "tmin3sd", simshere)



#################################
## Write out Phenofit sim data ##
#################################
# need tmin and tmax sims (eventually also PET sims) and then tmean from tmin/tmax
# 1 file per year
# each row is a 'site'
# Formatted with some information on top lines, as Victor does (see write_phenofit_data.R)
# and then need to rep all the other files we need with new lat/lon

# Some testing code that I am not deleting yet
if(FALSE){  
# example file
test <- fread("~/Documents/git/projects/treegarden/misc/climatehazards/data/recdVictor_2023Avr7/ERA5LAND/ERA5LAND_tmx_2006_dly.fit")
testoneyear <- subset(tminsims[[1]], year==1980)

# testing out lapply subset (me and lapply, always a work in progress)
whichyear <- unique(tminsims[[1]]["year"])[1,]
tryme <- lapply(tminsims, function(x) subset(x, year == whichyear))
test1970 <- do.call("rbind", tryme)

# Build one file for one year ... 
for (i in c(1:nrow(allchanges))){
    thistreat <- subset(test1970, fakelon==allchanges[["fakelon"]][i])
    onerowhere <- c(thistreat[["lat"]][1], thistreat[["fakelon"]][1], 
        thistreat[["tempC"]])
    plot(onerowhere[3:length(onerowhere)]~c(1:(length(onerowhere)-2)), type="l")
    onerowheredf <- as.data.frame(matrix(onerowhere, ncol = length(onerowhere)))
    write.table(onerowheredf, file = "output/phenofitsims/tminsimsyear1970.csv", 
        append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)

# Now do it across years ... 
for (i in c(1:length(listofyearshere))) {
    whichyear <- listofyearshere[i]
    tryme <- lapply(tminsims, function(x) subset(x, year == whichyear))
    trymedf <- do.call("rbind", tryme)
    for (treathere in c(1:nrow(allchanges))){
        thistreat <- subset(trymedf, fakelon==allchanges[["fakelon"]][treathere])
        onerowhere <- c(thistreat[["lat"]][1], thistreat[["fakelon"]][1], 
            thistreat[["tempC"]])
        plot(onerowhere[3:length(onerowhere)]~c(1:(length(onerowhere)-2)), type="l")
        onerowheredf <- as.data.frame(matrix(onerowhere, ncol = length(onerowhere)))
        write.table(onerowheredf, file = paste0("output/phenofitsims/tmn_", listofyearshere[i], ".csv"), 
            append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
        }
    }   
}    
}

    
# Now functionalize it ... 
writeoutdata <- function(listofyears, simdata, filenamestart){
    for (i in c(1:length(listofyears))) {
        whichyear <- listofyears[i]
        tryme <- lapply(simdata, function(x) subset(x, year == whichyear))
        trymedf <- do.call("rbind", tryme)
        filetowrite <- file.path(paste0("output/phenofitsims/ERA5LAND_", filenamestart, listofyears[i], "_dly.fit"))
        con <- file(filetowrite, open="wt")
        writeLines("Simulated climate datafile for Phenofit model", con)
        writeLines(paste0("Created in R by ", Sys.getenv("LOGNAME")," on ", Sys.Date()), con)
        writeLines(" ", con)
        close(con)
        for (treathere in c(1:nrow(allchanges))){
            thistreat <- subset(trymedf, fakelon==allchanges[["fakelon"]][treathere])
            onerowhere <- c(thistreat[["lat"]][1], thistreat[["fakelon"]][1], 
                thistreat[["tempC"]])
            plot(onerowhere[3:length(onerowhere)]~c(1:(length(onerowhere)-2)), type="l")
            onerowheredf <- as.data.frame(matrix(onerowhere, ncol = length(onerowhere)))
            write.table(onerowheredf, file = filetowrite, 
                append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
        }
    }
}     

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
