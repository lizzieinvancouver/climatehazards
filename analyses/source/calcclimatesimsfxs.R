## Started 21 June 2023 ##
## By Lizzie ## 

## Happy summer! ##
## Finally moving over f(x)s from calcclimatemetricssims.R related to simulated data ##


##############################
## Create Phenofit sim data ##
##############################
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
            newmean <- meanhere+(1+treatdf[["mean"]][j])
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


if(FALSE){ 
# For testing/troubleshooting code above and below
climatedatadet <-  onesitetmin
startyear <- 1950
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
    pdf(paste("graphs/phenofit/sims/climate/simdataPDF", filename, ".pdf", sep=""), width=14, height=10)
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
    pdf(paste("graphs/phenofit/sims/climate/simdata", filename, ".pdf", sep=""), width=14, height=10)
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
        append = TRUE, sep = "\t", col.names = FALSE, row.names = FALSE)

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
            append = TRUE, sep = "\t", col.names = FALSE, row.names = FALSE)
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
        writeLines("First two columns are latitude and fake longitude", con)
        writeLines(" ", con)
        close(con)
        for (treathere in c(1:nrow(allchanges))){
            thistreat <- subset(trymedf, fakelon==allchanges[["fakelon"]][treathere])
            onerowhere <- c(thistreat[["lat"]][1], thistreat[["fakelon"]][1], 
                thistreat[["tempC"]])
            plot(onerowhere[3:length(onerowhere)]~c(1:(length(onerowhere)-2)), type="l")
            onerowheredf <- as.data.frame(matrix(onerowhere, ncol = length(onerowhere)))
            write.table(onerowheredf, file = filetowrite, 
                append = TRUE, sep = "\t", col.names = FALSE, row.names = FALSE)
        }
    }
} 