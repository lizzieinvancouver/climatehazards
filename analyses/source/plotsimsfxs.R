## Started 20 June 2023 ##
## By Lizzie ##


cleanphenofitdata <- function(phenofitfilelist, filename, sitedf){
    df <- list()
    for (phenometric in phenofitfilelist){     
        d <- read.delim(paste("input/phenofit/", filename, phenometric, ".txt", sep=""),
                    skip=3, header=FALSE)
        dfhere <- data.frame(metric=character(), lat=numeric(), lon=numeric(), year=numeric(),
                             year=numeric(), value=numeric())
        for(i in c(1:nrow(sitedf))){
            dftobind <- data.frame(metric=rep(phenometric, nrow(d)),
                           lat=rep(sitedf[["lat"]][i], nrow(d)),
                           lon=rep(sitedf[["lon"]][i], nrow(d)),
                           year=as.numeric(unlist(d["V1"])),
                           value=as.numeric(unlist(d[,i+1])))
            dfhere <- rbind(dfhere, dftobind)
        }
       df[[phenometric]] <- dfhere
        }
   return(df)
}

if(FALSE){
	simsdf <- fsfitsims
	treatfile <- treatz
}

getdiffsims <- function(simsdf, treatfile){
    fitsimsdiff <- list()
for (whichlist in c(1:length(simsdf))){ # whichlist <- 1
    listhere <- simsdf[[whichlist]]
    controlrun <- treatfile[which(treatfile$mean==0 & treatfile$sd==0), 3] 
    controlsim <- subset(listhere, lon==controlrun)
    fitsimsdiff[[whichlist]] <- controlsim[1,] # cheap!
    for(uniquetreat in c(1:nrow(treatfile))){
        subby <- subset(listhere, lon==treatfile$fakelon[uniquetreat])
        newdf <- subby
        newdf$value <- NULL
        newdf$value <- subby$value-controlsim$value
        fitsimsdiff[[whichlist]] <- rbind(fitsimsdiff[[whichlist]], newdf)
    }
    fitsimsdiff[[whichlist]] <- fitsimsdiff[[whichlist]][-1,]
    fitsimsdiff[[whichlist]] <- merge(fitsimsdiff[[whichlist]], treatz, by.x="lon", by.y="fakelon")
}
return(fitsimsdiff)
}


countbadyrs <- function(simsdf, treatfile){
    countyrs <- data.frame(metric=character(), lat=numeric(), lon=numeric(), n=numeric())
    for (whichlist in c(1:length(simsdf))){ 
        listhere <- simsdf[[whichlist]]
        if(length((grep("Date", names(fsfitsims)[whichlist])>0))){
            for(uniquetreat in c(1:nrow(treatfile))){
                subby <- subset(listhere, lon==treatfile$fakelon[uniquetreat])
                subbybad <- subset(subby, value>365)
                newrow <- data.frame(metric=subby$metric[1],
                    lat=subby$lat[1],
                    lon=subby$lon[1],
                    n=nrow(subbybad))
                countyrs <- rbind(countyrs, newrow)

            }
        } else {
            for(uniquetreat in c(1:nrow(treatfile))){
                subby <- subset(listhere, lon==treatfile$fakelon[uniquetreat])
                subbybad <- subset(subby, value==0)
                newrow <- data.frame(metric=subby$metric[1],
                    lat=subby$lat[1],
                    lon=subby$lon[1],
                    n=nrow(subbybad))
                countyrs <- rbind(countyrs, newrow)
            }
        }
    }
    countyrs <- merge(countyrs, treatz, by.x="lon", by.y="fakelon")
    return(countyrs)
}