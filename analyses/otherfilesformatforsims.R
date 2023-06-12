## Started 12 June 2023 ###
## By Lizzie ##

## Format the other files we need for phenofit based on the simulated temperature data ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(data.table)


## set working directory
setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses")

if(FALSE){ # Development code; to delete
## We need to match to these files ...
simsmatch <- fread("output/phenofitsims/tmaxsimsyear1987.fit")
unique(simsmatch$V1)
fakelon <- unique(simsmatch$V2)

## Let's start with just altitude
dalt <- fread("input/ERA5LAND/ERA5LAND_Altitude.fit")
unique(dalt$V1) 
# this file has data for many locations, but we only want the 47.5 one
dalt1site <- dalt[which(dalt$V1==47.5),]
# Now we repeat it as many different sims (fakelon) as we have and replace the longitude with the fakelon
daltsims <- dalt1site[rep(seq_len(nrow(dalt1site)), times=length(fakelon)),]
daltsims$V2 <- fakelon

# Now write it out!
filetowrite <- file.path(paste0("output/phenofitsims/", "Altitude", ".fit"))
con <- file(filetowrite, open="wt")
writeLines("Repeated climate datafile with sims longitude for Phenofit model", con)
writeLines(paste0("Created in R by ", Sys.getenv("LOGNAME")," on ", Sys.Date()), con)
writeLines(" ", con)
close(con)
write.table(daltsims, file = filetowrite, 
   append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
}

# Now, let's write the above as a f(x) ... it always uses 47.5 latitude
repfilesnoyears <- function(simsfile, altorwhcfile, writefilename){
	simstomatch <- fread(paste0("output/phenofitsims/", simsfile, ".fit", sep=""))
	fakelon <- unique(simstomatch$V2)
	filetorep <- fread(altorwhcfile)
	filetorep1site <- filetorep[which(filetorep$V1==47.5),]
	filerepped <- filetorep1site[rep(seq_len(nrow(filetorep1site)), times=length(fakelon)),]
	filerepped$V2 <- fakelon
	filetowrite <- file.path(paste0("output/phenofitsims/", writefilename, ".fit"))
	con <- file(filetowrite, open="wt")
	writeLines("Repeated climate datafile with sims longitude for Phenofit model", con)
	writeLines(paste0("Created in R by ", Sys.getenv("LOGNAME")," on ", Sys.Date()), con)
	writeLines(" ", con)
	close(con)
	write.table(filerepped, file = filetowrite, 
                append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)


}


## Now adapt the f(x) to do the multiple year files
## WARNING: We don't seem to have the updated latlon, so using 47.3 for now but UPDATE! after chatting with Victor
repfileswyears <- function(simsfile, yearvector, climatefile, writefilename){
	simstomatch <- fread(paste0("output/phenofitsims/", simsfile, ".fit", sep=""))
	fakelon <- unique(simstomatch$V2)
	for(i in c(1:length(yearvector))){
		whichyear <- yearvector[i]
		filetorep <- fread(paste0("input/ERA5LAND/ERA5LAND_", climatefile, whichyear, "_dly.fit", sep=""))
		filetorep1site <- filetorep[which(filetorep$V1=="47.3"),]	
		filerepped <- filetorep1site[rep(seq_len(nrow(filetorep1site)), times=length(fakelon)),]
		filerepped$V2 <- fakelon
		filetowrite <- file.path(paste0("output/phenofitsims/", writefilename, whichyear, ".fit"))
		con <- file(filetowrite, open="wt")
		writeLines("Repeated climate datafile with sims longitude for Phenofit model", con)
		writeLines(paste0("Created in R by ", Sys.getenv("LOGNAME")," on ", Sys.Date()), con)
		writeLines(" ", con)
		close(con)
		write.table(filerepped, file = filetowrite, 
                append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)
	}
}
	


# just for troubleshooting
if(FALSE){ 
simsfile <- "tmaxsimsyear1987"
yearvector <- yearz
climatefile <- "pet_"
writefilename <- "pet_"
i <- 1
}
	
	
repfilesnoyears("tmaxsimsyear1987", "input/ERA5LAND/ERA5LAND_Altitude.fit", "Altitude")
repfilesnoyears("tmaxsimsyear1987", "input/ERA5LAND/ERA5LAND_WHC.fit", "WHC")
yearz <- c(1950:2020) # these are our sims years
repfileswyears("tmaxsimsyear1987", yearz, "pet_", "pet_")
repfileswyears("tmaxsimsyear1987", yearz, "pre_", "pre_")


