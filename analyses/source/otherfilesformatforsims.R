## Started 12 June 2023 ###
## By Lizzie ##

## Format the other files we need for phenofit based on the simulated temperature data ##

## Sourced file in calcclimatemetricssims.R

if(FALSE){ # Development code; to delete
## We need to match to these files ...
simsmatch <- fread("output/phenofitsims/ERA5LAND_tmn_1987_dly.fit")
unique(simsmatch$V1)
fakelon <- unique(simsmatch$V2)

## Let's start with just altitude
dalt <- fread("input/ERA5LAND/ERA5LAND_Altitude.fit")
unique(dalt$V1) 
# but udpated later to work with other locations
dalt1site <- dalt[which(dalt$V1==whichsiteotherfiles),]
# Now we repeat it as many different sims (fakelon) as we have and replace the longitude with the fakelon
daltsims <- dalt1site[rep(seq_len(nrow(dalt1site)), times=length(fakelon)),]
daltsims$V2 <- fakelon

# Now write it out!
filetowrite <- file.path(paste0("output/phenofitsims/", "Altitude", ".fit"))
con <- file(filetowrite, open="wt")
writeLines("Repeated climate datafile with sims longitude for Phenofit model", con)
writeLines(paste0("Created in R by ", Sys.getenv("LOGNAME")," on ", Sys.Date()), con)
writeLines("First two columns are latitude and fake longitude", con)
writeLines(" ", con)
close(con)
write.table(daltsims, file = filetowrite, 
   append = TRUE, sep = "\t", col.names = FALSE, row.names = FALSE)
}

# Now, let's write the above as a f(x) 
repfilesnoyears <- function(simsfile, altorwhcfile, writefilename){
	simstomatch <- fread(paste0("output/phenofitsims/ERA5LAND_", simsfile, "_dly.fit", sep=""))
	fakelon <- unique(simstomatch$V2)
	filetorep <- fread(altorwhcfile)
	filetorep1site <- filetorep[which(filetorep$V1==whichsiteotherfiles),]
	filerepped <- filetorep1site[rep(seq_len(nrow(filetorep1site)), times=length(fakelon)),]
	filerepped$V2 <- fakelon
	filetowrite <- file.path(paste0("output/phenofitsims/ERA5LAND_", writefilename, ".fit"))
	con <- file(filetowrite, open="wt")
	writeLines("Repeated climate datafile with sims longitude for Phenofit model", con)
	writeLines(paste0("Created in R by ", Sys.getenv("LOGNAME")," on ", Sys.Date()), con)
	writeLines("First two columns are latitude and fake longitude", con)
	writeLines(" ", con)
	close(con)
	write.table(filerepped, file = filetowrite, 
                append = TRUE, sep = "\t", col.names = FALSE, row.names = FALSE)


}


## Now adapt the f(x) to do the multiple year files
repfileswyears <- function(simsfile, yearvector, climatefile, writefilename){
	simstomatch <- fread(paste0("output/phenofitsims/ERA5LAND_", simsfile, "_dly.fit", sep=""))
	fakelon <- unique(simstomatch$V2)
	for(i in c(1:length(yearvector))){
		whichyear <- yearvector[i]
		filetorep <- fread(paste0("input/ERA5LAND/ERA5LAND_", climatefile, whichyear, "_dly.fit", sep=""))
		filetorep1site <- filetorep[which(filetorep$V1==whichsiteotherfiles),]	
		filerepped <- filetorep1site[rep(seq_len(nrow(filetorep1site)), times=length(fakelon)),]
		filerepped$V2 <- fakelon
		filetowrite <- file.path(paste0("output/phenofitsims/ERA5LAND_", writefilename, whichyear, "_dly.fit"))
		con <- file(filetowrite, open="wt")
		writeLines("Repeated climate datafile with sims longitude for Phenofit model", con)
		writeLines(paste0("Created in R by ", Sys.getenv("LOGNAME")," on ", Sys.Date()), con)
		writeLines("First two columns are latitude and fake longitude", con)
		writeLines(" ", con)
		close(con)
		write.table(filerepped, file = filetowrite, 
                append = TRUE, sep = "\t", col.names = FALSE, row.names = FALSE)
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
	
	
repfilesnoyears("tmx_1987", "input/ERA5LAND/ERA5LAND_Altitude.fit", "Altitude")
repfilesnoyears("tmx_1987", "input/ERA5LAND/ERA5LAND_WHC.fit", "WHC")
yearz <- c(1950:2000) # these are our sims years
repfileswyears("tmx_1987", yearz, "pet_", "pet_")
repfileswyears("tmx_1987", yearz, "pre_", "pre_")
# wait, I need more files for computing PET ...
repfileswyears("tmx_1987", yearz, "dtm_", "dtm_")
repfileswyears("tmx_1987", yearz, "glo_", "glo_")
repfileswyears("tmx_1987", yearz, "wnd_", "wnd_")




