## Started 21 mars 2023 ##
## Whoo-hoo, spring! Time to finally get some analyses going on Phenofit project ##

## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## packages
library(data.table)
library(ggplot2)

## set working directory
setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses")

## To do!
## START HERE !!!!
# Some quick calculations to check my code

justone <- fread("input/ERA5LAND/ERA5LAND_glo_1970_dly.fit")

yrz <- c(1970:2000)
filelist <-paste("input/ERA5LAND/ERA5LAND_tmn_", yrz, "_dly.fit", sep="")

resultzmin <- data.frame()

# loop through each file in the list
for (i in c(1:length(filelist))) {
  # read in the data file
    data <- fread(filelist[i])
    data <- as.matrix(data)
    # calculate the mean and variance by month for each site
  for (j in 1:nrow(data)) {
    # get the latitude and longitude for the site
      lat <- data[j,1]
      lon <- data[j,2]
      datasm <- data[,-c(1:2)]
     # loop through each month
      for (month in 1:12) {
        # calculate the mean and variance for the month (doing some gymnastics to make chatGPT's idea work)
        monthsdays  <- which(format(as.Date(paste0(yrz[i], "-01-01"), "%Y-%m-%d") + (0:364), "%m") == sprintf("%02d", month))
        monthstartday <- monthsdays[1]
        monthendday <- monthsdays[length(monthsdays)]
        monthdata <- datasm[j, monthstartday:monthendday]
        meanhere <- mean(monthdata, na.rm=TRUE)
        sdhere <- sd(monthdata, na.rm=TRUE)
      # add the results to the data frame
      resultzmin <- rbind(resultzmin, data.frame(lat=lat, lon=lon, month=month, year=yrz[i], mean=meanhere, sd=sdhere))
     }
   }
}

resultzmin$counter <- rep(seq(1:(12*length(yrz))), nrow(justone))

ggplot(resultzmin, aes(x=counter, y=mean, color=lat)) +
    geom_line() 

ggplot(resultzmin, aes(x=year, y=mean, group=lat, color=lat)) +
    geom_line() +
    # geom_smooth(method=lm) +
    facet_wrap(month~.)

ggplot(resultzmin, aes(x=year, y=sd, group=lat, color=lat)) +
    geom_line() +
    # geom_smooth(method=lm) +
    facet_wrap(month~.)

##



## ChatGPT output to get started with ...

# Okay, assume I have read in the files. There is one file per year and in the files, the first two columns are latitude/longitude while each column is a day of year and each row is a different site. Can you please write code that would calculate the mean and variance by calendar month for each site and each year?

# This code assumes that the files are all in the same directory, and that each file is named with the year in the first four characters (e.g., "2022_data.csv").

# The main changes to the code are [my third attempt]:

# We use as.Date() to convert the day of the year to a date object, which allows us to extract the month more easily.
# We use which() and format() to select the columns that correspond to the current month, even when dealing with leap years.
# We use sprintf() to format the month number with leading zeros, to match the format used in the date strings.
# With these changes, the code should correctly handle leap years and produce accurate results for each calendar month.

# set the working directory to where the files are located
setwd("path/to/files")

# get a list of all the data files in the directory
file_list <- list.files()

# create an empty data frame to store the results
results <- data.frame()

# loop through each file in the list
for (file_name in file_list) {
  # read in the data file
  data <- read.csv(file_name, header=TRUE)

  # calculate the mean and variance by month for each site
  for (i in 1:nrow(data)) {
    # get the latitude and longitude for the site
    lat <- data[i,1]
    lon <- data[i,2]

    # loop through each month
    for (month in 1:12) {
      # calculate the mean and variance for the month
      month_data <- data[i, which(format(as.Date(paste0(substr(file_name, 1, 4), "-01-01"), "%Y-%m-%d") + (0:364), "%m") == sprintf("%02d", month))]
      mean_val <- mean(month_data, na.rm=TRUE)
      var_val <- var(month_data, na.rm=TRUE)

      # add the results to the data frame
      results <- rbind(results, data.frame(Lat=lat, Lon=lon, Month=month, Year=substr(file_name, 1, 4), Mean=mean_val, Variance=var_val))
    }
  }
}

# save the results to a CSV file
write.csv(results, "results.csv", row.names=FALSE)
