library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggthemes)

airtempmarch <- read_csv("NEON_temp-air-triple/NEON.D02.SERC.DP1.00003.001.2022-03.basic.20230127T120753Z.RELEASE-2023/NEON.D02.SERC.DP1.00003.001.000.060.030.TAAT_30min.2022-03.basic.20221210T164653Z.csv")

datetime <- airtempmarch$endDateTime
airtempmarchclean <- airtempmarch %>%
  select(endDateTime,tempTripleMean,tempTripleMinimum,tempTripleMaximum) %>%
  mutate(year = as.numeric(format(datetime, format ="%Y"))) %>%
  mutate(doy = yday(endDateTime))

hourly <- airtempmarchclean %>%
  filter(row_number() %% 2 == 0) %>%
  filter(between(doy,60,63))



hourlyplot <- hourly %>%
  ggplot(aes(x=endDateTime,y=tempTripleMean)) +
  geom_line(linewidth = 0.75)+
  geom_abline(slope = 0,
              intercept = 1.4,
              linewidth = 0.25)+
  geom_abline(slope = 0,
              intercept = 2.4,
              linewidth = 0.25)+
  geom_abline(slope = 0,
              intercept = 9.1,
              linewidth = 0.25)+
  labs(x = "Time",
       y = "Mean Temperature (degC)")+
  theme_clean()+
  theme(axis.title = element_text(size = 12,face = "bold"))
hourlyplot  

