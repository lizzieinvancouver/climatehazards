library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggthemes)

airtempmarch <- read_csv("NEON_temp-air-triple/NEON_SERC_marchdata/SERC_30min.csv")

datetime <- airtempmarch$endDateTime
airtempmarchclean <- airtempmarch %>%
  select(endDateTime,tempTripleMean,tempTripleMinimum,tempTripleMaximum) %>%
  mutate(year = as.numeric(format(datetime, format ="%Y"))) %>%
  mutate(doy = yday(endDateTime))

hourly <- airtempmarchclean %>%
  filter(row_number() %% 2 == 0) %>%
  filter(between(doy,60,63))%>%
  mutate(experiment_data = 4)


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
  ylim(-3,12)+
  labs(x = "Time",
       y = "Mean Temperature (degC)")+
  theme_clean()+
  theme(axis.title = element_text(size = 12,face = "bold"))
hourlyplot
# ggsave("plots/chillingconcept.png",plot=hourlyplot,scale=1,dpi=600)

experimentplot <- hourly %>%
  ggplot(aes(x=endDateTime,y=experiment_data)) +
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
  ylim(-3,12)+
  labs(x = "Time",
       y = "Mean Temperature (degC)")+
  theme_clean()+
  theme(axis.title = element_text(size = 12,face = "bold"))
experimentplot
# ggsave("plots/experimentconcept.png",plot=experimentplot,scale=1,dpi=600)
