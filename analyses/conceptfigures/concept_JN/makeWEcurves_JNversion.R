## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## libraries
library(ggplot2)

## set working directory

getwd()

# if(length(grep("lizzie", getwd()))>0) { 
  # setwd("~/Documents/git/projects/treegarden/misc/climatehazards/analyses/conceptfigures/")
# } else setwd("/Users/Lizzie/Documents/git/projects/vinmisc/vassalphen/analyses/wangengel/")

setwd("C:/Users/sapph/Documents/ubc things/work/climatehazards/analyses/conceptfigures")

source("Script_functions_pheno_models.R")

testclim.avg <- seq(-5,42, length.out=200)

# Nacho has an alpha f(x) but I am not sure when to use it
#testalpha <- Alphafx(testclim.min, testclim.max, 29) #do we need Topt first?

# Wang & Engel model seems to want static values for first 4 inputs:
# WangEngelfx <- function(Tmin, Tmax, Topt, Alpha, Tavg)
wangeng24 <- WangEngelfx(0, 41, 26, 2.85, testclim.avg) # Nacho's budburst (with alpha from Inakis other models)
wangeng27 <- WangEngelfx(0, 41, 29, 2.85, testclim.avg) # Nacho's flowering
wangeng26 <- WangEngelfx(0, 41, 22, 2.85, testclim.avg) # Nacho's veraison
#should all Tmax be 40 that's what Inaki and Nacho use - NOPE, graph gets weird

# Now just format and plot
wangeng24clim <- data.frame(we=wangeng24[,1], tempC=testclim.avg,
   temp=testclim.avg)
wangeng27clim <- data.frame(we=wangeng27[,1], tempC=testclim.avg,
   temp=testclim.avg)
wangeng26clim <- data.frame(we=wangeng26[,1], tempC=testclim.avg,
                            temp=testclim.avg)

wangeng24clim.sm <- subset(wangeng24clim, we>=0)
wangeng27clim.sm <- subset(wangeng27clim, we>=0)
wangeng26clim.sm <- subset(wangeng26clim, we>=0)

# Some simple curves 
plot(we~temp, data=wangeng27clim.sm, type="l", xlim=c(0,40))
points(we~temp, data=wangeng24clim.sm, type="l", col="red")
points(we~temp, data=wangeng26clim.sm, type="l", col="blue")

## As above, but in a pdf! 
pdf(file.path("graphs/wengengsimple.pdf"), width = 8, height = 7)
plot(we~tempC, data=wangeng27clim.sm,  xlim=c(-5,42), ylab="Developmental rate",
     xlab=expression(paste("Temperature "( degree~C))), type="n")
points(we~tempC, data=wangeng27clim.sm, type="l", lwd=2, col="gold")
points(we~tempC, data=wangeng24clim.sm, type="l", lwd=2, col="dodgerblue")
points(we~tempC, data=wangeng26clim.sm, type="l", lwd=2, col="darkorchid4")
# dev.off()

# making a combined plot with ggplot2
library(tidyverse)
library(ggthemes)

wangeng24clim.a <- wangeng24clim.sm %>%
  mutate(category = "a")
wangeng27clim.b <- wangeng27clim.sm %>%
  mutate(category = "b")
wangeng26clim.c <- wangeng26clim.sm %>%
  mutate(category = "c")

wangeng.comb <- rbind(wangeng24clim.a,wangeng27clim.b,wangeng26clim.c)

wangeng_plot <- wangeng.comb %>%
  ggplot(aes(x = tempC,
             y = we,
             colour = category)) +
  geom_line(linewidth = 1) +
  labs(x = "Temperature (°C)",
       y = "Developmental Rate") +
  theme_few() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_colour_manual(values = c("a" = "black",
                                 "b" = "red",
                                 "c" = "blue")) +
  geom_vline(xintercept = 4, linewidth = 1,colour = "black", linetype = "twodash") +
  geom_vline(xintercept = 9, linewidth = 1,colour = "red", linetype = "twodash") +
  geom_vline(xintercept = 13, linewidth = 1,colour = "blue", linetype = "twodash")
wangeng_plot



# weird bends, from Nacho:
# Tmin = 0
# Tmax = 41
# 
# test = WangEngelfx(0,41,26,2.85,seq(5,45,0.2))
# plot(seq(5,45,0.2),test[,1],ylim=c(0,1),xlim=c(Tmin,Tmax),type="l")
# 
# test = WangEngelfx(0,41,29,2.85,seq(5,45,0.2))
# points(seq(5,45,0.2),test[,1],ylim=c(0,1),type="l",col="red")
# 
# test = WangEngelfx(0,41,22,2.85,seq(5,45,0.2))
# points(seq(5,45,0.2),test[,1],ylim=c(0,1),type="l",col="blue")

# Justin's edit
we.budburst <- WangEngelfx(0, 41, 26, 2.85, seq(5,45,0.2))
we.flowering <- WangEngelfx(0, 41, 29, 2.85, seq(5,45,0.2))
we.veraison <- WangEngelfx(0, 41, 22, 2.85, seq(5,45,0.2))

testclim.jn <- seq(-5,42, length.out=201)

we.budburst <- as.data.frame(we.budburst) %>%
  mutate(tempC = testclim.jn) %>%
  mutate(category = "a") %>%
  subset(Temp.action >= 0)
we.flowering <- as.data.frame(we.flowering) %>%
  mutate(tempC = testclim.jn) %>%
  mutate(category = "b") %>%
  subset(Temp.action >= 0)
we.veraison <- as.data.frame(we.veraison) %>%
  mutate(tempC = testclim.jn) %>%
  mutate(category = "c") %>%
  subset(Temp.action >= 0)

we.combined <- rbind(we.budburst,we.flowering,we.veraison)

we_comb_plot <- we.combined %>%
  ggplot(aes(x = tempC,
             y = Temp.action,
             colour = category)) +
  geom_line(linewidth = 1) +
  labs(x = "Temperature (°C)",
       y = "Developmental Rate") +
  theme_few() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_colour_manual(values = c("a" = "black",
                                 "b" = "red",
                                 "c" = "blue")) +
  geom_vline(xintercept = 4, linewidth = 1,colour = "black", linetype = "twodash") +
  geom_vline(xintercept = 9, linewidth = 1,colour = "red", linetype = "twodash") +
  geom_vline(xintercept = 13, linewidth = 1,colour = "blue", linetype = "twodash")
we_comb_plot



# still has a weird bend
# Nacho's new code has xlim and ylim specified, maybe that works?

Tmin = 0
Tmax = 41

wangeng_plot <- wangeng.comb %>%
  ggplot(aes(x = tempC,
             y = we,
             colour = category)) +
  geom_line(linewidth = 1) +
  labs(x = "Temperature (°C)",
       y = "Developmental Rate") +
  theme_few() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_colour_manual(values = c("a" = "black",
                                 "b" = "red",
                                 "c" = "blue")) +
  geom_vline(xintercept = 4, linewidth = 1,colour = "black", linetype = "twodash") +
  geom_vline(xintercept = 9, linewidth = 1,colour = "red", linetype = "twodash") +
  geom_vline(xintercept = 13, linewidth = 1,colour = "blue", linetype = "twodash") +
  xlim(Tmin,Tmax)+
  ylim(0,1)
wangeng_plot
# the weird uneven endings means xlim() caps off one final point that all three lines converge to, that is greater than Tmax= 41.
# Looking at wangeng.comb we see that the highest TempC value is 42, which is probably the culprit:

wangeng_plot <- wangeng.comb %>%
  ggplot(aes(x = tempC,
             y = we,
             colour = category)) +
  geom_line(linewidth = 1) +
  labs(x = "Temperature (°C)",
       y = "Developmental Rate") +
  theme_few() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_colour_manual(values = c("a" = "black",
                                 "b" = "red",
                                 "c" = "blue")) +
  geom_vline(xintercept = 4, linewidth = 1,colour = "black", linetype = "twodash") +
  geom_vline(xintercept = 9, linewidth = 1,colour = "red", linetype = "twodash") +
  geom_vline(xintercept = 13, linewidth = 1,colour = "blue", linetype = "twodash") +
  xlim(Tmin,42)+
  ylim(0,1)
wangeng_plot #confirmed

# Now finding out what causes the value of 42 and how to make those lines go straight down instead?
# seems like testclim.avg or testclim.jn stop at 42, when it should stop at 45?

# testclim.new <- seq(-5,45, length.out=201)
# 
# we.budburst.n <- WangEngelfx(0, 41, 26, 2.85, testclim.new)
# we.flowering.n <- WangEngelfx(0, 41, 29, 2.85, testclim.new)
# we.veraison.n <- WangEngelfx(0, 41, 22, 2.85, testclim.new)
# 
# we.budburst.df <- data.frame(we=we.budburst.n[,1], tempC=testclim.new,
#                             temp=testclim.new)
# we.flowering.df <- data.frame(we=we.flowering.n[,1], tempC=testclim.new,
#                             temp=testclim.new)
# we.veraison.df <- data.frame(we=we.veraison.n[,1], tempC=testclim.new,
#                             temp=testclim.new)
# 
# we.budburst.ss <- subset(we.budburst.df, we>=0) %>%
#   mutate(category="a")
# we.flowering.ss <- subset(we.flowering.df, we>=0) %>%
#   mutate(category="b")
# we.veraison.ss <- subset(we.veraison.df, we>=0) %>%
#   mutate(category="c")
# 
# we.comb.new <- rbind(we.budburst.ss,we.flowering.ss,we.veraison.ss)
# 
# we.comb.new_plot <- we.comb.new %>%
#   ggplot(aes(x = tempC,
#              y = we,
#              colour = category)) +
#   geom_line(linewidth = 1) +
#   labs(x = "Temperature (°C)",
#        y = "Developmental Rate") +
#   theme_few() +
#   theme(axis.title = element_text(size = 12,face = "bold",),
#         legend.title = element_text(size = 12, face = "bold"),
#         legend.text = element_text(size = 10)) +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank()) +
#   scale_colour_manual(values = c("a" = "black",
#                                  "b" = "red",
#                                  "c" = "blue")) +
#   geom_vline(xintercept = 4, linewidth = 1,colour = "black", linetype = "twodash") +
#   geom_vline(xintercept = 9, linewidth = 1,colour = "red", linetype = "twodash") +
#   geom_vline(xintercept = 13, linewidth = 1,colour = "blue", linetype = "twodash") +
#   xlim(Tmin,41)+
#   ylim(0,1)
# we.comb.new_plot

# It's even worse than before
# So far this is the best version

we_temporary_plot <- wangeng.comb %>%
  ggplot(aes(x = tempC,
             y = we,
             colour = category)) +
  geom_line(linewidth = 1) +
  labs(x = "Temperature (°C)",
       y = "Developmental Rate") +
  theme_few() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_colour_manual(values = c("a" = "black",
                                 "b" = "red",
                                 "c" = "blue")) +
  geom_vline(xintercept = 4, linewidth = 1,colour = "black", linetype = "twodash") +
  geom_vline(xintercept = 9, linewidth = 1,colour = "red", linetype = "twodash") +
  geom_vline(xintercept = 13, linewidth = 1,colour = "blue", linetype = "twodash") +
  xlim(Tmin,Tmax)+
  ylim(0,1)
we_temporary_plot
ggsave("concept_JN/plots/we_temporary_plot.png",plot=we_temporary_plot,scale=1,dpi=600)
