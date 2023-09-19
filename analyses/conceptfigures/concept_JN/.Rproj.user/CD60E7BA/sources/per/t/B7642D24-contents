library(tidyverse)
library(ggplot2)
library(ggthemes)

# can I just fabricate it with an asymptotic function?
cold <- data.frame(xval = c(1:100))
cold <- cold %>%
  mutate(yval = 1/(500000*xval))

# proof of concept
cold %>%
  ggplot(aes(x=xval,y=yval)) +
  geom_line()

# concept visualizaton
cold_plot <- cold %>%
  ggplot(aes(x = xval,
             y = yval)) +
  geom_line(linewidth = 1) +
  # geom_point(aes(xval = 5,size=2))+
  labs(x = "Accumulated cold temperatures",
       y = "Cold hardiness (°C)") +
  theme_few() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10)) +
  theme(axis.text.y = element_text(colour="white"),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour="white"),
        axis.ticks.x = element_blank()) +
  xlim(1,18) +
  ylim(0,1e-06)
  
cold_plot  
ggsave("plots/coldhazards_concept.png",plot=cold_plot,scale=1,dpi=600,units = "in",height=7,width=10.9)

