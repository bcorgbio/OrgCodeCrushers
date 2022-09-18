#F22 BIOL3140 Project Two
library(tidyverse)
library(ggplot2)
library(features) #assuming packages already installed
pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")
pseed.wide <- pseed%>%
  left_join(speeds,by=c("speed"="vol")) %>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl) %>% 
  group_by(date,frame) %>%
  mutate(amp.sum=sum(amp.bl)) %>% 
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)
#the code above created pseed.wide
exp1 <- pseed.wide %>% 
  filter() %>% 
  print()
f1 <- features(x = exp1$frame, y=exp1$amp.sum*100)
fget(f1)
fish.speed <- pseed.wide %>% 
  ungroup() %>% 
  group_by(fish,bl.s) %>% 
  summarize
fish.speed$num <- seq(1:12)
fish.speed
