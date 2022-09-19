#F22 BIOL3140 Project Two
#1
library(tidyverse)
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
  mutate(amp.sum=L+R) %>% 
  print() #the code above created pseed.wide
          #print() was included at every "checkpoint" to visualize result

#2
find.peaks <- function(x,y,mult=100){
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks)
}

pseed.peaks <- pseed.wide%>%
  group_by(fish,bl.s)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==TRUE)

pseed.sum.max<- pseed.peaks%>%
  group_by(fish,bl.s)%>%
  summarize(amp.sum.mean=mean(amp.sum)) %>% 
  print()

#3 
sem = function(x){
  sd(x)/sqrt(length(x))
}

pseed.sum.max <- pseed.peaks %>% 
  group_by(fish,bl.s) %>% 
  summarise(amp.sum.se=sem(amp.sum)) %>% 
  left_join(pseed.sum.max, by = c('fish','bl.s')) %>% 
  print()

#4
pseed.sum.max %>% 
  ggplot(aes(x=bl.s,y=amp.sum.mean,col=fish)) + geom_point() + geom_line() +
  geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se,ymax=amp.sum.mean+amp.sum.se))

#5
met.specs <- read_csv('pseed.met.rate.csv')
met.n.amp <- pseed.sum.max %>% 
  left_join(met.specs, by = c('bl.s','fish')) %>% 
  print()

#6
met.n.amp %>% 
  ggplot(aes(x=met.rate,y=amp.sum.mean,col=fish)) + geom_point()
