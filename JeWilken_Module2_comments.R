# Module 2 - Jenna Wilken

library(tidyverse)
library(ggplot2)
library(features)

pseed <- read_csv("pseed.fin.amps.csv")
pseed.bl <- read_csv("pseed.lengths.csv")
speeds <- read_csv("pseed.calibration.csv")

pseed2 <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  print()
pseed.bl%>%
  print()
pseed2%>%
  select(fish) %>%
  unique()
pseed2 <- pseed2%>%
  left_join(pseed.bl,by="fish")%>%
  print()
pseed2 <- pseed2%>%
  mutate(bl.s=cm.s/bl)%>%
  print()
pseed2 %>%
  filter(fin=="R")
pseed.wide <- pseed2 %>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

#CPK: This ^^ could have been done more concisely. . . .

pseed.wide <- pseed%>%
  left_join(speeds,by=c("speed"="vol"))%>%
  left_join(pseed.bl,by="fish")%>%
  mutate(bl.s=cm.s/bl)%>%
  select(-amp)%>%
  pivot_wider(names_from = fin,values_from = amp.bl) %>%
  mutate(amp.sum=L+R)%>%
  print() 

find.peaks <- function(x,y,mult=100){ 
  f <- fget(features(x = x,y=y*mult))[2:3]%>% 
    as_tibble()%>% 
    filter(curvature<0)%>% 
    mutate(peaks=round(crit.pts,0))
  return(f$peaks) 
}

pseed.max <- pseed.wide%>%
  group_by(fish,speed)%>%
  mutate(peak=frame %in% find.peaks(frame,amp.sum))%>%
  filter(peak==T)
pseed.max$peak <- NULL

#CPK: But for this ^^ we needed to group by date bc cycles refer to each experiment, not across them. [-1]

pseed.sum.max<- pseed.max %>%
  group_by(fish, speed) %>%
  summarize(amp.sum.mean=mean(amp.sum)) 

#3 Create a custom function that computes the standard error of the mean (SE). 
 SD <- function(x){
 sd(x)/ sqrt(length(x))
}

pseed.sum.se <- pseed.max%>%
  group_by(fish,speed)%>%
  summarize(amp.sum.se = SE(amp.sum))
pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.sum.se, by = c("speed","fish"))

#CPK: No need to do the SEM calculation separately. [-1] You could have define the se function earlier and computed se at the same time as mean, a la.. . 

pseed.sum.max<- pseed.max %>%
  group_by(fish, speed) %>%
  summarize(amp.sum.mean=mean(amp.sum),amp.sum.se = se(amp.sum))

#4 "The Good Stuff" Using ggplot, plot the mean amp.sum vs. specific swimming speed and add error bars that correspond to the SE of amp.sum. 
pd <- position_dodge(0.1)
pseed.sum.max %>%
  ggplot(aes(x=speed,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=0.5, colour="black", position=pd)+theme_classic()

#CPK: Why not color the error bars by fish, too?


#5 Merge with new pseed.sum.max tibble.
pseed.met.rate <- read_csv("OrgCodeCrushers/pseed.met.rate.csv")
pseed.max <- pseed.max%>%
  merge(pseed.met.rate,by=c("fish","date","m.s","cm.s","bl.s"))
pseed.mean.rate <- pseed.max %>%
  group_by(fish, speed)%>%
  summarize(amp.met.rate=mean(met.rate))
pseed.sum.max <- pseed.sum.max %>%
  left_join(pseed.mean.rate, by = c("speed","fish"))

#CPK: ^^ No need to specify "by" when common columns exist. And this could have been more concisely written ....

pseed.sum.max%>%
  left_join(pseed.met.rate%>%
              group_by(fish, bl.s)%>%
              summarize(amp.met.rate=mean(met.rate))
  )


#6 Use ggplot to plot the metabolic power output of each fish vs. mean maximum of amp.sum.
pseed.sum.max %>%
  ggplot(aes(x=amp.met.rate,y=amp.sum.mean,col=fish))+geom_point()+geom_smooth(method="lm")+geom_errorbar(aes(ymin=amp.sum.mean-amp.sum.se, ymax=amp.sum.mean+amp.sum.se), width=0.05, colour="black", position=pd)+theme_classic()

#CPK: Great work, just need to be a little more concise.

