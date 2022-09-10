#Project 1 - Tingwei (Jerry) Hu - Team 8
library(ggplot2)
library(tidyverse) #assuming the two packages were already installed
setwd("~/Desktop/BC/BIOl3140 Projects/Scales/OrgCodeCrushers")
dat <- read.csv("scales.csv")
sapply(dat,class)
dim(dat)
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
dat %>%
  group_by(species) %>%
  summarise(n = n())
dat %>% 
  count(species,specimen) %>%
  count(species,name = "n.specimens")
pdf("species.quadrant.TingweiHu.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()