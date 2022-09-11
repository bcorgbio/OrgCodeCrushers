library(ggplot2)
library(tidyverse)
setwd("C:/Users/chris/Downloads/scales")
dat <- read.csv("scales.csv")
dim(dat)
sapply(dat,class)
mean(dat$N)
dat$species <- as.factor(dat$species)
species <- levels(dat$species)
species
dat %>%
  group_by(species) %>%
  summarise(n = n())
dat %>%
  count(species,specimen) %>%
  count (species,name = "n.specimens")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()
list.files(pattern=".pdf")
