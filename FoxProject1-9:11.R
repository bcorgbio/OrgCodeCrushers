library(ggplot2)
library(tidyverse)

setwd("~/Desktop/OrgCodeCrushers")

#1
dat <- read.csv("scales.csv")

#2
sapply(dat,class)

#3
dim(dat)

#4
dat %>% count(species)

#5
species <- levels(as.factor(dat$species))
dat %>% 
  count(species,specimen) %>%
  count(species,name = "n.specimens")

#6
pdf("Fox.Module1.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}

dev.off()