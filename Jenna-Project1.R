library(ggplot2)
library(tidyverse)
setwd("~/Desktop/BIOL3140/OrgCodeCrushers/Project1/ModProj1- Jenna")
dat <- read.csv("scales.csv")
dat %>%
  group_by(species) %>%
  summarise(n = n())
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")
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