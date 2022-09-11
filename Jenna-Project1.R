#Project 1 Module 1-Jenna Wilken

#1 Load Packages
library(ggplot2)
library(tidyverse)
setwd("~/Desktop/BIOL3140/OrgCodeCrushers/Project1/ModProj1- Jenna")

#2 Variable containing data set
dat <- read.csv("scales.csv")

#3 Summary of how many scales were punctured in each species
dat %>%
  group_by(species) %>%
  summarise(n = n())

#4 Summary of individuals tested in each species
dat %>% 
  count(species,specimen) %>%
  print() %>%
  count(species,name = "n.specimens")

#5 Line that produces 6 box plots of puncture force vs quadrant for each species
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}

#6 Line that produces PDF of the respective box plots
pdf("species.quadrant.pdf")
for(i in species){
  p <- dat %>%
    filter(species==i)%>%
    ggplot()+geom_boxplot(aes(x=quadrant,y=N))+ggtitle(i)
  print(p)
}
dev.off()