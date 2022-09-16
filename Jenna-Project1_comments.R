#Project 1 Module 1-Jenna Wilken

#1 Load Packages
library(ggplot2)
library(tidyverse)

#CPK: No need to set the wd if you're working in an R project
setwd("~/Desktop/BIOL3140/OrgCodeCrushers/Project1/ModProj1- Jenna")

#CPK: This file insn't in your repo/dir so I can't run this line. Remember to include all data! I added it. [-1]
#2 Variable containing data set
dat <- read.csv("scales.csv")

#3 Dimensions and class of data set
dim(dat)

#CPK: This is unneeded.[-1]
mean(dat$N)
sapply(dat,class)

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
#CPK: This is unneeded, it doens't produce PDFs [-1]
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

#CPK: Really good work. Just be careful to include only what's need and no less. 