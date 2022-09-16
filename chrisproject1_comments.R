library(ggplot2)
library(tidyverse)
#CPK: No need to set the wd if you're working in an R project
setwd("C:/Users/chris/Downloads/scales")
#CPK: This file insn't in your repo/dir so I can't run this line. Remember to include all data! I added it. [-1]
dat <- read.csv("scales.csv")
dim(dat)
sapply(dat,class)

#CPK: This is unneeded.[-1]
mean(dat$N)
dat$species <- as.factor(dat$species)
species <- levels(dat$species)

#CPK: This is unneeded.[-1]
species
dat %>%
  group_by(species) %>%
  summarise(n = n())
dat %>%
  count(species,specimen) %>%
  count (species,name = "n.specimens")

#CPK: This is unneeded, it doens't produce PDFs [-1]
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
#CPK: This is unneeded.[-1]
list.files(pattern=".pdf")


#CPK: Good work. Just be careful to include only what's need and no less. And be sure to push/commit, not use file upload [-1]. It's important part of our workflow.
