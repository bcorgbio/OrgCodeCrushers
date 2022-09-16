library(ggplot2)
library(tidyverse)
#CPK: No need to set the wd if you're working in an R project
setwd("~/Desktop/OrgCodeCrushers")

#CPK: This file insn't in your repo/dir so I can't run this line. Remember to include all data! I added it. [-1]
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

#CPK: Great stuff!!Just be sure to push/commit, not use file upload [-1]. It's important part of our workflow.