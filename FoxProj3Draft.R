## Importing Libraries

  library(tidyverse) 
  library(ape)
  library(nlme)
  library(geiger)
  library(caper)
  library(phytools)
  library(viridis)
  library(MuMIn)
  library(phangorn)

## Loading Data
  
anole <- read_csv("anole.dat.csv")
anole.eco <- read_csv("anole.eco.csv")

## Data Tibble

anole.log <- anole %>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

## Two Linear Models

anole.log.ph.lm <- lm(HTotal~SVL+PH, anole.log)
anole.log.pd.lm <- lm(HTotal~SVL+ArbPD, anole.log)

## Adding Residuals Via Mutation

anole.log <- anole.log %>%
  mutate(res.ph = residuals(anole.log.ph.lm), 
         res.pd = residuals(anole.log.pd.lm))

#Effect of Perch Height and Diameter

anole.log%>%
  dplyr::select(Ecomorph2,res.ph,res.pd)%>%
  pivot_longer(cols=c("res.ph","res.pd"))%>%
  print%>%
  ggplot(aes(x=Ecomorph2,y=value)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=3)+
  facet_grid(name~.,scales = "free_y")+ylab("residual")

## * A PGLS model with the hindlimb-SVL relationship + perch height
## * A PGLS model with the hindlimb-SVL relationship + perch diameter
## * A PGSL model with the hindlimb-SVL relationship + perch height + perch diameter

anole.tree <- read.tree("anole.tre")

pgls.BM.ph <- gls(HTotal~SVL + PH, 
                  correlation = corBrownian(1, phy = anole.tree, form =~Species), 
                  data= anole.log, method = 'ML')

pgls.BM.pd <- gls(HTotal~SVL + ArbPD, 
                  correlation = corBrownian(1, phy = anole.tree, form =~Species), 
                  data= anole.log, method = 'ML')

pgls.BM.ph.pd <- gls(HTotal~SVL + PH + ArbPD, 
                     correlation = corBrownian(1, phy = anole.tree, form =~Species), 
                     data= anole.log, method = 'ML')

## Check Fit

anole.phylo.aic <- MuMIn::AICc(pgls.BM.ph, pgls.BM.pd, pgls.BM.ph.pd)
aicw(anole.phylo.aic$AICc)

## Plot (Question 6?)