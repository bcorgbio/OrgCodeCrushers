## Module 3 Project - OrgCodeCrushers
## Brendan F, Jerry H, Chris P, Jenna W

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

## 1 Establishing Data Tibble

anole.log <- anole %>%
  left_join(anole.eco)%>%
  filter(!Ecomorph%in%c("U","CH"))%>%
  na.omit()%>%
  mutate_at(c("SVL", "HTotal","PH","ArbPD"),log)

## 2 Two Linear Models To Assess Effects of Perch Height and Perch Diameter

anole.log.ph.lm <- lm(HTotal~SVL+PH, anole.log)
anole.log.pd.lm <- lm(HTotal~SVL+ArbPD, anole.log)

## 3 Adding Residuals Via Mutation to Assess the Effects of Heightlimb-SVL relationship

anole.log <- anole.log %>%
  mutate(res.ph = residuals(anole.log.ph.lm), 
         res.pd = residuals(anole.log.pd.lm))

## Effect of Perch Height and Diameter

anole.log%>%
  dplyr::select(Ecomorph2,res.ph,res.pd)%>%
  pivot_longer(cols=c("res.ph","res.pd"))%>%
  print%>%
  ggplot(aes(x=Ecomorph2,y=value)) +
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=3)+
  facet_grid(name~.,scales = "free_y")+ylab("residual")

## 4* A PGLS model with the hindlimb-SVL relationship + perch height
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

## 5 Check Fit of 3 Models

anole.phylo.aic <- MuMIn::AICc(pgls.BM.ph, pgls.BM.pd, pgls.BM.ph.pd)
aicw(anole.phylo.aic$AICc)

# fit     delta           w
# 1 -64.77956 10.746149 0.003247185
# 2 -73.81081  1.714901 0.296905077
# 3 -75.52571  0.000000 0.699847738

## 6 Plot 
anole.log <- anole.log %>%
  mutate(phylo.res = residuals(pgls.BM.ph.pd))
anole.log %>% 
  dplyr::select(Ecomorph2,res.ph,res.pd,phylo.res) %>%
  pivot_longer(cols = c("res.ph","res.pd", "phylo.res"))%>%
  print%>%
  ggplot(aes(x = Ecomorph2,y = value)) + 
  geom_boxplot() +
  stat_summary(fun=mean, geom="point", size=3)+
  facet_grid(name~.,scales = "free_y") + ylab("residual")