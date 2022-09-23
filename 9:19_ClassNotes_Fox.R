#Left side is predicted by ~ Right side
library(tidyverse)

set.seed(123)
x.A=1:50
y.A=x.A*2+runif(50,1,200)
x.B=1:50
y.B=x.B*3.5+runif(50,1,200)
d <- tibble(x=c(x.A,x.B), y=c(y.A,y.B), species = c(rep("A",50),rep("B",50)))

lm(y~x*d)
#Difference in intercept --> x+d
#Difference in slope --> x*d

##

#Comparitive method --> how does one thing vary with another?
#corBrownian
#corMartins

#gls()
#AIC = Lowest is better
#AIC(Fit1, Fit2)
