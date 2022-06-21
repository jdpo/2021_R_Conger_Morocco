#Author: Jan-Dag Pohlmann
#Year: 2021

library(lme4)
library(car)
library(stats)
library(MuMIn)
library(multcomp)
library(tidyverse)

conger <- read.csv("congeR.csv", header = T, sep  = ";")
conger <- conger %>% 
  mutate(Hg = Hg * 1000)


#TESTING FOR REGION GENERALLY REVEALED IT IS NOT MEANINGFUL


################################
# 1. Selection of the Hg model #
################################



glm.Hg <-  glm(Hg ~ length * station + 0,
            family=Gamma(link = "log"),
            data = conger)

plot(glm.Hg)         
summary(glm.Hg)
Anova(glm.Hg)

glm.Hg.reduced1 <-  glm(Hg ~ station + length + 0,
               family=Gamma(link = "log"),
               data = conger)

glm.Hg.reduced2 <-  glm(Hg ~ station + 0,
                       family=Gamma(link = "log"),
                       data = conger)

glm.Hg.reduced3 <-  glm(Hg ~ length + 0,
                       family=Gamma(link = "log"),
                       data = conger)

lrtest(glm.Hg, glm.Hg.reduced1, glm.Hg.reduced2, glm.Hg.reduced3)
AIC(glm.Hg, glm.Hg.reduced1, glm.Hg.reduced2, glm.Hg.reduced3)


#fitting final model for visualisation and predictions used in Summary_results.Rmd

glm.Hg <-  glm(Hg ~ station + station:length + 0,
               family=Gamma(link = "log"),
               data = conger)

################################
# 2. Selection of the Pb model #
################################

glm.Pb <-  glm(Pb ~  length * station + 0,
             family=Gamma(link = "log"),
             data = conger)

plot(glm.Pb)         
summary(glm.Pb)
summary(glm.Pb.nolength)
Anova(glm.Pb)

glm.Pb.reduced1 <-  glm(Pb ~  station + length + 0,
               family=Gamma(link = "log"),
               data = conger)

glm.Pb.reduced2 <-  glm(Pb ~  station +  0,
                        family=Gamma(link = "log"),
                        data = conger)

glm.Pb.reduced3 <-  glm(Pb ~  length + 0,
                        family=Gamma(link = "log"),
                        data = conger)

lrtest(glm.Pb, glm.Pb.reduced1, glm.Pb.reduced2, glm.Pb.reduced3)
AIC(glm.Pb, glm.Pb.reduced1, glm.Pb.reduced2, glm.Pb.reduced3)


#fitting final model for visualisation and predictions, but not used in Summary_results.Rmd, instead the mean is used.

glm.Pb <-  glm(Pb ~  station + length:station + 0,
               family=Gamma(link = "log"),
               data = conger)

#since a decline of Pb with length makes no biological sense, an informed choice was made to remove length

glm.Pb.nolength <-  glm(Pb ~  station + 0,
                        family=Gamma(link = "log"),
                        data = conger)


################################
# 3. Selection of the Cd model #
################################

conger.Cd <- conger[!is.na(conger$Cd),]


glm.Cd <-  glm(Cd ~  length * station + 0,
               family=Gamma(link = "log"),
               data = conger.Cd)

plot(glm.Cd)         
summary(glm.Cd)
Anova(glm.Cd)

glm.Cd.reduced1 <-  glm(Cd ~ station + length + 0,
                        family=Gamma(link = "log"),
                        data = conger.Cd)

glm.Cd.reduced2 <-  glm(Cd ~ station + 0,
                        family=Gamma(link = "log"),
                        data = conger.Cd)

glm.Cd.reduced3 <-  glm(Cd ~ length + 0,
                        family=Gamma(link = "log"),
                        data = conger.Cd)

lrtest(glm.Cd, glm.Cd.reduced1, glm.Cd.reduced2, glm.Cd.reduced3)
AIC(glm.Cd, glm.Cd.reduced1, glm.Cd.reduced2, glm.Cd.reduced3)


#fitting final model for visualisation and predictions used in Summary_results.Rmd

glm.Cd <-  glm(Cd ~  station + station:length + 0,
               family=Gamma(link = "log"),
               data = conger.Cd)
