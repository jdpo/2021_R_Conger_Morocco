#Author: Jan-Dag Pohlmann
#Year: 2021

library(lme4)
library(nlme)
library(car)
library(stats)
library(MuMIn)
library(multcomp)
library(tidyverse)

conger <- read.csv("R.csv", header = T, sep  = ";")
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

options(na.action = "na.fail")
select.glm.Hg <- dredge(glm.Hg, rank = "AIC", evaluate = T)
options(na.action = "na.omit")


#fitting final model for visualisation and predictions

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

options(na.action = "na.fail")
select.glm.Pb <- dredge(glm.Pb, rank = "AIC", evaluate = T)
options(na.action = "na.omit")


#fitting final model for visualisation and predictions

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

options(na.action = "na.fail")
select.glm.Cd <- dredge(glm.Cd, rank = "AIC", evaluate = T)
options(na.action = "na.omit")


#fitting final model for visualisation and predictions

glm.Cd <-  glm(Cd ~  station + length:station + 0,
               family=Gamma(link = "log"),
               data = conger.Cd)


