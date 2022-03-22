library(lme4)
library(nlme)
library(car)
library(stats)
library(MuMIn)
library(multcomp)


setwd("C:/Users/pohlmann/Desktop/Home_Office/GIT/shared/Conger_Ulrike")       #hier innerhalb der "" das Verzeichnis eintragen in dem die Tabelle liegt (nicht n?tig, spart aber Zeit) - statt "\" verwendet R "/"

path <- file.choose()
conger <- read.csv(path, header = T, sep  = ";")



#Anpassung von "mixed-models", gef?hlt der standard f?r den vorliegenden Datensatz. Im wesentlichen wird Hg durch L?nge vorhergesagt. Unterschiede zwischen Stationen werden ber?cksichtigt aber 
#im Ergebnis wird nur ersichtlich wieviel Varianz auf Station zur?ckzuf?hren ist. Eine mE gute Variante wenn man den Effekt von "length" (bzw. auch "region")
#im Mittel gut quantifizieren will. Vorhersagen sind auch pro station m?glich.
#Vorteil: Station als random effect "frisst" nur einen Freiheitsgrad, als fixed Effect (s.u.) frisst er so viele wie es Stationen gibt - u.U. mehr.  
#Nachteil: Wir brauchen pro random effects level (=Anzahl Conger pro Station) min. 5 Messungen - aber die haben wir.
#Nachteil: Wir bekommen wesentlich weniger Info dar?ber wie genau die Effekte von "Station" aussehen - Vorhersagen pro Station sind aber m?glich.


#TESTING FOR REGION GENERALLY REVEALED IT IS NOT NEEDED


########################################################
# 1. SELEKTION DES MODELLS ANHAND BEKANNTER DATEN (Hg) #
########################################################



glm.Hg <-  glm(Hg ~ length * station + region,
            family=Gamma(link = "log"),
            data = conger)

plot(glm.Hg)         
summary(glm.Hg)
Anova(glm.Hg)

options(na.action = "na.fail")
select.glm.Hg <- dredge(glm.Hg, rank = "AIC", evaluate = T)
options(na.action = "na.omit")


#fit des finalen Modells (Der AIC mit und ohne Region ist praktisch ident, daher einfacheres Modell)

glm.Hg <-  glm(Hg ~ station + station:length + 0,
             family=Gamma(link = "log"),
             data = conger)



########################################################
# 2. SELEKTION DES MODELLS ANHAND BEKANNTER DATEN (Pb) #
########################################################

glm.Pb <-  glm(Pb ~  length * station + region,
             family=Gamma(link = "log"),
             data = conger)

plot(glm.Pb)         
summary(glm.Pb)
summary(glm.Pb.nolength)
Anova(glm.Pb)

options(na.action = "na.fail")
select.glm.Pb <- dredge(glm.Pb, rank = "AIC", evaluate = T)
options(na.action = "na.omit")


#fit des finalen Modells (Der AIC mit und ohne Region ist praktisch ident, daher einfacheres Modell)

glm.Pb <-  glm(Pb ~  station + length:station + 0,
               family=Gamma(link = "log"),
               data = conger)

glm.Pb.nolength <-  glm(Pb ~  station + 0,
                        family=Gamma(link = "log"),
                        data = conger)



########################################################
# 3. SELEKTION DES MODELLS ANHAND BEKANNTER DATEN (Cd) #
########################################################

conger.Cd <- conger[!is.na(conger$Cd),]


glm.Cd <-  glm(Cd ~  length * station + region,
               family=Gamma(link = "log"),
               data = conger.Cd)

plot(glm.Cd)         
summary(glm.Cd)
Anova(glm.Cd)

options(na.action = "na.fail")
select.glm.Cd <- dredge(glm.Cd, rank = "AIC", evaluate = T)
options(na.action = "na.omit")


#fit des finalen Modells (Der AIC mit und ohne Region ist praktisch ident, daher einfacheres Modell)

glm.Cd <-  glm(Cd ~  station + length:station + 0,
               family=Gamma(link = "log"),
               data = conger.Cd)


#################################################################################################
#ALTERNATIV ALS MIXED MODEL (Gibt etwas bessere Schaetzungen für die Parameter bzw. v.a. deren  #
#Stabw etc., allerdings kann man den random Effect (Station) nicht mehr auf Unterschiede TEsten)#        
#################################################################################################

glmer1 <-  glmer(Hg ~  length + (1+length|station/region),
                 family=Gamma(link = "log"),
                 #control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                 data = conger)

conger$station <- as.factor(conger$station)

plot(glmer1)         
qqnorm(resid(glmer1))
qqline(resid(glmer1))
summary(glmer1)

anova(glmer1)


##### UNUSED GRAPH########
#ggplot(conger, aes(x=length, y=Hg)) +
#geom_point(aes(col = station, shape = station), size=1.5) +
#theme_bw()+
#geom_line(aes(x= length, y=predict(lme4, type="response")), size = 0.3)+
#geom_line(aes(x= length, y=predict(lme4, type="response"), col = station), linetype = "dashed", size = 0.3)+
#labs(x = "Hg (?g/kg)", y = "length (cm)")+
#facet_wrap(~ region)