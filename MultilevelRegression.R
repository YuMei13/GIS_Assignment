# Multilevel regression project

library(tidyverse)
library(ggplot2)
install.packages('sjmisc')
library(sjmisc)
library(lme4)

setwd('./Assessment3_15012019')
getwd()
data <- read_csv('variables_1.csv') 
# transform csv data to dataframe 
data <- as.data.frame(data)
#set levels #data$REGION <- factor(data$REGION, levels=c('North East','North West','Yorkshire and The Humber','East Midlands','West Midlands','East','London','South East','South West'))
# open new window for plot
dev.new(height=4, width=8)
#boxplot
boxplot(data$Econs2017 ~ data$REGION, data = data, col = "orange", cex.axis=1,xlab = "Region", ylab = "KWh", xaxt = 'n')
axis(1, at = 1:9, lab = c('North East','North West','Yorkshire and The Humber','East Midlands','West Midlands','East','London','South East','South West'), cex.axis=0.8)
boxplot(data$Econs2017, data = data, col = "lightblue", xlab = "Household Energy Consumption",ylab = "KWh")
summary(data$REGION)
## weight B process 
sum(data$Econs2017)
aggregate(data$Econs2017, by=list(Category=data$REGION), FUN=sum)
data$WET_B <- data$Econs2017* (203213400/sum(data$Econs2017*data$Econs2017))

library(dplyr)

# multilevel regression for Null Model with A weighing method 
HLM0 <- lmer(data$Econs2017 ~ (1 | data$REGION_NO), data = data,
             weights = data$WET_A)
coef(HLM0)
summary(HLM0)
100 * 676314 / (676314 + 5756808)

# multilevel regression for Null Model with B weighing method
HLM1 <- lmer(data$Econs2017 ~ (1 | data$REGION_NO), data = data,
             weights = data$WET_B)
coef(HLM1)
summary(HLM1)
100 * 688472 / (688472 + 27575508)

# multilevel regression for Null Model without weighing
HLM2 <- lmer(data$Econs2017 ~ (1 | data$REGION_NO), data = data)
coef(HLM2)
summary(HLM2)
#icc value of HLM0
icc(HLM0)
#test 
anova(Null.gls,HLM1)

Icc <-aov(data$Econs2017 ~as.factor(data$REGION_NO),data=data)
ICC1(Icc)
par(mfrow=c(2,1))
plot(HLM0)
plot(HLM2)
#dd <- data.frame(unlist(data$Econs2017))
# level 1 lm with A weighing
level1<-lm(data$Econs2017 ~1, data=data, weights=data$WET_A)
# level 1 lm with B weighing
level1_1<-lm(data$Econs2017 ~1, data=data, weights=data$WET_B)

# likelihood ratio test
logLik(HLM2)

-2*logLik(level1_1)-(-2*logLik(HLM1))


install.packages('nlme')
anova(level, HLM0)

library(nlme)

2(-466794.9+505031.4)

# set dummy variable in levels
data$PROT.f <- factor(data$PROP_TYPE, levels=c(1,2,3,4,5,6))
data$PV.f <- factor(data$PV, levels=c(0,1))
is.factor(data$PROT.f)
data$AGE.f <- factor(data$PROP_AGE, levels=c(1,2,3,4))
data$FLO.f <- factor(data$FLOOR_AREA, levels=c(1,2,3,4,5))
data$FLO.f <- factor(data$FLOOR_AREA, levels=c(1,2,3,4,5))
data$CON.f <- factor(data$COUN_TAX, levels=c(1,2,3,4,5,6,7,8,9))
data$LOF.f <- factor(data$LOFT, levels=c(0,1))
data$CW.f <- factor(data$CW, levels=c(0,1))
data$CONS.f <- factor(data$CONS, levels=c(0,1))
data$HEA.f <- factor(data$MAIN_HEAT_FUEL, levels=c(0,1))
data$REG.f <- factor(data$REGION_NO, levels=c(1,2,3,4,5,6,7,8,9))

install.packages("MuMIn")
install.packages("lmerTest")
library(lmerTest)
library(MuMIn)
install.packages("pvaluefunctions")
library(pvaluefunctions)
library(graphics)
library (foreign)
aggregate(data$Econs2017, by=list(Category=data$REGION), FUN=sum)##regoin demand
require(lme4)

install.packages("mitml")
library(mitml)

# test different combination of variables with random intercept in level 2 and residual in level 1
HLM1_1 <- lmer(data$Econs2017 ~ data$PROT.f + (1 | data$REGION_NO), data = data,
             weights = data$WET_B)
HLM1_1_A <- lmer(data$Econs2017 ~ data$PV+ (1 | data$REGION_NO), data = data,
               weights = data$WET_A)
HLM1_2_A <- lmer(data$Econs2017 ~ data$PV+data$PROT.f+ (1 | data$REGION_NO), data = data,
                 weights = data$WET_A)
HLM1_3_A <- lmer(data$Econs2017 ~ data$PV+data$PROT.f+data$AGE.f +(1 | data$REGION_NO), data = data,
                 weights = data$WET_A)
HLM1_4_A <- lmer(data$Econs2017 ~ data$PV+data$PROT.f+data$AGE.f +data$FLO.f+(1 | data$REGION_NO), data = data,
                 weights = data$WET_A )
HLM1_5_A <- lmer(data$Econs2017 ~ data$PV+data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+(1 | data$REGION_NO), data = data,
                 weights = data$WET_A )
HLM1_6_A <- lmer(data$Econs2017 ~ data$PV+data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+(1 | data$REGION_NO), data = data,
                 weights = data$WET_A )
HLM1_7_A <- lmer(data$Econs2017 ~ data$PV+data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$CW.f+data$LOF.f+(1 | data$REGION_NO), data = data,
                 weights = data$WET_A )
HLM1_8_A <- lmer(data$Econs2017 ~ data$PV+data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$CW.f+data$LOF.f+data$CONS.f+(1 | data$REGION_NO), data = data,
                 weights = data$WET_A )
HLM1_9_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+(1 | data$REGION_NO), data = data,
                 weights = data$WET_A, REML=F )
HLM1_lm <- lme(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f,random=~1 | data$REGION_NO,data = data,weights = data$WET_A,control=list(opt="optim") )
summary(HLM1_lm)
summary(HLM1_9_A)
anova(HLM1_9_A)
# p value
p <- pvalues(HLM1_9_A)

summary(HLM1_4_A)
summary(HLM0)
summary(HLM1_1)
anova(HLM1_4_A)
anova(HLM1_1_A, HLM1_2_A,HLM1_3_A,HLM1_4_A,HLM1_5_A,HLM1_6_A,HLM1_7_A,HLM1_8_A,HLM1_9_A,HLM1_10_A)

apsl2lme(HLM1_9_A)
plot(HLM1_10_A)


HLM1_10_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$INC_A+data$REG.f+(1+data$PROT.f | data$REGION_NO), data = data,
                 weights = data$WET_A, RMEL=F )
HLM1_11_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+(1+data$AGE.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
##better one
HLM1_12_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+(1+data$PROT.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
HLM1_13_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+data$INC_A+(1+data$PROT.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
HLM1_14_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+data$INC_A+(1+data$AGE.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
HLM1_15_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+data$INC_A+(1+data$FLO.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
HLM1_16_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+(1+data$FLO.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
HLM1_17_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+data$REG.f+(1+data$PROT.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
HLM1_18_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+(1+data$CON.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
HLM1_19_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+(1 | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
HLM1_20_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+(1+data$CW.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
HLM1_21_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+(1+data$HEA.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
HLM1_22_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+(1+data$LOF.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
HLM1_23_A <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+(1+data$CON.f | data$REGION_NO), data = data,
                  weights = data$WET_A, RMEL=F )
summary(lmerTest::lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$INC_A+(1 | data$REGION_NO), data = data,
                       weights = data$WET_A, RMEL=F))
summary(HLM1_18_A)
HLM1_18_B <- lmer(data$Econs2017 ~ data$PROT.f+data$AGE.f +data$FLO.f+data$CON.f+data$LOF.f+data$CW.f+data$HEA.f+data$PRI+(1+data$CON.f | data$REGION_NO), data = data,
                  weights = data$WET_B, RMEL=F )
summary(HLM1_18_B)
summary(HLM0)
plot(HLM1_18_A)
# anova test each combination 
anova(HLM1_10_A, HLM1_11_A,HLM1_12_A,HLM1_13_A,HLM1_14_A,HLM1_15_A)
anova(HLM1_12_A, HLM1_20_A,HLM1_18_A,HLM1_21_A,HLM1_22_A)
anova(HLM1_18_A, HLM1_23_A)
plot(data$WET_A)
plot(data$WET_B)
multilevelR2(HLM1_18_A)

# Rsquare value
r.squaredGLMM(HLM1_18_A)

table(data$REGION)

