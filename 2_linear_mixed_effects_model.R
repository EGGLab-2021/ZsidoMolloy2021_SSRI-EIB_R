#EEG Data Analysis - linear mixed effect modeling
#Code for Zsido & Molloy et al., 2021, BioRxiv
#Written by Rachel G. Zsido and Eóin N. Molloy
###############################################################################################################################################
#0 Load packages
library(magrittr)
library(dplyr)
library(lme4)
library(car)
library(lmerTest)
library(psych)
require(MuMIn)
library(ggpubr)
library(rmcorr)

#1 Set directory and load data .csv file
setwd('')
data_eeg <-read.csv("", header = T)

#2 Set factors for lmer
data_eeg$day = factor(data_eeg$Day)
data_eeg$group = factor(data_eeg$Group)
data_eeg$measure = data_eeg$XXX #XXX assign dependent measure name, e.g. slope, alpha power 
str(data_eeg)

#3 Slope Model fitting
#A Intercept Only
Brain_Intercept <- lmer(measure ~  (1|Subject), data=data_eeg, REML = F)
summary(Brain_Intercept)
#B With Day
Brain_Time <- lmer(measure ~  day + (1|Subject), data=data_eeg, REML = F)
summary (Brain_Time)
#C Compare A and B
anova(Brain_Intercept, Brain_Time)
#D With both main effects of group and day
Brain_Both <- lmer(measure ~  day + group + (1|Subject), data=data_eeg, REML = F)
summary (Brain_Both)
#E Compare B and D
anova(Brain_Time, Brain_Both)
#F With both main effects in interaction
Brain_Interaction <- lmer(measure ~ group*day + (1|Subject), data = data_eeg, REML = F)
summary(Brain_Interaction)
#G Compare D with F
anova(Brain_Both, Brain_Interaction)
#H Marginal R squared for fixed effects
r.squaredGLMM(Brain_Intercept)
r.squaredGLMM(Brain_Time)
r.squaredGLMM(Brain_Both) 
r.squaredGLMM(Brain_Interaction) 

###############################################################################################################################################
#1 Set directory and load data .csv file
setwd('')
means <-read.csv("", header = T)

#2 Set factors for post-hoc t-tests
means$baseline = data_eeg$YYY #YYY assign baseline measure, e.g. slope, alpha power
means$day1 = data_eeg$ZZZ #ZZZ assign day-1 measure, e.g. slope, alpha power 
means$day7 = data_eeg$AAA #AAA assign day-7 measure, e.g. slope, alpha power 

#3 Between-group:
t.test(baseline ~ Group, data = means, var.equal = FALSE)
t.test(day1 ~ Group, data = means, var.equal = FALSE)
t.test(day7 ~ Group, data = means, var.equal = FALSE)

#4 Within-group - escitalopram
SSRI<-subset(means, Group != "Placebo",rename=c())
t.test(SSRI$baseline, SSRI$day7, paired = TRUE, alternative = "two.sided")
t.test(SSRI$baseline, SSRI$day1, paired = TRUE, alternative = "two.sided")
t.test(SSRI$day1, SSRI$day7, paired = TRUE, alternative = "two.sided")
#5 Within-groups - placebo
PLAC<-subset(means, Group != "Escitalopram",rename=c())
t.test(PLAC$baseline, PLAC$day7, paired = TRUE, alternative = "two.sided")
t.test(PLAC$baseline, PLAC$day1, paired = TRUE, alternative = "two.sided")
t.test(PLAC$day1, PLAC$day7, paired = TRUE, alternative = "two.sided")