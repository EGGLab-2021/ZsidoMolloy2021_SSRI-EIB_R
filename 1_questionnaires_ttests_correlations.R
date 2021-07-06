#EEG Data Analysis - ASEC, STAI, POMS, ESS questionnaires: t-tests and correlational analyses 
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
questionnaires_eeg <-read.csv("", header = T)

#2 Set factors for independent samples t-tests
questionnaires_eeg$day = factor(questionnaires_eeg$Day)
questionnaires_eeg$group = factor(questionnaires_eeg$Group)
questionnaires_eeg$day1 = questionnaires_eeg$XXX #XXX assign day-1 measure for specific questionnaire 
questionnaires_eeg$day7 = questionnaires_eeg$YYY #YYY assign day-7 measure for specific questionnaire 
str(questionnaires_eeg)

#3 Independent samples t-tests for ASEX, STAI, POMS, ESS
t.test(day1  ~ Group, paired=FALSE, var.equal=FALSE, data=questionnaires_eeg)
t.test(day7  ~ Group, paired=FALSE, var.equal=FALSE, data=questionnaires_eeg)

#4 Bivariate correlational analyses - followup in escitalopram group for ASEC
cors <- read.csv("STAI_ASEC_Plasma_Mean_Brain.csv", header = T)
cors <- subset(cors, Group != "Placebo", rename=c())

#ASEC and escitalopram plasma levels
#day 1
cor.test(cors$ASEC_D1, cors$Plasma_D1, 
         method = "pearson")
#day 7
cor.test(cors$ASEC_D7, cors$Plasma_D7, 
         method = "pearson")

#ASEC and alpha power
#day 1
cor.test(cors$ASEC_D1, cors$Power_Log10_Day1, 
         method = "pearson")
#day 7
cor.test(cors$ASEC_D7, cors$Power_Log10_Day7, 
         method = "pearson")

#ASEC and slope
#day 1
cor.test(cors$ASEC_D1, cors$Slope_2, 
         method = "pearson")
#day 7
cor.test(cors$ASEC_D7, cors$Slope_3, 
         method = "pearson")
