#EEG Data Analysis - linear Regression prediction analyses
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
reg <-read.csv("", header = T)

#2 Base vs Day1
linearMod_1 <- lm(Base_ResD7 ~ Day1_ResD7, data=reg)
summary(linearMod_1)
#3 Base vs Day7
linearMod_2 <- lm(Base_ResD1 ~ Day7_ResD1, data=reg)
summary(linearMod_2)
#4 Day1 vs Day 7
linearMod_3 <- lm(Day_1_ResBL ~ Day_7_ResBL, data=reg)
summary(linearMod_3)