gc()
rm(list=ls())
options(scipen = 999)

setwd("C:/R Studio Files/Regime-Type-Politics-Institutions-Data/regime-information")

library(maxLik)
library(car)
library(foreign)
library(ggplot2)
library(GGally)
library(grid)
library(gridExtra)
library(censReg)
library(VGAM)
library(mvtnorm)
library(gmodels)
library(Hmisc)
library(MASS)
library(ordinal)
library(reshape)
library(stargazer)
library(AER)
library(glmx)
library(plm)
library(pglm)
library(mediation)
library(Hmisc)
library(coefplot)
library(dplyr)

#Import Polity V Dataset

library(readxl)
PolityV <- read_excel("p5v2018.xls")
View(PolityV)

summary(PolityV)
str(PolityV)

Selectorate1 <- PolityV[,c("p5","cyear","ccode","scode","country","year","flag","fragment",
                           "democ","autoc","durable","xrreg","xrcomp","xropen","xconst",
                           "parreg","parcomp","exrec","exconst","polcomp","sf","regtrans")]


VDem <- readRDS("C:/R Studio Files/Authoritarian Politics Project/Conflict Data/V-Dem-CY-Full+Others-v9.rds")
View(VDem)

VDem$ccode <- VDem$COWcode

attach(VDem)
Selectorate2 <- VDem[,c("country_name","country_text_id","country_id","ccode","COWcode","year",
                        "v2regsupgroupssize","v2x_suffr","v2x_elecoff","v2x_partip","v2xdd_dd",
                        "v2elmulpar","v2elrstrct","v2elvotlrg","v2elwestmon","v2psbars","v2psparban",
                        "v2psoppaut","v2pscnslnl","v2psnatpar","v2exremhsp","v2exrmhsol_0",
                        "v2exrmhsol_1","v2exrmhsol_2","v2exrmhsol_3","v2exrmhsol_4","v2exrmhsol_5","v2exrmhsol_6",
                        "v2exrmhsol_7","v2exctlhs_0",
                        "v2exctlhs_1","v2exctlhs_2","v2exctlhs_3","v2exctlhs_4","v2exctlhs_5",
                        "v2exctlhs_6","v2exctlhs_7","v2exhoshog","v2exremhog","v2exrmhgnp_0",
                        "v2exrmhgnp_1","v2exrmhgnp_1","v2exrmhgnp_3","v2exrmhgnp_4","v2exrmhgnp_5",
                        "v2exrmhgnp_6","v2exrmhgnp_7","v2exctlhg_0",
                        "v2exctlhg_1","v2exctlhg_2","v2exctlhg_3","v2exctlhg_4","v2exctlhg_5","v2exctlhg_6",
                        "v2exctlhg_7","v2exrescon","v2exagehog","v2exagehos","v2exaphogp",
                        "v2exaphos","v2exapup","v2exapupap","v2expathhg","v2expathhs",
                        "v2regimpgroup","v2dlencmps","v2clrspct","v2stfisccap","v2svstterr",
                        "v2exl_legitratio","v2exl_legitperf","v2exl_legitideol","v2exl_legitlead","v2regsupgroups_1",
                        "v2regsupgroups_2","v2regsupgroups_3","v2regsupgroups_4","v2regsupgroups_5","v2regsupgroups_6",
                        "v2regsupgroups_7","v2regsupgroups_8","v2regsupgroups_9","v2regsupgroups_10","v2regsupgroups_11",
                        "v2regsupgroups_12","v2regsupgroups_13")]

Selectorate <- merge(Selectorate1,Selectorate2,by = c("ccode","year"))

#Build W

#W1 = conditional on milsup = 1 

summary(Selectorate$v2regsupgroups_5)
Selectorate$W1 <- ifelse(Selectorate$v2regsupgroups_5 != 1,1,0)
summary(Selectorate$W1)

#W2 = conditional on xrcomp >= 2
summary(Selectorate$xrcomp)
Selectorate$W2 <- ifelse(Selectorate$xrcomp >= 2,1,0)
summary(Selectorate$W2)


#W3 = conditional on xropen >= 2

summary(Selectorate$xropen)
Selectorate$W3 <- ifelse(Selectorate$xropen >= 2,1,0)
summary(Selectorate$W3)

#W4 = conditional on parcomp = 5

summary(Selectorate$parcomp)
Selectorate$W4 <- ifelse(Selectorate$parcomp == 5,1,0)
summary(Selectorate$W4)

#W = W1 + W2 + W3 + W4
Selectorate$W = Selectorate$W1 + Selectorate$W2 + Selectorate$W3 + Selectorate$W4
summary(Selectorate$W)

write.csv(Selectorate,"selectorate-miscellaneous-measures.csv")

