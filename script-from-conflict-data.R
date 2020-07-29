gc()
rm(list=ls())
options(scipen = 999)

setwd("C:/R Studio Files/Authoritarian Politics Project/Conflict Data")

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

#import First Use of Violent Force Dataset (Caprioli and Trumbore)
#http://www.d.umn.edu/~mcapriol/

library(readr)
FUFv1_10 <- read_csv("FUFv1.10.csv")
#View(FUFv1_10)



#read V-Dem dataset

library(stats)
VDem <- readRDS("C:/R Studio Files/Authoritarian Politics Project/Conflict Data/V-Dem-CY-Full+Others-v9.rds")
View(VDem)


#subset Vdem data I neeed
ConflictPolicy <- VDem[,c("year","COWcode","country_name","v2x_polyarchy","v2x_libdem",
                                "v2x_partipdem","v2x_delibdem","v2x_egaldem","v2x_regime","v2x_regime_amb",
                                "v2x_ex_military","v2x_ex_confidence","v2x_ex_direlect","v2x_ex_hereditary",
                                "v2x_ex_party","v2x_neopat","v2xnp_client","v2x_frassoc_thick","v2x_jucon",
                                "v2xlg_legcon","v2x_cspart","v2lpname","v2exhoshog","v2exremhog","v2exdjdshg",
                                "v2exdfvthg","v2reginfo","v2regint", "v2regendtype","v2regimpgroup","v2regsupgroupssize",
                                "v2lgoppart","v2dlencmps","v2juhcind","v2juncind","v2juhccomp","v2jucomp","v2jureview",
                                "v2stfisccap","v2svstterr","v2mecenefi","v2mecenefm","v2exl_legitideol","v2exl_legitideolcr_0",
                                "v2exl_legitideolcr_1","v2exl_legitideolcr_2","v2exl_legitideolcr_3","v2exl_legitideolcr_4",
                                "v2xpe_exlsocgr",
                                "v2x_gencl","v2x_rule","v2xcl_prpty","v2xcs_ccsi","v2x_clpol","v2x_clpriv","v2clfmove",
                                "v2xcl_dmove","v2cldmovem","v2cldmovew","v2cldiscm",
                                "v2cldiscw","v2clslavem","v2clslavef","v2clstown","v2clprptym","v2clprptyw","v2clacjstm",
                                "v2clacjstw", "e_legparty","e_autoc","e_peaveduc","e_migdppc","e_cow_exports",
                                "e_cow_imports","e_total_fuel_income_pc","e_total_oil_income_pc","e_miurbani",
                                "e_mipopula","e_civil_war","e_miinteco","v2exrmhgnp_0","v2exrmhgnp_1",
                                "v2exrmhgnp_2","v2exrmhgnp_3","v2exrmhgnp_4","v2exrmhgnp_5","v2exrmhgnp_6",
                                "v2exrmhgnp_7","v2exrmhgnp_8","v2exctlhg_0","v2exctlhg_1","v2exctlhg_2","v2exctlhg_3",
                                "v2exctlhg_4","v2exctlhg_5","v2exctlhg_6","v2exctlhg_7","v2exctlhg_8",
                                "v2regsupgroups_0","v2regsupgroups_1","v2regsupgroups_2","v2regsupgroups_3",
                                "v2regsupgroups_4","v2regsupgroups_5","v2regsupgroups_6","v2regsupgroups_7",
                                "v2regsupgroups_8","v2regsupgroups_9","v2regsupgroups_10","v2regsupgroups_11",
                                "v2regsupgroups_12","v2regsupgroups_13",
                                "v2exl_legitideolcr_0","v2exl_legitideolcr_1","v2exl_legitideolcr_2",
                                "v2exl_legitideolcr_3","v2exl_legitideolcr_4")]
                  



#remove any NAs from index columns

ConflictPolicy <- ConflictPolicy[!rowSums(is.na(ConflictPolicy["COWcode"])),]

ConflictPolicy <- ConflictPolicy[!rowSums(is.na(ConflictPolicy["year"])),]

#organize FUFV

FUFv1_10$COWcode <- FUFv1_10$ccode

FUFv1_10$year <- FUFv1_10$fufyear

#old code - removed some variables not used
#FUFVData <- FUFv1_10[,c("COWcode","year","scode","fuf","fufdate","endate","fufjoin","midjoin")]

FUFVData <- FUFv1_10[,c("COWcode","year","scode","fuf")]


#remove NAs from index columns

FUFVData <- FUFVData[!rowSums(is.na(FUFVData["year"])),]

FUFVData <- FUFVData[!rowSums(is.na(FUFVData["COWcode"])),]

FUFVData$fuf <- ifelse(FUFVData$fuf == 1,1,0)

summary(FUFVData$fuf)

FUFVData$initiator <- FUFVData$fuf

FUFVData <- subset(FUFVData, FUFVData$initiator == 1)

#Merge CIP and FUFVD

MergedData1 <- merge(FUFVData, ConflictPolicy, by.x = c("COWcode","year"), all = TRUE)

#subset years

MergedData1 <- subset(MergedData1, year > 1979 & year < 2002)

###use Correlates of War National Military Capabilities data

library(haven)
NMC_5_0 <- read_dta("NMC_5_0.dta")
View(NMC_5_0)

CINC <- NMC_5_0[,c("ccode","year","cinc","milex","milper")]
CINC$COWcode <- CINC$ccode

CINC <- subset(CINC, year > 1979 & year < 2002)
MergedData1 <-  merge(MergedData1, CINC, by.x = c("COWcode","year"), all = TRUE)

MergedData1$cincp <- (MergedData1$cinc*100)



#MergedData2$initiator <- ifelse(MergedData2$fuf == 1,1,0)

# Non-Democracies only
NonDemocraciesConflict <- MergedData1[ which(MergedData1$v2x_regime_amb < 6), ]

summary(NonDemocraciesConflict$initiator)





#create control variables

#lagged conflict status

NonDemocraciesConflict$lag.confstat <- Lag(NonDemocraciesConflict$e_miinteco)	

NonDemocraciesConflict$conflict.status <- NonDemocraciesConflict$e_miinteco

#TIME SINCE LAST CONFLICT





#total trade

NonDemocraciesConflict$trade <- NonDemocraciesConflict$e_cow_exports + NonDemocraciesConflict$e_cow_imports

#per capita GDP - e_migdppc

summary(NonDemocraciesConflict$e_migdppc)

#GDP

NonDemocraciesConflict$gdp <- (NonDemocraciesConflict$e_migdppc * NonDemocraciesConflict$e_mipopula * 1000)

NonDemocraciesConflict$coldwar <- ifelse(NonDemocraciesConflict$year > 1947 & NonDemocraciesConflict$year < 1991,1,0)

#create a variable to represent the erosion of the rule of law
NonDemocraciesConflict$rule.inverse <- (1 - NonDemocraciesConflict$v2x_rule)
summary(NonDemocraciesConflict$rule.inverse)
summary(NonDemocraciesConflict$v2x_rule)



#Convert NAs to zero for initiation of force 

library(dplyr)        

f <- numeric(2391)
NonDemocraciesConflict$conf_init <- coalesce(NonDemocraciesConflict$initiator,f)
summary(NonDemocraciesConflict$conf_init)

#In Russia for several years, Russia initiated a conflict, but the conflict status
#shows as zero. This is logically impossible. If a country starts a conflict, conflict status must be
#1 for at least one year


summary(NonDemocraciesConflict$conflict.status)

NonDemocraciesConflict$conflict.status <- with(NonDemocraciesConflict, ifelse(NonDemocraciesConflict$conf_init==1,1,NonDemocraciesConflict$conflict.status))

summary(NonDemocraciesConflict$conflict.status)

#Convert NAs to zeros for conflict.status
summary(NonDemocraciesConflict$conflict.status)

NonDemocraciesConflict$conflict.status <- coalesce(NonDemocraciesConflict$conflict.status,f)

summary(NonDemocraciesConflict$conflict.status)


# 
# NonDemocraciesConflict <- NonDemocraciesConflict2


#Create binary variables

NonDemocraciesConflict$jucon1 <- ifelse(NonDemocraciesConflict$v2x_jucon >= .5, 1, 0)

NonDemocraciesConflict$jucon2 <- ifelse(NonDemocraciesConflict$v2x_jucon >= mean(NonDemocraciesConflict$v2x_jucon, na.rm = TRUE),1,0)

NonDemocraciesConflict$legcon1 <- ifelse(NonDemocraciesConflict$v2xlg_legcon >= .5,1,0)

NonDemocraciesConflict$legcon2 <- ifelse(NonDemocraciesConflict$v2xlg_legcon >= mean(NonDemocraciesConflict$v2xlg_legcon, na.rm = TRUE), 1, 0)

NonDemocraciesConflict$constrained.per1 <- ifelse(NonDemocraciesConflict$v2x_neopat >= .5,0,1)

NonDemocraciesConflict$constrained.per2 <- ifelse(NonDemocraciesConflict$v2x_neopat >= mean(NonDemocraciesConflict$v2x_neopat),0,1)

#create combined institutional constraint variables - 1 and 2 include personalism, 3 and 4 do not 

NonDemocraciesConflict$inst.constr1 <- (NonDemocraciesConflict$jucon2 + NonDemocraciesConflict$legcon2 + NonDemocraciesConflict$constrained.per2)

NonDemocraciesConflict$inst.constr2 <- ifelse(NonDemocraciesConflict$inst.constr1 >= mean(NonDemocraciesConflict$inst.constr1, na.rm = TRUE),1,0)

NonDemocraciesConflict$inst.constr3 <- (NonDemocraciesConflict$jucon2 + NonDemocraciesConflict$legcon2)

NonDemocraciesConflict$inst.constr4 <- ifelse(NonDemocraciesConflict$inst.constr3 >= mean(NonDemocraciesConflict$inst.constr3, na.rm = TRUE),1,0)

summary(NonDemocraciesConflict$inst.constr2)

save(NonDemocraciesConflict, file = "NonDemocraciesConflict.RData")


attach(NonDemocraciesConflict)

#############
#Summary Statistics
#######

summary(conf_init)
summary(v2x_clpriv)
summary(v2exl_legitideol)
summary(v2exl_legitideolcr_1)
summary(v2exl_legitideolcr_2)
summary(v2exl_legitideolcr_3)
summary(v2exl_legitideolcr_0)
summary(v2exl_legitideolcr_4)


########################
###Plots ad Correlations
########################

#Private civil liberties and general ideology

with(NonDemocraciesConflict, scatter.smooth(v2exl_legitideol, v2x_clpriv, xlab = "Regime uses ideology for legitimacy",
                                            ylab = "Regime respect for private liberties", lpars =
                                                    list(col = "red", lwd = 5, lty = 1)))

cor(v2exl_legitideol,v2x_clpriv,use = "pairwise.complete.ob")


#ideology and conflict initation, general conflict status

cor(NonDemocraciesConflict$v2exl_legitideol,NonDemocraciesConflict$conf_init,use = "pairwise.complete.ob")

p <- ggplot(data=NonDemocraciesConflict,aes(v2exl_legitideol,conf_init)) + 
        geom_line() + 
        geom_smooth(method='loess')
p + ggtitle("Plot of conflict initiation by regime ideology") +
        xlab("Regime uses ideology as legitimation") + ylab("Conflict initiation")

cor(v2exl_legitideol,e_miinteco, use = "pairwise.complete.ob")

q <- ggplot(data=NonDemocraciesConflict,aes(v2exl_legitideol,conf_init)) + 
        geom_line() + 
        geom_smooth(method='loess')
q + ggtitle("Plot of interstate conflict status by regime ideology") +
        xlab("Regime uses ideology as legitimation") + ylab("Interstate conflict status")

#individual ideology types

cor(v2exl_legitideolcr_0,v2x_clpriv,use = "pairwise.complete.ob")

cor(v2exl_legitideolcr_1,v2x_clpriv,use = "pairwise.complete.ob")

cor(v2exl_legitideolcr_2,v2x_clpriv,use = "pairwise.complete.ob")

cor(v2exl_legitideolcr_3,v2x_clpriv,use = "pairwise.complete.ob")

cor(v2exl_legitideolcr_4,v2x_clpriv,use = "pairwise.complete.ob")

cor(v2exl_legitideolcr_0,conf_init,use = "pairwise.complete.ob")

cor(v2exl_legitideolcr_1,conf_init,use = "pairwise.complete.ob")

cor(v2exl_legitideolcr_2,conf_init,use = "pairwise.complete.ob")

cor(v2exl_legitideolcr_3,conf_init,use = "pairwise.complete.ob")

cor(v2exl_legitideolcr_4,conf_init,use = "pairwise.complete.ob")

write.csv(NonDemocraciesConflict,"nondemocraciesconflict.csv")

detach(NonDemocraciesConflict)


#Generate time since conflict variable - Tcount




#reading the csv file
fileData <- read.csv('nondemocraciesconflict.csv')
#changing all the NA's in conflict status to 0's
fileData$conflict.status[is.na(fileData$conflict.status)] <- 0

fileData$status <- fileData$conflict.status
# names(file)=c("COWcode","year","status")
file=fileData
str(file)

#Selecting the unique Country Codes from the file
xyz=file$COWcode
codes<-unique(xyz)

data=file[file$COWcode==40,]
temp=data[1,]

#Intializing the dataset to NULL, later we will append all the rows in it
MyData=NULL

for (i in codes)
{  
        data=file[file$COWcode==i,]   #Selecting only the data of that country from the file
        T=0           #Intializing the T to zero
        T_altCount=0       #Intializing the T_altCount to zero
        prev=0
        #Prev variable is used to check if the prevConflict is 1, if it is 1 and current conflict is 0, then increment the T_altCount by 1
        for(row in 1:nrow(data))    #All the rows of the selected country data is traversed one by one
        {
                temp=data[row,]            #To get the data of a specific row
                st= data[row, "status"]
                if(st==0)   #if status is zero
                { 
                        T=T+1             #increment T
                        if(T_altCount!=0){           #if the T_altCount is not zero increment it by 1 
                                T_altCount=T_altCount+1
                        }
                        else if(prev == 1) {            #if the prev conflict was one and the current is zero increment the count
                                T_altCount=T_altCount+1
                        }
                }
                else
                { 
                        T=0                #if current conflict is 1, reset both the count is zero
                        T_altCount=0
                }
                prev=temp$status  #assign the current conflict to the previous
                temp=cbind(temp,T,T_altCount) #binding T and T_altCount as columns in data
                MyData=rbind(MyData,temp)#add new data created after adding two columns to the newData variables as row
        }
}
View(MyData)

summary(MyData$T)



write.csv(MyData,"mydata.csv")

#Clear the environment

rm(list=ls())
options(scipen = 999)

setwd("C:/R Studio Files/Authoritarian Politics Project/Conflict Data")

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

#load cleaned data as NonDemocraciesConflict

NonDemocraciesConflict <- read.csv("C:/R Studio Files/Authoritarian Politics Project/Conflict Data/mydata.csv")

NonDemocraciesConflict$T2 <- (NonDemocraciesConflict$T)^2

NonDemocraciesConflict$T3 <- (NonDemocraciesConflict$T)^3

write.csv(NonDemocraciesConflict,"nondemocraciesconflictwithTtoT3.csv")

#demean variables

library(parameters)

NDC <- cbind(NonDemocraciesConflict,demean(NonDemocraciesConflict,select = "conf_init", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2exl_legitideol", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "e_mipopula", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "gdp", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "coldwar", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "lag.confstat", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "cincp", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2x_clpriv", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2x_rule", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2x_clpriv", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2exl_legitideolcr_0", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2exl_legitideolcr_1", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2exl_legitideolcr_2", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2exl_legitideolcr_3", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2exl_legitideolcr_4", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2x_ex_hereditary", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2x_ex_party", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2x_ex_military", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2x_neopat", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "v2regsupgroups_7", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "T", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "T2", group = "COWcode"),
             demean(NonDemocraciesConflict,select = "T3", group = "COWcode"))

NonDemocraciesConflict <- NDC

write.csv(NonDemocraciesConflict,"data-demeaned-with-T123.csv")