rm(list=ls()) #clear environment
#Set up
library(dplyr)
library(NHANES)
library(tidyverse)
?NHANES

#SUBSETTING
data(NHANES)
d <- subset(NHANES, NHANES$Age >= 18 & #exclude 18 or under
                         !NHANES$PregnantNow %in% "Yes" & #exclude pregnant
                         !is.na(NHANES$BPSysAve) & #missing outcomes
                         !is.na(NHANES$BPDiaAve) &
                         NHANES$BPDiaAve > 0 & NHANES$BPDiaAve < 120 & #0 Dia and Sys
                         NHANES$BPDia1 > 0 &
                         NHANES$BPDia2 > 0 &
                         NHANES$BPDia3 > 0 &
                         NHANES$BPSysAve > 0 & NHANES$BPSysAve < 180 &
                         !is.na(NHANES$BMI) & #missing exposures
                         !is.na(NHANES$SleepHrsNight) &
                         NHANES$BMI >= 18.5 & NHANES$BMI <= 40 &
                         NHANES$Diabetes == "No") #BMI exclusion) #diabetics 

#-----BMI
#Data analysis for SYSTOLIC

#Table 1
d$hypertensive <- d$BPSysAve > 140 | d$BPDiaAve > 90
library(tableone)
variables = c("Age", "BMI", "BPSysAve", "BPDiaAve", "Diabetes", "SleepHrsNight", "PhysActiveDays", 
              "Gender", "Race1", "Education", "MaritalStatus", "HHIncome", 
              "Depressed", "SleepTrouble", "PhysActive", "Alcohol12PlusYr", "SmokeNow", "Smoke100")
facvariables = c("Gender", "Race1", "Education", "MaritalStatus", "HHIncome", 
               "Depressed", "SleepTrouble", "PhysActive", "Alcohol12PlusYr", "SmokeNow", "Smoke100")
tab <- CreateTableOne(data = d, vars = variables, factorVars = facvariables, 
                      strata = "hypertensive")
table <- print(tab)
write.csv(table, file = "Table1.csv")

#BMI
qplot(d$BMI, d$BPSysAve)
qplot(d$BMI, d$BPDiaAve)
cor(!is.na(d$BMI), d$BPDiaAve)

ggplot(d, aes(x = BMI,
                          y = BPSysAve, 
                          color = Age,)) + geom_point()

ggplot(d, aes(x = BMI,
                                   y = BPDiaAve, 
                                   color = Age,)) + geom_point()

ggplot(d, aes(x = BMI,
                          y = BPSysAve, 
                          color = Gender,)) + geom_point()

ggplot(d, aes(x = BMI,
                                   y = BPDiaAve, 
                                   color = Gender,)) + geom_point()


#BMI Linear models
m1 <- glm(BPSysAve ~ BMI, data=d)
m2 <- glm(BPSysAve ~ BMI + Age + Gender, data=d)
m3 <- glm(BPSysAve ~ BMI + Age + Gender + PhysActive, data=d)
m4 <- glm(BPSysAve ~ BMI + Age + Gender + PhysActive + Race1, data=d)
m5 <- glm(BPSysAve ~ BMI + Age + Gender + Race1 + MaritalStatus, data=d)
m6 <- glm(BPSysAve ~ BMI + Age + Gender + PhysActive + Race1 + HHIncomeMid + SleepHrsNight + Smoke100, data=d)
summary(m1)
summary(m2)
summary(m3)
summary(m4) #diff between 3 and 4 are small
summary(m5)
summary(m6) # jump from 47357 to ~43000
plot(m1)

#Table output
install.packages("jtools")
install.packages("huxtable")
library(huxtable)
library(jtools)
export_summs(m2, m4, m6, scale = TRUE)

#Analysis for DIASTOLIC

#regression models
md1 <- glm(BPDiaAve ~ BMI, data=d)
md2 <- glm(BPDiaAve ~ BMI + Age + Gender, data=d)
md3 <- glm(BPDiaAve ~ BMI + Age + Gender + PhysActive, data=d)
md4 <- glm(BPDiaAve ~ BMI + Age + Gender + PhysActive + Race1, data=d)
md5 <- glm(BPDiaAve ~ BMI + Age + Gender + PhysActive + Race1 + Education, data=d)
md6 <- glm(BPDiaAve ~ BMI + Age + Gender + PhysActive + Race1 + HHIncomeMid + SleepHrsNight, data=d)
summary(md1)
summary(md2)
summary(md3)
summary(md4) #diff between 3 and 4 are small
summary(md5)
summary(md6) # jump from 47357 to ~43000
plot(md6)

#Table output
install.packages("jtools")
install.packages("huxtable")
library(huxtable)
library(jtools)
export_summs(md2, md4, md6, scale = TRUE)
#-----SLEEP
#Data analysis

#BINARYSleep 
#make binary variable 
d$SleepDeprived5hrs <- ifelse(d$SleepHrsNight <=5, 1,0)
table(d$SleepDeprived5hrs) #645 that are sleep deprived (5 or less hrs)

d$SleepDeprived7hrs <- ifelse(d$SleepHrsNight <=7, 1,0)
table(d$SleepDeprived7hrs) #3577 that are sleep deprived (7 or less hrs)

#regression
mm1 <- glm(BPSysAve ~ SleepDeprived5hrs, data=d)
mm2 <- glm(BPSysAve ~ SleepDeprived5hrs + Age + Gender, data=d)
mm3 <- glm(BPSysAve ~ SleepDeprived5hrs + Age + Gender + PhysActive, data=d)
mm4 <- glm(BPSysAve ~ SleepDeprived5hrs + Age + Gender + PhysActive + Race1, data=d)
mm5 <- glm(BPSysAve ~ SleepDeprived5hrs + Age + Gender + PhysActive + Race1 + Education, data=d)
mm6 <- lm(BPSysAve ~ SleepDeprived5hrs + Age + Gender + PhysActive + Race1 + HHIncomeMid + BMI +Diabetes, data=d)
summary(mm1)
summary(mm2)
summary(mm3)
summary(mm4) #diff between 3 and 4 are small
summary(mm5)
summary(mm6) # jump from 47357 to ~43000
plot(mm1)

#Table output
library(huxtable)
library(jtools)
export_summs(mm2, mm4, mm6, scale = TRUE)
