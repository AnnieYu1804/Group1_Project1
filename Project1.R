#Set up
install.packages("dplyr")
update.packages("diplyr")
library(dplyr)
install.packages("NHANES")
library(NHANES)
data(NHANES)
?NHANES
dim(NHANES) #n = 10,000
library(tidyverse)


#A. Data cleaning + Defining population
#Selecting adults >18
nhanes_adult <- subset(NHANES, NHANES$Age >= 18)
dim(nhanes_adult) #n = 7481
str(nhanes_adult)
colSums(is.na(nhanes_adult))
#Dropping cases with missing outcome variables
nhanes_adult1 <- subset(nhanes_adult, !is.na(nhanes_adult$BPSysAve))
dim(nhanes_adult1) #n = 7205
nhanes_adult1 <- subset(nhanes_adult1, !is.na(nhanes_adult1$BPDiaAve))
dim(nhanes_adult1) #n = 7205
summary(nhanes_adult1$BPSysAve)
summary(nhanes_adult1$BPDiaAve)
nhanes_adult1 <- subset(nhanes_adult1, nhanes_adult1$BPDiaAve >0)
dim(nhanes_adult1) #n = 7178
summary(nhanes_adult1$BPDiaAve)
#Selecting only non-pregnant adults
typeof(nhanes_adult$PregnantNow)
table(nhanes_adult$PregnantNow)
levels(nhanes_adult$PregnantNow)
nhanes_adult1 <- nhanes_adult1[!nhanes_adult1$PregnantNow %in% "Yes",]
dim(nhanes_adult1) #n = 7109
table(nhanes_adult1$PregnantNow)
#Exploring potential other exclusion criteria
colSums(is.na(nhanes_adult1))
typeof(nhanes_adult1$Diabetes)
levels(nhanes_adult1$Diabetes)
table(nhanes_adult1$Diabetes) #727/7135 are diabetic i.e. approx 10%
hist(nhanes_adult1$Age)
summary(nhanes_adult1$Age) #Oldest person in study is 80 years old

#B. Data analysis
#Table 1
nhanes_adult1$hypertensive <- nhanes_adult1$BPSysAve > 140 | nhanes_adult1$BPDiaAve > 90
install.packages("tableone")
library(tableone)
str(nhanes_adult1)
variables = c("Age", "BMI", "BPSysAve", "BPDiaAve", "Diabetes", "SleepHrsNight", "PhysActiveDays", 
              "Gender", "Race1", "Education", "MaritalStatus", "HHIncome", 
              "Depressed", "SleepTrouble", "PhysActive", "Alcohol12PlusYr", "SmokeNow", "Smoke100")
facvariables = c("Gender", "Race1", "Education", "MaritalStatus", "HHIncome", 
               "Depressed", "SleepTrouble", "PhysActive", "Alcohol12PlusYr", "SmokeNow", "Smoke100")
tab <- CreateTableOne(data = nhanes_adult1, vars = variables, factorVars = facvariables, 
                      strata = "hypertensive")
table <- print(tab)
write.csv(table, file = "Table1.csv")

#Data visualisation
hist(nhanes_adult1$BPSysAve)
hist(nhanes_adult1$BPDiaAve)

cor(nhanes_adult1$BPSysAve, nhanes_adult1$BPDiaAve)
qplot(nhanes_adult1$BPSysAve, nhanes_adult1$BPDiaAve)

hist(nhanes_adult1$BMI)
qplot(nhanes_adult1$BMI, nhanes_adult1$BPSysAve)
cor(!is.na(nhanes_adult1$BMI), nhanes_adult1$BPSysAve)
qplot(nhanes_adult1$BMI, nhanes_adult1$BPDiaAve)
cor(!is.na(nhanes_adult1$BMI), nhanes_adult1$BPDiaAve)

hist(nhanes_adult1$SleepHrsNight)
qplot(nhanes_adult1$SleepHrsNight, nhanes_adult1$BPSysAve)
cor(!is.na(nhanes_adult1$SleepHrsNight), nhanes_adult1$BPSysAve)
qplot(nhanes_adult1$SleepHrsNight, nhanes_adult1$BPDiaAve)
cor(!is.na(nhanes_adult1$SleepHrsNight), nhanes_adult1$BPDiaAve)

boxplot(nhanes_adult1$BPSysAve ~ nhanes_adult1$Smoke100)
boxplot(nhanes_adult1$BPDiaAve ~ nhanes_adult1$Smoke100)

hist(nhanes_adult$Age)
qplot(nhanes_adult1$Age, nhanes_adult1$BPSysAve)
qplot(nhanes_adult1$Age, nhanes_adult1$BPDiaAve)

#Descriptive analysis
mean(nhanes_adult1$BPSysAve) #120.8394
mean(nhanes_adult1$BPDiaAve) #70.24504

#Age and BP
mean(nhanes_adult1$BPSysAve[nhanes_adult1$Gender == "male"])
mean(nhanes_adult1$BPSysAve[nhanes_adult1$Gender == "female"])
t.test(nhanes_adult1$BPSysAve[nhanes_adult1$Gender == "male"], nhanes_adult1$BPSysAve[nhanes_adult1$Gender == "female"])

mean(nhanes_adult1$BPDiaAve[nhanes_adult1$Gender == "male"])
mean(nhanes_adult1$BPDiaAve[nhanes_adult1$Gender == "female"])
t.test(nhanes_adult1$BPDiaAve[nhanes_adult1$Gender == "male"], nhanes_adult1$BPSysAve[nhanes_adult1$Gender == "female"])

#BMI and BP
mean(nhanes_adult1$BPSysAve[nhanes_adult1$BMI > 30], na.rm = TRUE)
mean(nhanes_adult1$BPSysAve[nhanes_adult1$BMI <30], na.rm = TRUE)
t.test(nhanes_adult1$BPSysAve[nhanes_adult1$BMI > 30], nhanes_adult1$BPSysAve[nhanes_adult1$BMI <30])

mean(nhanes_adult1$BPDiaAve[nhanes_adult1$BMI > 30], na.rm = TRUE)
mean(nhanes_adult1$BPDiaAve[nhanes_adult1$BMI <30], na.rm = TRUE)
t.test(nhanes_adult1$BPDiaAve[nhanes_adult1$BMI > 30], nhanes_adult1$BPSysAve[nhanes_adult1$BMI <30])

#Race and BP
levels(nhanes_adult1$Race1)
black <- mean(nhanes_adult1$BPSysAve[nhanes_adult1$Race1 == "Black"], na.rm = TRUE)
hispanic <- mean(nhanes_adult1$BPSysAve[nhanes_adult1$Race1 == "Hispanic"], na.rm = TRUE)
mexican <- mean(nhanes_adult1$BPSysAve[nhanes_adult1$Race1 == "Mexican"], na.rm = TRUE)
white <- mean(nhanes_adult1$BPSysAve[nhanes_adult1$Race1 == "White"], na.rm = TRUE)
other <- mean(nhanes_adult1$BPSysAve[nhanes_adult1$Race1 == "Other"], na.rm = TRUE)
group_by(nhanes_adult1, nhanes_adult1$Race1) %>%
  summarise(
    count = n(),
    mean = mean(BPSysAve, na.rm = TRUE),
    sd = sd(BPSysAve, na.rm = TRUE)
  )
boxplot(nhanes_adult1$BPSysAve ~ nhanes_adult1$Race1)
res.aov <- aov(nhanes_adult1$BPSysAve ~ nhanes_adult1$Race1, data = nhanes_adult1)
summary(res.aov) #We can reject null hypothesis that means are the same

group_by(nhanes_adult1, nhanes_adult1$Race1) %>%
  summarise(
    count = n(),
    mean = mean(BPDiaAve, na.rm = TRUE),
    sd = sd(BPDiaAve, na.rm = TRUE)
  )
boxplot(nhanes_adult1$BPDiaAve ~ nhanes_adult1$Race1)
res.aov <- aov(nhanes_adult1$BPDiaAve ~ nhanes_adult1$Race1, data = nhanes_adult1)
summary(res.aov)

#Linear regression
#Univariate
model <- lm(nhanes_adult1$BPSysAve ~ nhanes_adult1$SleepHrsNight)
summary(model)
#Multivariate
model1 <- lm(nhanes_adult1$BPSysAve ~ nhanes_adult1$SleepHrsNight + nhanes_adult1$Age +
               nhanes_adult1$Gender)
summary(model1)

model2 <- lm(nhanes_adult1$BPSysAve ~ nhanes_adult1$SleepHrsNight + nhanes_adult1$BMI
               + nhanes_adult1$Age +
               nhanes_adult1$Gender + nhanes_adult1$Race1 + 
               nhanes_adult1$Education)
summary(model2)

#Stepwise regression
library(tidyverse)
install.packages("caret")
library(caret)
install.packages("leaps")
library(leaps)

model2 <- regsubsets(nhanes_adult1$BPSysAve ~ nhanes_adult1$SleepHrsNight + nhanes_adult1$Age +
                     nhanes_adult1$Gender, 
                     data = nhanes_adult1, nvmax = 2, nbest = 3, 
                     method = "seqrep")
summary(model2)
plot(model2, scale = "r2")
