#missing data free dataset 
#missing data was averaged over the stratum, as the controls are the closest match
fibro <- read.table(file.choose(), header=TRUE, stringsAsFactors=FALSE)
head(fibro)
fibro

#packages needed
install.packages("plyr")
library(plyr)
#install.packages("survival")
library(survival)
install.packages("car")
library(car)

rename(fibro, c("STR"="Stratum", "OBS"="Observation", "AGMT"="Age", "FNDX"="Diagnosed", "HIGD"="Grade",
                "DEG"="Degree", "CHK"="MedicalCheckups", "AGP1"="AgePregnant", "AGMN"="AgeMenarche",
                "NLV"="Miscarriages", "LIV"="Births", "WT"="Weight", "AGLP"="AgeLastPeriod", "MST"="MaritalStatus"))

#bring in variables without using attach()
#use as.numeric as needed for the fields that has missing values
#fields with missing values at any point are strings, need numeric
Stratum <- fibro$STR
Observation <- fibro$OBS
Age <- fibro$AGMT
Diagnosed <- fibro$FNDX
Grade <- fibro$HIGD
Degree <- fibro$DEG
MedicalCheckups <- fibro$CHK
AgePregnant <- as.numeric(fibro$AGP1)
AgeMenarche <- fibro$AGMN
Miscarriages <- as.numeric(fibro$NLV) 
Births <- as.numeric(fibro$LIV)
Weight <- fibro$WT
AgeLastPeriod <- fibro$AGLP 
MaritalStatus <- fibro$MST

#Hosmer 7 steps
#1-run each var independently and check sig
  #dont run stratum, age, observation

gradecond=clogit(Diagnosed~Grade + strata(Stratum), method="exact", data=fibro)
summary(gradecond)

degreecond=clogit(Diagnosed~Degree + strata(Stratum), method="exact", data=fibro)
summary(degreecond)

medchkcond=clogit(Diagnosed~MedicalCheckups + strata(Stratum), method="exact", data=fibro)
summary(medchkcond)

agepregcond=clogit(Diagnosed~AgePregnant + strata(Stratum), method="exact", data=fibro)
summary(agepregcond)

agemencond=clogit(Diagnosed~AgeMenarche + strata(Stratum), method="exact", data=fibro)
summary(agemencond)

miscond=clogit(Diagnosed~Miscarriages + strata(Stratum), method="exact", data=fibro)
summary(miscond)

birthcond=clogit(Diagnosed~Births + strata(Stratum), method="exact", data=fibro)
summary(birthcond)

weightcond=clogit(Diagnosed~Weight + strata(Stratum), method="exact", data=fibro)
summary(weightcond)

alpcond=clogit(Diagnosed~AgeLastPeriod + strata(Stratum), method="exact", data=fibro)
summary(alpcond)

maritalcond=clogit(Diagnosed~MaritalStatus + strata(Stratum), method="exact", data=fibro)
summary(maritalcond)


#change to ever never for marriage and save to dataset
fibro<-mutate(fibro, EverNever = ifelse(fibro$MST > 4, 1, 0))


EverNever<-fibro$EverNever

evernevercond=clogit(Diagnosed~EverNever+ strata(Stratum), method="exact", data=fibro)
summary(evernevercond)

#recode the degrees
#Deg0=none
#Deg1=High School
#Deg2=Jr. College
#Deg3=College
#Deg4=Graduate
fibro<-mutate(fibro, Deg0=ifelse(fibro$DEG==0, 1, 0), Deg1=ifelse(fibro$DEG==1, 1, 0), Deg2=ifelse(fibro$DEG==2, 1, 0), Deg3=ifelse(fibro$DEG==3, 1, 0), Deg4=ifelse(fibro$DEG==4, 1, 0))

Deg0<-fibro$Deg0
Deg1<-fibro$Deg1
Deg2<-fibro$Deg2
Deg3<-fibro$Deg3
Deg4<-fibro$Deg4

Deg0cond=clogit(Diagnosed~Deg0+ strata(Stratum), method="exact", data=fibro)
summary(Deg0cond)

Deg1cond=clogit(Diagnosed~Deg1+ strata(Stratum), method="exact", data=fibro)
summary(Deg1cond)

Deg2cond=clogit(Diagnosed~Deg2+ strata(Stratum), method="exact", data=fibro)
summary(Deg2cond)

Deg3cond=clogit(Diagnosed~Deg3+ strata(Stratum), method="exact", data=fibro)
summary(Deg3cond)

Deg4cond=clogit(Diagnosed~Deg4+ strata(Stratum), method="exact", data=fibro)
summary(Deg4cond)


#2 - throw in vars that are significant at the alpha=25% level
#throw out: marital status (not evernever), #live births, miscarriages, 
#keep: weight, aglp,  agemenarche, agepregnant, medicalcheckups, degree4, higd, evernever

step2cond1=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + AgePregnant + AgeLastPeriod +  Deg4 + Degree + strata(Stratum), method="exact", data=fibro)
summary(step2cond1)

step2cond2=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + AgePregnant + strata(Stratum), method="exact", data=fibro)
summary(step2cond2)

step2cond3=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + AgeLastPeriod + strata(Stratum), method="exact", data=fibro)
summary(step2cond3)

step2cond4=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + Deg4 + strata(Stratum), method="exact", data=fibro)
summary(step2cond4)

step2cond5=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + Degree + strata(Stratum), method="exact", data=fibro)
summary(step2cond5)

step2cond=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + AgePregnant + strata(Stratum), method="exact", data=fibro)
summary(step2cond)

step2bcond=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + strata(Stratum), method="exact", data=fibro)
summary(step2bcond)

#checking the covariates left out to see if there is a 20% change
checkagp=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + AgePregnant + strata(Stratum), method="exact", data=fibro)
summary(checkagp)
coef(step2bcond)

checkaglp=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + AgeLastPeriod + strata(Stratum), method="exact", data=fibro)
summary(checkaglp)
coef(step2bcond)

checkdeg4=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + Deg4 + strata(Stratum), method="exact", data=fibro)
summary(checkdeg4)
coef(step2bcond)

checkdegree=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + Degree + strata(Stratum), method="exact", data=fibro)
summary(checkdegree)
coef(step2bcond)

#correlations
cor(fibro)

#checking rest of var (birth related)
checkbirth=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + Births + strata(Stratum), method="exact", data=fibro)
summary(checkbirth)
coef(step2bcond)

checkmisc=clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + Miscarriages + strata(Stratum), method="exact", data=fibro)
summary(checkmisc)
coef(step2bcond)

maineffects<-clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + strata(Stratum), method="exact", data=fibro)
summary(maineffects)

maineffects2<-clogit(Diagnosed~Weight**2 + AgeMenarche + MedicalCheckups + EverNever + strata(Stratum), method="exact", data=fibro)
summary(maineffects2)

#checking more
coef(checkmisc)/coef(step2bcond)
coef(checkbirth)/coef(step2bcond)

coef(step2bcond)/coef(step2cond1)
coef(step2bcond)/coef(step2cond2)
coef(step2bcond)/coef(step2cond3)
coef(step2bcond)/coef(step2cond4)
coef(step2bcond)/coef(step2cond5)

#no changes greater than 20%

#matchit with evernever
install.packages("MatchIt")
library(MatchIt)

matchWT<-matchit(EverNever~AgeMenarche + MedicalCheckups + Weight, data=fibro, method="nearest", distance="logit", discard="none", reestimate = FALSE)
wt.data<-match.data(matchWT)
wtcond<-clogit(wt.data$FNDX~wt.data$EverNever, method="exact", data=wt.data)
summary(wtcond)

plot(matchWT)




#playing with plotting
range(Weight)
range(AgeMenarche)
range(MedicalCheckups)

mainwt<-clogit(Diagnosed~Weight, method="exact", data=fibro)

plot(Weight, Diagnosed, xlab="Weight", ylab="FBD")

curve(predict(mainwt, data.frame(Weight=x), type="risk"), add=TRUE)

plot(residuals(maineffects, "deviance"), xlab="Main Effects Model", ylab="Residuals")

#checking histograms
par(mfrow=c(2,2))
hist(Weight)
hist(AgeMenarche)
hist(MedicalCheckups)
hist(EverNever)


plot(Observation, AgeMenarche)
plot(Observation, Weight)
plot(Observation, Degree)


#checking density of Continuous Vars
par(mfrow=c(2,1))
densityPlot(Weight)
densityPlot(AgeMenarche)

#quartiles midpoints - linear check of weight and weight**2
par(mfrow=c(2,1))

x<-c(100.75, 129.5, 147.25, 218.5) 
#estimates of coef
y<-c(0, 0.591, 1.832, 13.545)
plot(x,y)
lines(x,y)

x2<-c(10582.25, 16835.5, 21778.25, 51525)
y2<-c(0, -0.527, 0.6052, 2.606)
plot(x2, y2)
lines(x2, y2)


install.packages("ggplot2")
library(ggplot2)

fibro<-mutate(fibro, EverBirth = ifelse(fibro$LIV > 0, 0, 1))

maineffects3<-clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + EverBirth + strata(Stratum), method="exact", data=fibro)
summary(maineffects3)

prelimfinalmodel<-clogit(Diagnosed~Weight + AgeMenarche + MedicalCheckups + EverNever + AgeMenarche*AgeLastPeriod + strata(Stratum), method="exact", data=fibro)
summary(prelimfinalmodel)

#switching to SAS PROC LOGISTIC as it is more well fit for this analysis than R


