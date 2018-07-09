###############################################################################################################################################################
                                                                    #LOAD DATA AND BASIC PLOTS
library(dplyr)
library(tidyr)
library(rms)
library(ggplot2)
library(corrplot)

library(coin) #test
library(DescTools)

library("sandwich")
library("pscl") # for zero-inflated model


#Load Data
setwd('C:/Users/Groniu/Desktop/Data science I rok/II semestr/Advanced Econometrics/Model')
data<-read.csv('student-mat.csv',sep=';')


#Preparing data
data <- subset(data, select = -c(G1, G2))
sum(is.na(data))
str(data)
table(data$G3)


#Change for categorical
data$famrel <- as.factor(data$famrel)
data$freetime <- as.factor(data$freetime)
data$goout <- as.factor(data$goout)
data$Dalc <- as.factor(data$Dalc)
data$Walc <- as.factor(data$Walc)
data$health <- as.factor(data$health)
data$Medu <- as.factor(data$Medu)
data$Fedu <- as.factor(data$Fedu)
data$traveltime <- as.factor(data$traveltime)
data$studytime <- as.factor(data$studytime)


#Histograms for numerical
par(mfrow=c(1,3))
hist(data$age,main='age',xlab='',ylab='')
#hist(log(data$age))
hist(data$failures,main='failures',xlab='',ylab='')
# hist(log(data$failures))
hist(data$absences,main='absences',xlab='',ylab='')
# hist(log(data$absences))
table(data$failures,data$absences)

###############################################################################################################################################################
                                                                            #PLOTS
#Boxplots
boxplot(data$age)
boxplot(data$failures)
boxplot(data$absences)

#Categorical plots
GetPlotCategorical<-function(x){
  plot(data[,x],main=x)
}

par(mfrow=c(4,4))
GetPlotCategorical('school')
GetPlotCategorical('sex')
GetPlotCategorical('address')
GetPlotCategorical('famsize')
GetPlotCategorical('Pstatus')
GetPlotCategorical('Medu')
GetPlotCategorical('Fedu')
GetPlotCategorical('Mjob')
GetPlotCategorical('Fjob')
GetPlotCategorical('reason')
GetPlotCategorical('guardian')
GetPlotCategorical('traveltime')
GetPlotCategorical('studytime')
GetPlotCategorical('schoolsup')
GetPlotCategorical('famsup')
GetPlotCategorical('paid')

par(mfrow=c(3,4))
GetPlotCategorical('activities')
GetPlotCategorical('nursery')
GetPlotCategorical('higher')
GetPlotCategorical('internet')
GetPlotCategorical('romantic')
GetPlotCategorical('famrel')
GetPlotCategorical('freetime')
GetPlotCategorical('goout')
GetPlotCategorical('Dalc')
GetPlotCategorical('Walc')
GetPlotCategorical('health')
GetPlotCategorical('G3')

###############################################################################################################################################################
                                                                          #CORRELATION
numericaldata<-data[,c('age','failures','absences','G3')]
M<-cor(numericaldata)
corrplot(M, method="circle")

###############################################################################################################################################################
                                                                      #CREATE BINARY COLUMNS
data$Medu_primary = 0
data$Medu_primary[data$Medu == 1] = 1
data$Medu_5thto9th = 0
data$Medu_5thto9th[data$Medu == 2] = 1
data$Medu_secondary = 0
data$Medu_secondary[data$Medu == 3] = 1
data$Medu_higher = 0
data$Medu_higher[data$Medu == 4] = 1
data$Fedu_primary = 0
data$Fedu_primary[data$Fedu == 1] = 1
data$Fedu_5thto9th = 0
data$Fedu_5thto9th[data$Fedu == 2] = 1
data$Fedu_secondary = 0
data$Fedu_secondary[data$Fedu == 3] = 1
data$Fedu_higher = 0
data$Fedu_higher[data$Fedu == 4] = 1

data$Mjob_at_home = 0
data$Mjob_at_home[data$Mjob == "at_home"] = 1
data$Mjob_health = 0
data$Mjob_health[data$Mjob == "health"] = 1
data$Mjob_services = 0
data$Mjob_services[data$Mjob == "services"] = 1
data$Mjob_teacher = 0
data$Mjob_teacher[data$Mjob == "teacher"] = 1
data$Fjob_at_home = 0
data$Fjob_at_home[data$Fjob == "at_home"] = 1
data$Fjob_health = 0
data$Fjob_health[data$Fjob == "health"] = 1
data$Fjob_services = 0
data$Fjob_services[data$Fjob == "services"] = 1
data$Fjob_teacher = 0
data$Fjob_teacher[data$Fjob == "teacher"] = 1

data$reason_home = 0
data$reason_home[data$reason == "home" ] = 1
data$reason_course = 0
data$reason_course[data$reason == "course" ] = 1
data$reason_reputation = 0
data$reason_reputation[data$reason == "reputation" ] = 1

data$guardian_father = 0
data$guardian_father[data$guardian == "father"] = 1
data$guardian_mother = 0
data$guardian_mother[data$guardian == "mother"] = 1

data$traveltime_2 = 0
data$traveltime_2[data$traveltime == 2] = 1
data$traveltime_3 = 0
data$traveltime_3[data$traveltime == 3] = 1
data$traveltime_4 = 0
data$traveltime_4[data$traveltime == 4] = 1

data$studytime_2 = 0
data$studytime_2[data$studytime == 2] = 1
data$studytime_3 = 0
data$studytime_3[data$studytime == 3] = 1
data$studytime_4 = 0
data$studytime_4[data$studytime == 4] = 1

data$famrel_2 = 0
data$famrel_2[data$famrel == 2] = 1
data$famrel_3 = 0
data$famrel_3[data$famrel == 3] = 1
data$famrel_4 = 0
data$famrel_4[data$famrel == 4] = 1
data$famrel_5 = 0
data$famrel_5[data$famrel == 5] = 1

data$freetime_2 = 0
data$freetime_2[data$freetime == 2] = 1
data$freetime_3 = 0
data$freetime_3[data$freetime == 3] = 1
data$freetime_4 = 0
data$freetime_4[data$freetime == 4] = 1
data$freetime_5 = 0
data$freetime_5[data$freetime == 5] = 1

data$goout_2 = 0
data$goout_2[data$goout == 2] = 1
data$goout_3 = 0
data$goout_3[data$goout == 3] = 1
data$goout_4 = 0
data$goout_4[data$goout == 4] = 1
data$goout_5 = 0
data$goout_5[data$goout == 5] = 1

data$Dalc_2 = 0
data$Dalc_2[data$Dalc == 2] = 1
data$Dalc_3 = 0
data$Dalc_3[data$Dalc == 3] = 1
data$Dalc_4 = 0
data$Dalc_4[data$Dalc == 4] = 1
data$Dalc_5 = 0
data$Dalc_5[data$Dalc == 5] = 1

data$Walc_2 = 0
data$Walc_2[data$Walc == 2] = 1
data$Walc_3 = 0
data$Walc_3[data$Walc == 3] = 1
data$Walc_4 = 0
data$Walc_4[data$Walc == 4] = 1
data$Walc_5 = 0
data$Walc_5[data$Walc == 5] = 1

data$health_2 = 0
data$health_2[data$health == 2] = 1
data$health_3 = 0
data$health_3[data$health == 3] = 1
data$health_4 = 0
data$health_4[data$health == 4] = 1
data$health_5 = 0
data$health_5[data$health == 5] = 1

#################################################################################################################################################
                                                             #CHECK VARIANCE AND MEAN
summary(data$G3)
var(data$G3)
mean(data$G3)

par(mfrow=c(3,1))
hist(data$G3, breaks = 0:80)
hist(rpois(n=649, lambda=11.90601), breaks = 0:80) 
hist(rnegbin(n=649, mu=11.90601,theta=1.2), breaks = 0:80 -0.5)

names(data)

#################################################################################################################################################
                                                                    #REGRESSION
### Poisson Model

model_poisson <- glm(G3 ~ school + sex + log(age) + address + famsize + Pstatus + as.factor(Medu)+as.factor(Fedu)+as.factor(Mjob)+
                       as.factor(Fjob) + as.factor(reason) + as.factor(guardian) + as.factor(traveltime) + as.factor(studytime) + failures + schoolsup+ famsup + paid +
                       activities + nursery + higher + internet + romantic + as.factor(famrel) + as.factor(freetime) + as.factor(goout) +
                       as.factor(Dalc) + as.factor(Walc) + as.factor(health) + absences, data = data, family = poisson)
summary(model_poisson)



### Negative-binomial

model_negative <- MASS::glm.nb(G3 ~ school + sex + age + address + famsize + Pstatus + as.factor(Medu)+as.factor(Fedu)+as.factor(Mjob)+
                                 as.factor(Fjob) + as.factor(reason) + as.factor(guardian) + as.factor(traveltime) + as.factor(studytime) + failures + schoolsup+ famsup + paid +
                                 activities + nursery + higher + internet + romantic + as.factor(famrel) + as.factor(freetime) + as.factor(goout) +
                                 as.factor(Dalc) + as.factor(Walc) + as.factor(health) + absences, data = data)
summary(model_negative)

model_poisson$aic
model_negative$aic


### Zero-Inflated

model_zero <- zeroinfl(G3 ~ school + sex + age + address + famsize + Pstatus + as.factor(Medu)+as.factor(Fedu)+as.factor(Mjob)+
                         as.factor(Fjob) + as.factor(reason) + as.factor(guardian) + as.factor(traveltime) + as.factor(studytime) + failures + schoolsup+ famsup + paid +
                         activities + nursery + higher + internet + romantic + as.factor(famrel) + as.factor(freetime) + as.factor(goout) +
                         as.factor(Dalc) + as.factor(Walc) + as.factor(health) + absences, data = data, dist = "negbin")

summary(model_zero)

#################################################################################################################################################
                                                                  #CHOOSING MODEL
# First, let's check over-dispersion

# dispersion parameter (1 - geometric distirbution, infinity - Poisson distirbution)

model_negative$theta # 14.782 

# Let's make likelihood ratio test (H0: theta = infinity, H1: theta < infinity). 
# If H0 can not be rejected, we should choose Poisson Regression, if H0 should be rejected we should choose negative-binomial model

options(scipen=100)
pchisq(2*(logLik(model_negative) - logLik(model_poisson)), df = 1, lower.tail = FALSE)

# Result: 'log Lik.' 0.000000009154813 (df=69)
# So we rejected H0, there is over-dispersion and we will use negative-binomial model

################ Additional check
as.numeric(2*(logLik(model_negative) - logLik(model_poisson)))
# z tego równanka wychodzi 33.01293, więc negative lepszy od poissona (https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/)
###############

1 - pchisq(summary(model_poisson)$deviance, summary(model_poisson)$df.residual)

# Check whether there is excess zeros in our dependent variable
barplot(table(data$G3))



#################################################################################################################################################
                                                                        #ANOVA TESTS
# our new binary variables are in dataset as numeric variables, so let's change them into factors 
as.factor(data$Medu_primary)
as.factor(data$studytime_3)
as.factor(data$freetime_5)
as.factor(data$Walc_5)

### Simplified model with only significant variables from general model
model_negative_sig <- MASS::glm.nb(G3 ~ sex + Medu_primary + studytime_3 + failures + romantic + freetime_5 + Walc_5 + absences, data = data)
### ANOVA for general and simplified negative-binomial models to check whether rejected variables are jointly insignificant
anova(model_negative, model_negative_sig) # p-value 
summary(model_negative_sig)

#we see that we have to perform another anova test
model_negative_sig2 <- MASS::glm.nb(G3 ~ sex  + studytime_3 + failures + romantic + absences, data = data)
anova(model_negative_sig,model_negative_sig2)

#Our Final Model has form
summary(model_negative_sig2)

#################################################################################################################################################
                                                            #ADD INTERACTION AND TRANSFORMED VARIABLE
model_negative_try <-  MASS::glm.nb(G3 ~ sex  + studytime_3 + failures + romantic + absences+sex*romantic+log(age), data = data)
summary(model_negative_try)
model_negative_try <-  MASS::glm.nb(G3 ~ sex  + studytime_3 + failures + romantic + absences+failures*Dalc, data = data)
summary(model_negative_try)
#adding transformed variable and interaction to model does not help much

#################################################################################################################################################
                                                                    #PSEUDO R2 STATISTICS
PseudoR2(model_negative_sig2)

#################################################################################################################################################
                                                                        #LINK TEST

data$yhat <- model_negative_sig2$fitted.values
data$yhat2 <- data$yhat^2
aux.reg= MASS::glm.nb(G3~  yhat + yhat2, data = data)
summary(aux.reg)


#################################################################################################################################################
### Comparison of 3 general models and final model:



sjt.lm(model_poisson, model_negative, model_negative_sig, depvar.labels = c("General: Poisson", "General: negative-binomial", "Simpler: negative-binomial"), string.est = "Estimate", 
       string.ci = "Conf. Int.", string.p = "p-value",separate.ci.col = FALSE,CSS = list(css.separatorrow = 'padding-top:0.5em; padding-bottom:0.5em;'))
