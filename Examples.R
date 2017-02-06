rm(list=ls())
############################
## Polynomial Regression ##
############################

#Use "Wage" data to show how easily non-linear 
#fitting procedures can be implemented in R

library(ISLR)
attach(Wage)

#Fit the model - very similar syntax to what we're used to
#Notice "~poly" command
#Model can be fit many ways in R, but this is most parsimonious
#See pages 288-89 for 3 variations of code that give same result
#4 denotes fourth degree polynomial

fit=lm(wage~poly(age,4), data=Wage)
coef(summary(fit))

#Create a grid of values for age at which we want predictions

agelims=range(age)
age.grid=seq(from=agelims[1], to=agelims[2])
preds=predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands=cbind(preds$fit+2*preds$se, preds$fit-2*preds$se.fit)

#Plot the regression. Recall mar and oma (for margins), lwd and col (for line width and color)

par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(age,wage,xlim=agelims, cex=.5, col="darkgrey")
title("Degree-4 Polynomial Example", outer=TRUE)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

#Discover what degree polynomial model best describes relationship between wage and age
#Start by fitting linear to degree 5 polynomial models
#Use ANOVA to compare simpler vs. more complex model in front of it
#Draw conclusions from P -values

fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)


#Alternative method: poly creates orthogonal polynomials

coef(summary(fit.5))

#However, ANOVA can always be used

#Predict wage over 250k
#Continue as earlier, except use GLM and family=binomial
#Allows for polynomial
#Using I creates a binary wrapper variable for under/over 250k

fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial)

#Use predict() to make predictions

preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))

#Create plot
#Jitter is used so age observations with same value do not cover each other

plot(age,I(wage>250),xlim=agelims, type="n",ylim=c(0,.2))
points(jitter(age),I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

########################
#### Step Functions ####
########################

par(mfrow=c(1,1))
#create a list of all of the ages in our data
age_grid = seq(from = min(agelims), to = max(agelims))
#cut() splits the age range into four equal parts
table(cut(Wage$age,4))
#now we can fit a constant to each group
fit_step = lm(wage~cut(age,4), data = Wage)
print(coef(summary(fit_step)))
# Predict the value of the generated ages, returning the standard error using se=TRUE
preds = predict(fit_step, newdata = list(age = age_grid), se = TRUE)
# Compute error bands (2*SE)
se_bands = cbind("upper" = preds$fit+2*preds$se.fit, "lower" = preds$fit-2*preds$se.fit)

# Plot the step function and error bands
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = preds$fit), color = "#0000FF", size = 2) +
  geom_ribbon(aes(x = age_grid, ymin = se_bands[,"lower"], ymax = se_bands[,"upper"]), alpha = 0.3) +
  xlim(agelims) +
  labs(title = "Step Function")

#Now suppose we wanted to cut the data into our own categories
#we can do this with the "breaks" option
table(cut(Wage$age,breaks=c(0,20,30,40,60,80)))
fit_step = lm(wage~cut(age,breaks=c(0,20,30,40,60,80)), data = Wage)
print(coef(summary(fit_step)))
preds = predict(fit_step, newdata = list(age = age_grid), se = TRUE)
se_bands = cbind("upper" = preds$fit+2*preds$se.fit, "lower" = preds$fit-2*preds$se.fit)
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = preds$fit), color = "#0000FF", size =2) +
  geom_ribbon(aes(x = age_grid, ymin = se_bands[,"lower"], ymax = se_bands[,"upper"]), alpha = 0.3) +
  xlim(agelims) +
  labs(title = "Step Function")


#try with more cuts
table(cut(Wage$age,10))
fit_step = lm(wage~cut(age,10), data = Wage)
print(coef(summary(fit_step)))
preds = predict(fit_step, newdata = list(age = age_grid), se = TRUE)
se_bands = cbind("upper" = preds$fit+2*preds$se.fit, "lower" = preds$fit-2*preds$se.fit)
ggplot() +
  geom_point(data = Wage, aes(x = age, y = wage)) +
  geom_line(aes(x = age_grid, y = preds$fit), color = "#0000FF", size= 2) +
  geom_ribbon(aes(x = age_grid, ymin = se_bands[,"lower"], ymax = se_bands[,"upper"]), alpha = 0.3) +
  xlim(agelims) +
  labs(title = "Step Function")

detach(Wage)
