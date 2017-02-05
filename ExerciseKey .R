##############################Exercise ###################################

##########################################################################
########################  Polynomial Regression ##########################
##########################################################################

#install.packages("boot")
#The data we use here give the speed of cars and the distances taken to stop. 
# Note that the data were recorded in the 1920s
rm(list=ls())
library(MASS)
library(ISLR)
library(ggplot2) 
require(boot)

# use ggplot to visualize the dataset "cars." 
p<-ggplot(cars,aes(x=dist,y=speed))+
  geom_point(shape=1,size=2)
p

# Prevent Overfitting
# Implement 10-fold cross-validation and apply a generalized linear model
# Find out MSE changes as the degree of the polynomial increases from 1 to 8
# set seed (3)
set.seed(3)
degree=1:8
cv.error=rep(0,8)
for(d in degree){
  glm.fit = glm(speed~poly(dist, d), data=cars)
  cv.error[d] = cv.glm(cars,glm.fit,K=5)$delta[1]
}
ggplot(data.frame(degree,cv.error),aes(x=degree,y=cv.error))+
  geom_line()+
  geom_point(size=2)+
  labs(x="Degree of Polynomial")+
  labs(y="MSE")

# Find out the best degree for the polynomial and its MSE
print(paste("Best d is",degree[which.min(cv.error)],"with lowest MSE of",cv.error[which.min(cv.error)]))

# Plot the best polynomial model
p+stat_smooth(method = 'lm', formula = y ~ poly(x,degree[which.min(cv.error)]), colour =c("red"),se= FALSE)

####################################################################
########################  Step Function  ###########################
####################################################################

#create a list of all of the ages in our data
distlims=range(cars$dist)
dist_grid = seq(from = min(distlims), to = max(distlims))

# split the distance range into three parts:0-23,23-50,50-85,85-120
table(cut(cars$dist,breaks=c(0,23,50,85)))
fit_step = lm(speed~cut(dist,breaks=c(0,23,50,85,120)), data = cars)
print(coef(summary(fit_step)))
preds = predict(fit_step, newdata = list(dist = dist_grid), se = TRUE)
ggplot() +
  geom_point(data=cars,aes(x=dist,y=speed),size=2,shape=1)+
  geom_line(aes(x = dist_grid, y = preds$fit), color = "red", size =1) 
