##############################Exercise ###################################

##########################################################################
########################  Polynomial Regression ##########################
##########################################################################

#install.packages("boot")
#The data we use here gives the speed of cars and the distances taken to stop. 
# Note that the data were recorded in the 1920s
rm(list=ls())
library(MASS)
library(ISLR)
library(ggplot2) 
require(boot)

# use ggplot to visualize the dataset "cars" with dist on the x axis and speed on the y axis. 


# Prevent Overfitting
# Implement 10-fold cross-validation and apply a generalized linear model
# Find the degree of the polynomial with the lowest MSE
# (use polynomials with degree 1 to 8)
# set the seed to 3

# plot the cross validation errors

# Find out the best degree of polynomial and its MSE

# Plot the best polynomial model


####################################################################
########################  Step Function  ###########################
####################################################################

# create a list of all of the distances in our data

# split the distance range into three parts:0-23,23-50,50-85,85-120

# use lm and predict to create the step function model

# plot your results

