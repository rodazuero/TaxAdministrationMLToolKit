

# Machine Learning for All: Tree based model
# Class 4

# Date: 2019-02-15
# Author: Anastasiya Yarygina

# Data used: 

# 1. Boston Housing, Kaggle, R-MASS 
# 2. California Housing Prices, Kaggle 
# 3. IRIS, R



# 0.start----------------------------------------------
# Set up working space
# are you in the right working directory? 
getwd()

# set wd, if needed
# you can do that by navigating the "Files" tab in the bottom-right panel of RStudio
# and the pressing "More" -> "Set As Wroking Directory"
# or, you can excute the command setwd
# setwd ("YOUR WORKING DIRECTORY")

# Clear workig space
rm(list = ls())

# 0.end------------------------------------------------




# I. Tree models, regression problem (continuos response variable)

# 1.start----------------------------------------------
# Fit one tree model to California Housing data
if (!require("tree")) install.packages("tree") # install package if necessary
library(tree) # load package
ca = read.csv("CAhousing.csv") # read data
tree.model <- tree(log(medianHouseValue) ~ longitude + latitude, data=ca) # fit single tree model
plot(tree.model,type= "uniform") # plot tree
text(tree.model, label=c("yval"), cex=1)
# 1.end------------------------------------------------





# 2.start----------------------------------------------
# Look at the data partitions overlayed on housing prices
price.deciles <- quantile(ca$medianHouseValue, 0:10/10)
cut.prices    <- cut(ca$medianHouseValue, price.deciles, include.lowest=TRUE)
plot(ca$longitude, ca$latitude, col=grey(10:2/11)[cut.prices], pch=20, 
     xlab="Longitude",ylab="Latitude")
partition.tree(tree.model, ordvars=c("longitude","latitude"), add=TRUE)
# daker dots on the plot represent more expencive properties
# 2.end------------------------------------------------




# 3.start----------------------------------------------
# load and have a quick look at Boston data
if (!require("MASS")) install.packages("MASS") # install MASS package if not available
if (!require("tree")) install.packages("tree") # install tree package if not available
library(MASS) # load package that contains the Boston Housing Data
library(tree) # load package to estimate the tree model

str(Boston) # Have a quick look at the data

attach(Boston) # Make Boston variables directly accesible

# 3.end------------------------------------------------




# 4.start----------------------------------------------
# First, get a big tree using a small value of mindev
temp = tree(medv~lstat, data=Boston, mindev=.001)

# mindev is a minimum improvement in the model fit
# smaller mindev -> bigger tree

# plot the big tree
plot(temp,type= "uniform") # uniform makes branches of the same size
text(temp, label=c("yval"), cex=0.8)
# if the tree is too small, make mindev smaller.

summary(temp) # just a summary
?tree

# to see the size of the tree: 
cat("The big tree size is: ",length(unique(temp$where)), "\n")

# 4.end------------------------------------------------



# 5.start----------------------------------------------
# use Cross Validation to know how much should we prun the tree
cvpst <- cv.tree(temp, K=10) # K is the number of folds
cvpst$size # the list of tree sizes
cvpst$dev  # the list of deviances (SSE) for each tree size


plot(cvpst)
# 5.end------------------------------------------------



# 6.start----------------------------------------------
# we prune the tree down to 7 paths 
boston.tree=prune.tree(temp,best=7) 
# Plot the prunned tree
plot(boston.tree,type= "uniform" ) # uniform makes branches of the same size 
text(boston.tree,col="blue",label=c("yval"),cex=.8)

# what is the size of the prunned tree? 
cat("the size of the prunned tree is", length(unique(boston.tree$where)),"\n")
# 6.end------------------------------------------------



# 7.start----------------------------------------------
# plot tree and linear model fits
plot(lstat,medv,cex=.5,pch=16) # plot Y and X (predictor and response)

boston.fit = predict(boston.tree) #get fitted values of the tree model
oo=order(lstat) # order by lstat
lines(lstat[oo],boston.fit[oo],col="red",lwd=3) # plot tree model fit

abline(lm(medv~lstat, data=Boston), col="blue") # add linear fit
legend("topright", legend=c("Tree Model fit", "Linear Model fit"),
       col=c("red", "blue"), lty=1)
# 7.end------------------------------------------------





# II. Practice: Regression tree models using California Housing Prices dataset

# 8.start----------------------------------------------
ca = read.csv("CAhousing.csv")
str(ca)

# replace median house value by log median house values
ca$logMedVal=log(ca$medianHouseValue)
ca$medianHouseValue <- NULL

# make variables accesible my names
attach(ca)

# split data in training and testing partitions 
set.seed(14) 

n=nrow(ca) # total number of observations
n1=floor(n*0.6) # observations in training partition
n2=n-n1 # observations in testing partition 

ii = sample(1:n,n) # get a random order of n observations
catrain=ca[ii[1:n1],] # select 60% observations in random order ii into train dataset
catest = ca[ii[n1+1:n2],] # the reminder 40% to testing dataset

# check the size of training partition
dim(catrain)[1]/(dim (catrain)[1] + dim (catest)[1])

# check the size of testing partition
dim(catest)[1]/(dim (catrain)[1] + dim (catest)[1])
# 8.end------------------------------------------------



# 9.start----------------------------------------------
# Single Tree model using rpart
if (!require("rpart")) install.packages("rpart")  #install if necessary
library(rpart) # load package to fit a tree model

# fit a big tree using rpart.control
# tunning parameters: 
# minsplit: minmum number of observations in the node for split to take place
# cp: a complexity parameter which sets a threshold value of 
# minimum improvement in fit for split to take place

big.tree = rpart(logMedVal~., method="anova",data=catrain, 
                 control=rpart.control(minsplit=5,cp=.0005))

# show size of the big tree 
ntree = length(unique(big.tree$where))
cat("size of the tree: ",ntree,"\n")


# predict on testing partition using prunned tree
rparttestpred = predict(big.tree,newdata=catest)

# compute Root Mean Squared Error out of sample 
rmse_rpart = sqrt(mean((catest$logMedVal-rparttestpred)^2))
cat("RMSE Single Tree Model: ",rmse_rpart,"\n")
# 9.end------------------------------------------------



# 10.start----------------------------------------------
# Random Forest model
if (!require("randomForest")) install.packages("randomForest") # install if necessary
library(randomForest) # load package to fit RF model

# tuning parameters: 
# mtry = number of variables in each node 
# ntree = number of trees or number of bootstrap samples

# fit 3 different RF models

# rffit1 = randomForest(logMedVal~.,data=catrain,mtry=3,ntree=500)
# rffit2 = randomForest(logMedVal~.,data=catrain,mtry=6,ntree=250)
rffit3 = randomForest(logMedVal~.,data=catrain,mtry=3,ntree=50)

# predict on testing partition 
# rftestpred1 = predict(rffit1,newdata=catest)
# rftestpred2 = predict(rffit2,newdata=catest)
rftestpred3 = predict(rffit3,newdata=catest)

# OOS error 1
# rmse_rf1 = sqrt(mean((catest$logMedVal-rftestpred1)^2))
# cat("rmse on test for RF m=3 ntree=500: ",rmse_rf1,"\n")

# OOS error 2
# rmse_rf2 = sqrt(mean((catest$logMedVal-rftestpred2)^2))
# cat("rmse on test for RF m=6 ntree=250: ",rmse_rf2,"\n")

# OOS error 2
rmse_rf3 = sqrt(mean((catest$logMedVal-rftestpred3)^2))
cat("RMSE on test for RF m=3 ntree=50: ",rmse_rf3,"\n")

# Variable importance:  
# Variables with high importance are better predictors
cat("Variable Importance RF")
varImpPlot(rffit3)
# 10.end------------------------------------------------



# 11.start----------------------------------------------
# Linear fit
lmfit <- lm(logMedVal ~ ., data=catrain)

# predict on testing partition
lmtestpred=predict(lmfit,newdata=catest)

# OOS error
rmse_lm = sqrt(mean((catest$logMedVal-lmtestpred)^2))
cat("RMSE on test for linear model: ",rmse_lm,"\n")
# 11.end------------------------------------------------


# 12.start----------------------------------------------
# compare OOS predictive capacity of different models
# single tree model, random forest, linear model:
cbind(rmse_rpart,rmse_rf3, rmse_lm)
# 12.end------------------------------------------------



# 13.start----------------------------------------------
# Boosting model
if (!require("gbm")) install.packages("gbm") # install if necessary 
library ("gbm") # GBM: Gradient Boost Machine

# tuning parameters:
# B = number of iterations (number of trees in the sum)
# depth = size of each tree
# shrinkage = shrinkage factor

# fit 3 models with different tuning parameters

# boostfit1 = gbm(logMedVal~.,data=catrain,distribution="gaussian",
#               interaction.depth=4,n.trees=5000,shrinkage=.2)

# boostfit2 = gbm(logMedVal~.,data=catrain,distribution="gaussian",
#                interaction.depth=10,n.trees=1000,shrinkage=.02)

boostfit3 = gbm(logMedVal~.,data=catrain,distribution="gaussian",
                interaction.depth=4,n.trees=1000,shrinkage=.2)

# predict on testint partition
# boosttestpred1=predict(boostfit1,newdata=catest,n.trees=5000)
# boosttestpred2=predict(boostfit2,newdata=catest,n.trees=1000)
boosttestpred3=predict(boostfit3,newdata=catest,n.trees=1000)

# OOS error 1
# rmse_gbm1 = sqrt(mean((catest$logMedVal-boosttestpred1)^2))
# cat("rmse on test for boosting depth=4, n.trees=5000, shrinkage=.2: ",rmse_gbm1,"\n")

# OOS error 2
# rmse_gbm2 = sqrt(mean((catest$logMedVal-boosttestpred2)^2))
# cat("rmse on test for boosting depth=10, n.trees=1000, shrinkage=.02: ",rmse_gbm2,"\n")

# OOS error 2
rmse_gbm3 = sqrt(mean((catest$logMedVal-boosttestpred3)^2))
cat("RMSE on test for Boosting 4; 1000; 2: ",rmse_gbm3,"\n")

# Variable importance:  
# Variables with high importance are better predictors
cat("Variable Importance Boosting")
summary(boostfit3, plotit = FALSE)
# 13.end------------------------------------------------




# 14.start----------------------------------------------
# compare OOS predictive capacity of different models 
# single tree model, random forest, boosting, linear model
cbind(rmse_rpart,rmse_rf3, rmse_gbm3, rmse_lm)
# 14.end------------------------------------------------




# Extra: Classification problem
# Practice using IRIS dataset

# 15.start----------------------------------------------
# load the iris dataset 
data(iris)
# look at the dataset
str(iris)
# plot the data
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
legend(2,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)
# 15.end------------------------------------------------



# 16.start----------------------------------------------
# build a classification tree model on iris data
if (!require("tree")) install.packages("tree")  #install if necessary
library(tree)
# fit a single tree model, using sepal width and petal width as predictors
tree <- tree(Species ~ Sepal.Width + Petal.Width, data = iris) 
plot(tree,type= "uniform")
text(tree, label=c("yval"), cex=1)
# 16.end------------------------------------------------



# 17.start----------------------------------------------
# Look at partitions overlayed on species data
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree,label="Species",add=TRUE)
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)
# 17.end------------------------------------------------




# Fit tree models on the iris dataset and compare their predictive capacity

# 18.start----------------------------------------------
# Load iris data, fit one tree model
data(iris)
set.seed(14) 
n=nrow(iris)
n1=floor(n/2)
n2=n-n1
ii = sample(1:n,n)
iristrain=iris[ii[1:n1],]
iristest = iris[ii[n1+1:n2],]

# check size of training and testing datasets
dim (iristrain)
dim (iristest)

# Fit One tree model using rpart package
rpartfit <- rpart(Species ~ ., data=iristrain) # predict using all predictors
# OOS prediction
rpartfitpred <- predict(rpartfit, newdata=iristest, type="class")
# Classification table
table(iristest$Species, rpartfitpred)
# OOS accuracy
accuracy <- table(rpartfitpred, iristest$Species)
rpart_acc <- sum(diag(accuracy))/sum(accuracy)
# 18.end------------------------------------------------




# 19.start----------------------------------------------
# Random Forest
rfrit = randomForest(Species~.,data=iristrain, importance=TRUE, ntree=100)
# OOS Prediction
rfritpred = predict(rfrit,newdata=iristest)
# Classification table
table(iristest$Species, rfritpred)
# OOS accuracy
accuracy <- table(rfritpred, iristest$Species)
rf_acc <- sum(diag(accuracy))/sum(accuracy)
# 19.end------------------------------------------------



# 20.start----------------------------------------------
# Boosting
gbmfit = gbm(Species~.,data=iristrain, distribution ='multinomial',
             interaction.depth=10,n.trees=1000,shrinkage=.02)
# OOS prediction
gbmfitpred = predict(gbmfit,newdata=iristest,n.trees=1000, type='response')
# pick the response with the highest probability 
# from the resulting matrix of predicted probabilities
gbmfitpredcat <- apply(gbmfitpred, 1, which.max)
# Classification table
table(iristest$Species, gbmfitpredcat)
# OOS accuracy
accuracy <- table(gbmfitpredcat, iristest$Species)
gbm_acc <- sum(diag(accuracy))/sum(accuracy)
# 20.end------------------------------------------------




# 21.start----------------------------------------------
# Multinomial model
if (!require("nnet")) install.packages("nnet") # install if necessary
library("nnet") # load the package
mnfit <- multinom(Species ~ ., data=iristrain, trace=FALSE)
# OOS prediction
mnfitpred<- predict(mnfit, newdata=iristest)
# Classification table
table(iristest$Species, mnfitpred)
# OOS accuracy
accuracy <- table(mnfitpred, iristest$Species)
mn_acc <- sum(diag(accuracy))/sum(accuracy)
# 21.end------------------------------------------------



# 22.start----------------------------------------------
# Compare performance of all models
cbind(rpart_acc,rf_acc,gbm_acc,mn_acc)
# 22.end------------------------------------------------

