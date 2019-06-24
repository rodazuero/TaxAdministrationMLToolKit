
# Class 5
# Model selection and Regularization


## ------------------------------------------------------------------------
# Remove everything from your working space
rm(list = ls())
# setwd ("your working directory")
#setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/Class 5")

## ------------------------------------------------------------------------
# We will need a few packages
if (!require("leaps")) install.packages("leaps"); library("leaps")

## ------------------------------------------------------------------------
# 1. We read the data
prostate <- read.csv("Prostate.csv")
summary(prostate)

# Dependent variable: level of lpsa (level of prostate-specific antigen)
# lcavol (log cancer volume); lweight (log weight); age; lbph (log amount of 
# benign prostatic hyperplasia); svi (seminal vesicle inversion); lc (log capsular 
# penetration), gleason, pgg45 (percentage of Gleason scores 4 or 5)

head(prostate)
sum(is.na(prostate)) # To check if we have missing values

## ------------------------------------------------------------------------
# 2. SUBSET SELECTION
# 2.1 Best subset selection - Different combination of p models
# Want to predict level of lpsa (level of prostate-specific antigen)
# The function identifies the best model that contains a given number
# of predictors, where best is quantified using RSS
reg.best <- regsubsets(lpsa ~ ., data = prostate)

# We can use the summary command to check the best set of variables for
# each model size
reg.summary <- summary(reg.best)
reg.summary 

# The names function tells us other elements that we have:
#  For example: RSS, adjusted R2, Cp, and BIC
names(reg.summary)
reg.summary$rss
reg.summary$adjr2
reg.summary$bic

# By default regsubsets reports results for up to the best eight-variable model, 
# but we can change that to return as many variable-models as we want
reg.best.2 <- regsubsets(lpsa ~ ., data = prostate, nvmax = 5)
summary(reg.best.2)

## ------------------------------------------------------------------------
# We can rank our models using different criteria
# We can plot different metrics to help us choose which one to select
par(mfrow = c(1,2)) # we create a panel to store 4 plots arranged in 2 rows and 2 columns

plot(reg.summary$rss , xlab = "Number of Variables ", ylab =" RSS", type = "l")
which.min(reg.summary$rss)
points(8, reg.summary$rss[8], col = "red", cex = 2, pch = 20)
plot(reg.summary$adjr2 , xlab = "Number of Variables ", ylab = "adjr2" , type = "l")
which.max(reg.summary$adjr2)
points(7, reg.summary$adjr2[7], col = "red", cex = 2, pch = 20)


# Or we rank them in a dataframe
data.frame (
  RSS = which.min(reg.summary$rss),
  BIC = which.min(reg.summary$bic),
  Adj.R2 = which.max(reg.summary$adjr2)
  )

## ------------------------------------------------------------------------
## 2.1 Stepwise approach
# Forward selection
reg.fwd <- regsubsets(lpsa ~ ., data = prostate, method = "forward")
summary.fwd <- summary(reg.fwd)
summary.fwd

results.fwd <- data.frame (
  RSS = which.min(summary.fwd$rss),
  BIC = which.min(summary.fwd$bic),
  Adj.R2 = which.max(summary.fwd$adjr2)
)
results.fwd

## ------------------------------------------------------------------------
# Backward selection
reg.bwd <- regsubsets(lpsa ~ ., data = prostate, method = "backward")
summary.bwd <- summary(reg.bwd)
summary.bwd

results.bwd <- data.frame (
  RSS = which.min(summary.bwd$rss),
  BIC = which.min(summary.bwd$bic),
  Adj.R2 = which.max(summary.bwd$rsq)
)
results.bwd

# In this case both forward and backward selection give the same model using different criteria
## ------------------------------------------------------------------------
## 2.2 Now we make predictions out of sample
if (!require("caTools")) install.packages("caTools"); library("caTools")

# We are going to divide our sample in a training and a testing data
set.seed(25722)
prostate$sample <- sample.split(prostate$lpsa, SplitRatio = 0.70)
summary(prostate$sample) # we have 67 training observation and 30 testing observations
prostate.train <- prostate[prostate$sample == TRUE, 1:9]
prostate.test <- prostate[prostate$sample == FALSE, 1:9]

# We run a model in the training data using forward selection
reg.fwd <- regsubsets(lpsa ~ ., data = prostate.train, method = "forward")
test.mat <- model.matrix(lpsa ~ ., data = prostate.test)

# Unfortunately, regsubsets does not have a prediction function, so we have to create predictions by hand
# We create an empty vector of size 8 to store my error terms
errors <- rep(0, 8) 

# We use a loop and for each best model (best model with 1 variable to best model
#  with 8 variables using RSS), we predict the cancer volume in the testing data
 for(i in 1:8) {
  coefficient <- coef(reg.fwd, id = i) # we use the best model of size p
  prediction <- test.mat[, names(coefficient)] %*% coefficient
  errors[i] <- mean((prostate.test$lpsa - prediction)^2)
  }
  
which.min(errors) # model number five (5 variables) has the lowest testing error (MSE)
  

## ------------------------------------------------------------------------
# 3. Regularization/Shrinking Strategies

# Let's clean our working space 
rm(list = ls())

# We will work with a database in which we have more covariates than observations
# Data: Community and crime in the US combining information from socio-economic census, survey, and crime data
# The full data can be found here "https://archive.ics.uci.edu/ml/machine-learning-databases/communities"
crime <- read.csv("crime.csv") 
head(crime)

# Our variable of interest (to predict) will be ViolentCrimesPerPop
# Violent crimes per population was calculated using population and the sum of 
# different crime variables in the US such as murder, rape, robbery, and assault
summary(crime$ViolentCrimesPerPop)
  
# We need to standardize the data
crime[1:2, 1:4]
var(crime[, 3]) # variance of column number 3, percentage of black people
pctblack <- (crime$racepctblack - mean(crime$racepctblack)) / sd(crime$racepctblack)
var(pctblack)
  
# Instead of doing this manually, we can use the scale function which does the same
crime.sc <- scale(crime[, (1:122)])
crime.sc[1:2, 1:4]
var(crime.sc[, 1:4]) 
  
# Now we can split the data in training and testing
  
set.seed (12323)
crime$sample <- sample.split(crime$ViolentCrimesPerPop, SplitRatio = 0.70)
summary(crime$sample) # we have 84 training observations and 36 testing observations
sc.train <- crime.sc[crime$sample == TRUE,  ]
sc.test  <- crime.sc[crime$sample == FALSE, ]
  
## ------------------------------------------------------------------------
# 3.1 Linear model?
  
# Let's try to run a OLS model in the full data
ols <- lm(ViolentCrimesPerPop ~ ., data = crime, y = TRUE)
summary(ols) # In this case OLS does not work
rm(ols)
  
## ------------------------------------------------------------------------
# 3.2 Ridge Regression
# The package glmnet has an alpha argument that determines what type of model we fit
#   Paramater: alpha = 0 is Ridge, alpha = 1 is Lasso
# Also, by default the function glmnet() performs Ridge 
#   for automatically select range of lambda values (about a grid of 100 values)
# Unlike, lm We have to specify a numerical design matrix and a response vector
# Paramater: alpha = 0 is Ridge, alpha = 1 is Lasso
if (!require("glmnet")) install.packages("glmnet"); library("glmnet")
set.seed(12372)
fit.ridge <- glmnet(sc.train[, 1:121], sc.train[, 122], alpha = 0)
par(mfrow = c(1,1)) 
plot(fit.ridge, xvar = "lambda", label = TRUE)
  
# We can also choose the lambda via cross-validation
# Function cv.glmnet() finds the right parameter for lambda (penalty). Works with matrices, not data frames.
# By default the function uses K-fold cross-validation with K = 10
cv.fit.ridge <- cv.glmnet(sc.train[, 1:121], sc.train[, 122], alpha = 0, nfolds = 3)
bestlam.r <- cv.fit.ridge$lambda.min
bestlam.r # value of lambda that yields the smallest CV-error is 8.7
log(8.7)
  
# We can visualize the MSE along different values of log-lambda
# Here we see log value of lambda that best minized the CV-error
plot(cv.fit.ridge, main = "Ridge")
  
# Now we can use that value to estimate the associated testing error to that lambda
pred.ridge <- predict(cv.fit.ridge, s = bestlam.r, sc.test[, 1:121]) 
SSR.ridge <- sum((pred.ridge - sc.test[, 122])^2)
MSE.ridge <- mean((pred.ridge - sc.test[, 122])^2)
SSR.ridge

# We can also check our regularized coefficients
coef(cv.fit.ridge, cv.fit.ridge$lambda.min)
  
## ------------------------------------------------------------------------
# 3.3 LASSO
# Now we fit a lasso
fit.lasso <- glmnet(sc.train[, 1:121], sc.train[, 122], alpha = 1)
plot(fit.lasso, xvar = "lambda", label = TRUE)
  
# We use CV to find the best lambda
cv.fit.lasso <- cv.glmnet(sc.train[, 1:121], sc.train[, 122], alpha = 1, nfolds = 3)
bestlam.l <- cv.fit.lasso$lambda.min
bestlam.l # value of lambda that yields the smallest CV-error is 0.056
log(0.056)
  
# We can visualize the MSE along different values of log-lambda
# Notice that for the optimal value of lambda we wind up with 16 coefficients
plot(cv.fit.lasso)
coef(cv.fit.lasso, cv.fit.lasso$lambda.min) # we can check them
  
# we predict the MSE
pred.lasso <- predict(cv.fit.lasso, s = bestlam.l, sc.test[, 1:121]) 
SSR.lasso <- sum((pred.lasso - sc.test[, 122])^2)
MSE.lasso <- mean((pred.lasso - sc.test[, 122])^2)

  
## ------------------------------------------------------------------------
# LASSO performs better on this data according to the RSS or MSE
SSR.ridge
SSR.lasso

MSE.ridge
MSE.lasso

par(mfrow = c(2,2))
plot(fit.ridge, xvar = "lambda", label = TRUE)
plot(cv.fit.ridge, main = "Ridge")
plot(fit.lasso, xvar = "lambda", label = TRUE)
plot(cv.fit.lasso, main = "Lasso")


