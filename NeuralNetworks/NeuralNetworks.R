#The neural network example is based on
#code developed by Michy Alice. 



rm(list = ls())
#setwd('')
if (!require("neuralnet")) install.packages("neuralnet")
library(MASS)
library(neuralnet)
data <- Boston


#===================================================#
#Block 1. Linear regression model
set.seed(2581633)
index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)



#===================================================#
#2. Second Block. Preparing the data for the neural network. 
maxs <- apply(data, 2, max) #2 for columns 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]


#===================================================#
#3. Third block.  Estimating the neural network
n <- names(train_)

#Setting up the formula. 
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
g <- as.formula(medv ~.)
h<-as.formula(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + 
                tax + ptratio + black + lstat)

#3.2 Estimating the neural network
nn <- neuralnet(h,data=train_,hidden=c(5,3),linear.output=T)

#3.3 Exploring the output
names(nn)
InitialWeights<-nn$weights
plot(nn)


#===================================================#
#4. Predicting neural networks

#4.1. First network
#4.1.1 Estimate results using first neural network
pr.nn <- compute(nn,test_[,1:13])

#4.1.2. Unstandardize predicted results
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)

#4.1.3 Unstandardized results in the testing data
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

#4.1.4 Obtaining the MSE. 
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)


#5. Comparing the results

print(paste(MSE.lm,MSE.nn))




plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)


#----
#6 Estimating an additional neural network. 
#----


nn2 <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T,act.fct="logistic",
                algorithm='backprop',learningrate = 0.005,
                threshold = 0.1,stepmax=2000,rep=1)


#6.2 Second neural network
pr.nn2 <- compute(nn2,test_[,1:13])
pr.nn2_ <- pr.nn2$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r2 <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE.nn2 <- sum((test.r2 - pr.nn2_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn,MSE.nn2))

#===================================================#



#=========================================
#7. Additional material if there is time
#========================================


#7.1 Cross entropy cost


email<- read.csv("spam.csv")
email<-data.frame(email)
head(email)

sigmoid<-function(z){
  return(1/(1+exp(-z)))
}

## ------------------------------------------------------------------------
x<-cbind(rep(1,4601),email$word_freq_make,email$word_freq_address)
y<-email$spam

## ------------------------------------------------------------------------
CrossEntropy <- function(beta){
  linearTransformation<-x%*%beta
  input <- sigmoid(linearTransformation)
  Cost <- -sum((y*log(input)) + ((1-y)*log(1-input)))
  n<-nrow(x)
  return(Cost/n)
}

## ------------------------------------------------------------------------
beta<-c(0,0,0)
CrossEntropy(beta)

## ------------------------------------------------------------------------
betaOwn1 <- optim(par=beta,fn=CrossEntropy)

## ------------------------------------------------------------------------
spammy <- glm(spam ~ word_freq_make +word_freq_address, data=email, family="binomial")
b <- coef(spammy)
b
betaOwn1$par

## ------------------------------------------------------------------------
CrossEntropy2<-function(beta){
  cost<-0
  for(i in 1:nrow(x)){
    linearTransformation<-x[i,]%*%beta
    input<-sigmoid(linearTransformation)
    costI<-y[i]*log(input) + (1-y[i])*log(1-input)
    cost<-cost+costI
  }
  return(-cost)
}

## ------------------------------------------------------------------------
if (!require("tictoc")) install.packages("tictoc")
library(tictoc)
tic("Start Cross Entropy with loops")
betaOwnLoop <- optim(par=beta,fn=CrossEntropy2)
print("done with looped-version")
toc()
tic("Start cross entropy vectorized")
betaOwn1 <- optim(par=beta,fn=CrossEntropy)
toc()
betaOwn1
betaOwnLoop




#-------------------------------#
#6.2 Gradient descent

#Gradient descent
fxgradient<-function(x){
  return(x^2+2*x+2)
}

x<-seq(-5,5,1)
plot(x,fxgradient(x),type='l')





xguess<-4
xguessactual<-4
alpha=0.009
for(i in 1:10){
  newg<-xguessactual-alpha*(2*xguessactual+2)
  xguess<-c(newg,xguess)
  yguess<-fxgradient(xguess)
  xguessactual<-newg
}
plot(x,fxgradient(x),type='l')
lines(xguess,yguess,col='green',type='o')

plot(xguess,yguess,col='green',type='o')





