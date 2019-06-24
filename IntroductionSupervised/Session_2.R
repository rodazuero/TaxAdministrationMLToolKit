
## ------------------------------------------------------------------------
#setwd()

## ------------------------------------------------------------------------
getwd()

## ------------------------------------------------------------------------
rm(list = ls())

## ------------------------------------------------------------------------
WageD<-read.table("WageCSV.csv",sep = ",")
WageD<-as.data.frame(WageD)
head(WageD)

## ------------------------------------------------------------------------
colnames(WageD)<-c("age","schooling","wage")

## ------------------------------------------------------------------------
head(WageD)

tail(WageD)

dim(WageD)

nrow(WageD)

ncol(WageD)

class(WageD)

names(WageD)


#Structure of an object
str(WageD)

#Summary
summary(WageD)

#Some additional analysis of the data
#We can ask R to return specifics of the dataset. 
#When we execute WageD[i,j] we should obtain the
#element stored in the i-th row and j-th column of the dataframe
#WageD

WageD[2,3]

WageD[1,]


#We can call the columns by their name via two ways:

WageD$age

WageD[,"age"]

#

WageD[1:10,]

WageD[,1]

head(WageD$age)

head(WageD[,"age"])

WageD[WageD$age>55,]

subset(WageD,schooling<3)


mean(WageD$wage)
sd(WageD$wage)
    
mean(WageD$age)
sd(WageD$age)
    
mean(WageD$schooling)
sd(WageD$schooling)


## ------------------------------------------------------------------------
hist(WageD$age)

plot(WageD$schooling,WageD$wage)

## ------------------------------------------------------------------------
mod<-lm(wage~ age+schooling, data=WageD)

#let us see what we have stored in 'mod'
summary(mod)

#Mod stores a series of objects with different names. We can see the list of names 
#with the 'names' function:
names(summary(mod))

#We can call each object with the '$' sign:
summary(mod)$coefficients

summary(mod)$call

#Do you want to know more about the 'lm'  command?
?lm

## ------------------------------------------------------------------------
print(coef(summary(mod)))

## ------------------------------------------------------------------------
head(predict(mod))
head(WageD$wage)

## ------------------------------------------------------------------------
newdata <- data.frame(age=32,schooling=12)
predict(mod,newdata)

## ------------------------------------------------------------------------
email<- read.csv("spam.csv")
email<-data.frame(email)
head(email)

Spam <- subset(email, spam == 1)
NoSpam <- subset(email, spam == 0)

mean(Spam$word_freq_make)
mean(NoSpam$word_freq_make)


## ------------------------------------------------------------------------
mod<-lm(spam~ ., email)
print(coef(summary(mod)))
T<-predict(mod, email[,1:57])
min(T)
max(T)
mean(T)
sd(T)

## ------------------------------------------------------------------------
x<-seq(-5,5,1)
logistic<-function(z){
  ans<-1/(1+exp(-z))
  return(ans)
}
plot(x,logistic(x),type="l")

## ------------------------------------------------------------------------
if (!require("caTools")) install.packages("caTools")
library(caTools)
set.seed(241567)
email$sample<-sample.split(email$spam,SplitRatio=0.8)
emailtrain=subset(email, email$sample==TRUE)
emailtest=subset(email, email$sample==FALSE)

#Removing the last column (sample)
email$sample<-NULL
emailtrain$sample<-NULL
emailtest$sample<-NULL

## ------------------------------------------------------------------------
#Logistic regression
LogitModel <- glm(spam ~ ., data=emailtrain, family="binomial")
bLogit <- coef(LogitModel)

#Linear regression
LinearRegressionModel <- lm(spam ~ ., data=emailtrain)
bLinearRegression <- coef(LinearRegressionModel)

#Predictions in logistic regression
PredictProbabilitiesLogistic <- predict(LogitModel, newdata=emailtest, type="response")
PredictProbabilitiesLinear <- predict(LinearRegressionModel, newdata=emailtest,type="response")

#Predicted probabilities in test data. Confusion matrix
PredictionsLogistic<-PredictProbabilitiesLogistic>0.5
PredictionsLogistic<-as.numeric(PredictionsLogistic)
TLogistic<-table(PredictionsLogistic,emailtest$spam)
TLogistic
colnames(TLogistic)<-c("Predicted non-spam","Predicted Spam")
rownames(TLogistic)<-c("Non-spam","Spam")
TLogistic
D<-dim(emailtest)
TLogistic<-100*(TLogistic/D[1])
TLogistic
TLogistic[1,1]+TLogistic[2,2]

TLogistic

PredictProbabilitiesLinear<-PredictProbabilitiesLinear>0.5
PredictProbabilitiesLinear<-as.numeric(PredictProbabilitiesLinear)
TLinear<-table(PredictProbabilitiesLinear,emailtest$spam)
TLinear
colnames(TLinear)<-c("Predicted non-spam","Predicted Spam")
rownames(TLinear)<-c("Non-spam","Spam")
D<-dim(emailtest)
TLinear<-100*(TLinear/D[1])
TLinear
TLinear[1,1]+TLinear[2,2]

TLinear

## ------------------------------------------------------------------------
#Precission:
TLogistic[2,2]/(TLogistic[2,2]+TLogistic[1,2])

#Recall
TLogistic[2,2]/(TLogistic[2,2]+TLogistic[2,1])

##Accuracy
TLogistic[2,2]+TLogistic[1,1]

## ------------------------------------------------------------------------
    n<-length(WageD$wage)
    X0<-rep(1,n)
    X1<-WageD$age
    X2<-WageD$schooling
    Y<-WageD$wage


    X<-cbind(X0,X1,X2)

    Beta<-(solve(t(X)%*%X))%*%(t(X)%*%Y)
    Beta0<-Beta[1]
    Beta1<-Beta[2]
    Beta2<-Beta[3]

## ------------------------------------------------------------------------
SSR<-function(Beta){
  Pred<-Beta[1]+Beta[2]*WageD$age+Beta[3]*WageD$schooling
  SR<-(WageD$wage-Pred)^2
  ans<-sum(SR)
  return(ans)
}

BetaInic<- c(1 ,1 ,1)
Parameters<- optim(BetaInic, SSR)
Parameters$par

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
SpamData<-read.csv("SpamData2.csv")

## ------------------------------------------------------------------------
head(SpamData)
mean(SpamData$Spam)

## ------------------------------------------------------------------------
SpamEmails<-subset(SpamData,Spam==1)
mean(SpamEmails$exclamation_point==1)

## ------------------------------------------------------------------------
#install.packages("e1071")
library(e1071)

## ------------------------------------------------------------------------
#install.packages("caTools")
library(caTools)
SpamData$sample<-sample.split(SpamData$Spam,SplitRatio=0.8)
train=subset(SpamData, SpamData$sample==TRUE)
test=subset(SpamData, SpamData$sample==FALSE)

train<-train[,1:55]
test<-test[,1:55]


## ------------------------------------------------------------------------
nB_model <- naiveBayes(as.factor(Spam) ~.,data=train)
T<-table(predict(nB_model, test[,1:54]),as.factor(test[,55]))
colnames(T)<-c("predicted non-spam","predicted spam")
rownames(T)<-c("true non-spam", "true spam")
T<-T/sum(T)
T

## ------------------------------------------------------------------------
#Precission:
T[2,2]/(T[2,2]+T[1,2])

#Recall
T[2,2]/(T[2,2]+T[2,1])

##Accuracy
T[2,2]+T[1,1]


## ------------------------------------------------------------------------
Likelihood<-function(Theta){
  bbeta0<-Theta[1]
  bbeta1<-Theta[2]
  bbeta2<-Theta[3]
  ssigma<-exp(Theta[4])
  
  loglik=0;
  for(i in 1:n){
    predwage<-bbeta0+bbeta1*WageD$age[i]+bbeta2*WageD$schooling[i]
    yobserved<-WageD$wage[i]
    error<-yobserved-predwage
    loglikelihood=log(dnorm(error,mean=0,sd=(ssigma)))
    loglik=loglik+loglikelihood
  }
  loglik=-loglik
  return(loglik)
  
}

## ------------------------------------------------------------------------
Theta<-c(1,1,1,1)
Likelihood(Theta)
Parameters<-optim(Theta,Likelihood)
Parameters

## ------------------------------------------------------------------------

x<- seq(-3,3,0.1)
L<-length(x)
xguess <- rep(NA, L)
y <- rep(NA, L)
fx=x^2+2*x+2
fxe<-function(x){x^2+2*x+2}
df<-function(x){2*x+2}
x0=3
alpha=0.1


for (i in 1:L) {
  xguess[i]=x0
  y[i]=fxe(x0)
  x0=x0-alpha*df(x0)
}


plot(xguess,y, ylim = c(0.8, 18),xlim=c(-3,3),col="blue")
lines(x,fx,col="green")

## ------------------------------------------------------------------------
if (!require("animation")) install.packages("animation")
library(animation)
saveHTML({
  x<- seq(-3,3,0.1)
  xguess=x*10000
  y<-10000*x
  fx=x^2+2*x+2
  df<-function(x){2*x+2}
  x0=3
  fxe<-function(x){x^2+2*x+2}
  fxeVector<-fx*0
  y[1]=x0
  for (i in 1:20) {
    plot(xguess,y, ylim = c(0.8, 18),xlim=c(-3,3),col="blue")
    lines(x,fx,col="green")
    x0=x0-0.1*df(x0)
    xguess[i+1]=x0
    y[i+1]=fxe(x0)
    ani.pause()
  }
}, img.name = "Grad_Descent", imgdir = "Grad_descent", htmlfile = "Grad_descent.html", 
autobrowse = FALSE, title = "Demo of 20 grad descent")

