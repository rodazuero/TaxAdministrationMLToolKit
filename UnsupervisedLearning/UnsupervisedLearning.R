
# Class 7: Unsupervised ML
# Practical examples

# Data: 
# 1) European Protein Consumption
#    https://github.com/WinVector/zmPDSwR/tree/master/Protein

#  2) wine 
#    https://rdrr.io/cran/rattle.data/man/wine.html

# 3) we8there
#    https://www.rdocumentation.org/packages/distrom/versions/0.1/topics/we8there




# 0.start----------------------------------------------
# remove everything from your working space
rm(list = ls())
# set wd
# setwd ("your working directory")
# 0.end------------------------------------------------



# 1.start----------------------------------------------
# Exmpale 1: Cluster Europe by Food

# European protein consumption dataset

# This data set shows consumption of different types of protein
# in European countries.
# The consumption of proteins is per person per day in grams


# We want to understand in which way nutrition is different in different countries


# load the data
food <- read.csv("protein.csv", row.names=1) # 1st column is country name

# quick look at the data 
str(food)

# K-means
# K-means idea: classify xi in K clusters so that 
# sum of (xi-muK)*(xi-muK) is minimal.

# the algorimt starts at rondom K, and changes Ks until
# the sum of squares stops improving. 

# !!! Standarization !!!!
# for K-means we have to standardize (or scale) features 
xfood <- scale(food) # use scale funcion to standardize
head(xfood)
# 1.end------------------------------------------------


# 2.start----------------------------------------------
# Consider 3 clusters of Red vs White meat consumption 
grp<- kmeans(xfood[,c("WhiteMeat","RedMeat")],
                         centers=3) # centers=3 clusters

# results of kmeans function:
grp
?kmeans # to learn more about kmeans function

grp$cluster # holds cluster assignment for each observation
grp$withinss # SS withing cluster: measure of "compactness" 

# visualizing k-means clusters
# consumption is shown in standard deviations from the mean

# plot 
plot(xfood[,"RedMeat"], xfood[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="Red Meat", ylab="White Meat")
text(xfood[,"RedMeat"], xfood[,"WhiteMeat"], labels=rownames(food), 
     col=rainbow(4)[grp$cluster])
# 2.end------------------------------------------------




# 3.start----------------------------------------------
# same thing, but now we are clustering in 7 clusters 
# on all nine protein types
grpProtein <- kmeans(xfood, centers=7) # change the number of centers to see what happens.


grpProtein$cluster # holds cluster assignment for each observation


# Red meat and White meat
plot(xfood[,"RedMeat"], xfood[,"WhiteMeat"], xlim=c(-2,2.75), 
     type="n", xlab="RedMeat", ylab="WhiteMeat")
text(xfood[,"RedMeat"], xfood[,"WhiteMeat"], labels=rownames(food), 
     col=rainbow(7)[grpProtein$cluster]) ## col is all that differs from first plot
# 3.end------------------------------------------------




# 4.start----------------------------------------------
# same plot, but now we show Milk and Eggs consumption 
# clustering in 7 clusters 
# on all nine protein types
plot(xfood[,"Milk"], xfood[,"Eggs"], xlim=c(-2,2.75), 
     type="n", xlab="Milk", ylab="Eggs")
text(xfood[,"Milk"], xfood[,"Eggs"], labels=rownames(food), 
     col=rainbow(7)[grpProtein$cluster]) ## col is all that differs from first plot
# 4.end------------------------------------------------




# 5.start----------------------------------------------
# Exmpale 2: Cluster Wine on 11 chemical composition attributes

# the dataset contains the information of 11 chemical attributes of different wines
# + wine color and wine quality. 


# load the data
wine <- read.csv("wine.csv")
str(wine) # one of the featuers is the color of wine

# scale!!!
xwine <- scale(wine[,1:11])
# 5.end------------------------------------------------



# 6.start----------------------------------------------
# fit two clusters on all predictors
two <- kmeans(xwine,
              2, # number of clusters
              nstart=10) # R will try 10 different random starting centriod assignments
                         # R selects the assignment with the lowest within cluster variation

two$centers # big differences on all accounts

# what is the color distribution in each cluster?
tapply(wine$color,two$cluster,table)
# mostly, the two clusters are red and white wines.
# 6.end------------------------------------------------




# 7.start----------------------------------------------
# Plot clusters against the wine color

# point border is true color, body is cluster membership

# we randomize the order in plot, just so it is not all white on top of red

# note that in this case cluster 1 is white, this could be flipped for you.
# if flipped, draw the graph switching the colors in the labels
# we have done this for you in chunk 8.

i <- sample(1:nrow(xwine))  
par(mfrow = c(1, 1))
plot(wine$fixed.acidity[i], wine$volatile.acidity[i],
     pch=21, cex=.75, bty="n",
     xlab="fixed acidity",
     ylab="volatile acidity",
     bg=c("gold","maroon")[two$cluster[i]],
     col=c("maroon","gold")[wine$color[i]])
# 7.end------------------------------------------------





# 8.start----------------------------------------------

# if your figure looks like Oops, try this code instead. 
# there the order of color labels in flipped

i <- sample(1:nrow(xwine))  
plot(wine$fixed.acidity[i], wine$volatile.acidity[i],
     pch=21, cex=.75, bty="n",
     xlab="fixed acidity",
     ylab="volatile acidity",
     bg=c("maroon","gold")[two$cluster[i]], # the order of colors is flipped
     col=c("maroon","gold")[wine$color[i]])
# 8.end------------------------------------------------






# 9.start----------------------------------------------
# Determine the true number of clusters

# Elbow method: plot a scree plot with Sum of Squares within group 
# against the number of clusters

# Sum of Squares within clusters measures the compactness of the clustering 
# and we want it to be as small as possible


# Plot a scree plot
# Initialize total within sum of squares error: wss
wss <- 0
# Look over 1 to 15 possible clusters
for (i in 1:15) {
  
  # Fit the model: km.out
  km.out <- kmeans(xwine, centers = i, nstart = 20, iter.max = 50)
  
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}
wss

# Produce a scree plot
par(mfrow = c(1, 1))
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")


# choose a number of clusters so that adding another cluster 
# does no€™t improve much within sum of squares.


# in real world data, a nice clean elbow in the scree plot
# does not usually exist

# other methods: Silhouette method, Gap Statistic

# or information criteria (AIK, BIC).

# 9.end------------------------------------------------




# 10.start----------------------------------------------
# does the initial guess of centriod matter? 

# Set up 2 x 3 plotting grid to show 6 results of classification
# in clusters, all with different initial guesses of centriods. 

# Set seed
set.seed(1)

# Plot the figure
par(mfrow = c(2, 3))
for(i in 1:6) {
  # Run kmeans() on data with 4 clusters and one start 
  km.out <- kmeans(xwine, centers = 4, nstart = 1)
  
  # Plot clusters
  plot(xwine, col = km.out$cluster,
       main = km.out$tot.withinss,
       xlab = "", ylab = "")
}

# 10.end------------------------------------------------





# 11.start----------------------------------------------
# Example 3: text clustering in topics
# using we8there data

# The dataset is based on 6166 restaurant reviews

# Each review is a short text, which is user-written

# The short user-submitted reviews are accompanied by a five-star rating on 
# four specific aspects of restaurant quality: food, service, value, and atmosphere
# as well as the overall experience.  


# Content: 
# Two data files: we8thereCounts and we8thereRatings

# Counts for 2640 bigrams (pairs of words) exctracted from 6166 restaurant reviews

# Ratings: show the rating for each of 6166 reviews in 4 dimensions, and the overall rating. 


# load the data
if (!require("textir")) install.packages("textir") # install if needed
library(textir) # the dataset we8there is in this package

data(we8there)


#Raitings:
str(we8thereRatings)

# Counts
dim(we8thereCounts)
# 6166 reviews
# 2640 bigrams (pair of words)
str(we8thereCounts)

# 11.end------------------------------------------------




# 12.start----------------------------------------------
# Topic modelling clusters bigrams in topics 

# Idea: each bigram is from a different topic, and the document
# is a mixture of topics.

# we'll use the topics function in maptpx 
if (!require("maptpx")) install.packages("maptpx") # install if needed
library(maptpx) # for the topics function

# we need to convert from a Matrix to a simple_triplet_matrix
x <- as.simple_triplet_matrix(we8thereCounts)

# let's fit 5 topics model
# to fit, just pass the counts and the number of topics K 
tpc <- topics(x,K=5) 
summary(tpc)

# interpretation: 
# summary prints the top bigrams for each topic,
# The top n bigrams are ordered by lift = 
# topic word prob/ marginal word prob.

# what is the right nubmer of topics? 
# run topics model, allowing R to choose the nuber of topics based on BIC criterion
tpcs <- topics(x,K=5*(1:5)) 
summary(tpcs) # R chooses 10 topics 

# 12.end------------------------------------------------



# 13.start----------------------------------------------
# We can also look at words ordered by simple in-topic probability
# the topic-term probability matrix is called 'theta', 
# and each column is a topic
# we can use these to rank terms by probability within topics

rownames(tpcs$theta)[order(tpcs$theta[,1], decreasing=TRUE)[1:10]]
rownames(tpcs$theta)[order(tpcs$theta[,2], decreasing=TRUE)[1:10]]

# topic one looks "good", topic 2 looks "bad". 
# 13.end------------------------------------------------




# 14.start----------------------------------------------
# use the wordcloud library to visulalize words in topics 
if (!require("wordcloud")) install.packages("wordcloud") # install if needed
library(wordcloud)

# we'll size the bigram proportional to its in-topic probability
# and only show those with > 0.004 omega
# it will still likely warn that it couldn't fit everything

par(mfrow=c(1,2))
par(mar = rep(0, 4))
wordcloud(row.names(tpcs$theta), 
          freq=tpcs$theta[,1], min.freq=0.004, col="maroon")
par(mar = rep(0, 4))
wordcloud(row.names(tpcs$theta), 
          freq=tpcs$theta[,2], min.freq=0.004, col="navy")

# 14.end------------------------------------------------






