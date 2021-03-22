# reminder on descriptive statistics
{r}
data(iris)
X = iris$Sepal.Length 
# we use famous iris dataset that is built in R
hist(X)
# lets see how histogram changes based on breaks specified
hist(X, breaks=100)
#it's also interesting to combine hist with an estimation of the 
#pdf function made by density
hist(X, freq=FALSE)
lines(density(X),col=2, lwd=2)

boxplot(X) #let's now use boxplots
#to relate boxplot with hist:
par(mfrow=c(2,1))
hist(X, freq=FALSE)
lines(density(X),col=2, lwd=2)
boxplot(X, horizontal = TRUE)

#regarding multivariate data it's possible to perform boxplot
boxplot(iris[,-5])
#it's possible for iris data to plot several boxplots side by side
#because 4 variables are all expressed in the same units (cm)

#the alternative will be a for loop
par(mfrow=c(2,2))
for (j in 1:4) boxplot(iris[,j], main=paste('variable',j))

#about scatterplots:
plot(iris[,-5])

cor(iris[,-5]) #correlation matrix 

#it is possible to add to this plot an additional category
#called variable(here the species)
plot(iris[,-5], col=as.numeric(iris$Species))

# of course, it is possible to have a classical scatter plot 
#when looking only at 2 variables
plot(iris$Sepal.Length, iris$Petal.Length, col=as.numeric(iris$Species))

# Multivariate numerical indicators

mu = colMeans(iris[,-5])  #colMeans computes mean of each column of the dataset
mu

#mean() function computes mean of all data, NA for multiviriable

#Covariance and correlation metrices:

S = cov(iris[,-5])
S

C = cor(iris[,-5])
C

#it's easier to read correlation matrix where -1 -> inverse-relation
# 0 -> no relation and 1 -> strong relation


#LDA and logistic regression
#lets start again with iris data (as Fisher used it himself)
data(iris)
library(MASS)

X = iris[,-5]
Y = as.numeric(iris$Species)


library(MASS)
system.time(f <- lda(X,Y))

#after learning it's possible to examine the output 
#Call:
lda(X,Y)

#afterwards, if we would like to classify a new observation,
#we have to call the 'predict' function:

xstar = c(6, 2.9, 5, 3) #new observtion 

ystar = predict(f, xstar)
ystar #it belongs to class 1 virginica 

#let's compute error rate of this model
#first a single split

n = nrow(X)
learn = sample(1:n, 2/3*n)
f <- lda(X[learn,], Y[learn])
ystar = predict(f,X[-learn,])

#compiute the classification error
err = sum(Y[-learn] != ystar$class) / length(ystar$class)
err

# a better version with resampling (boostraping):

n = nrow(X)
err = c()
for (i in 1:10){
  learn = sample(1:n, 2/3*n)
  f <- lda(X[learn,], Y[learn])
  ystar = predict(f,X[-learn,])
  #compute the classification error
  err[i] = sum(Y[-learn] != ystar$class) / length(ystar$class)
}
boxplot(err)



