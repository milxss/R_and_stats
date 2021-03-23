#SA Massive and High-Dimentional Data

#classification with KNN
# a basic call to KNN is as follows:

data(iris)
X = iris[,-5]
Y = as.numeric(iris$Species)
install.packages("class")
library(class)
?knn

xstar = c(6,2.9,5,3)
ystar = knn(X, xstar, Y,k=5)
ystar

#evaluate with a V-fold cross-val the perdormance of KNN
#k=5
V=25
n=nrow(X)
fold=rep(1:V, n/V)
err.knn5 = err.lda = rep(NA, V)
for (v in 1:V){
  learn = which(fold!=v)
  test = which(fold ==v)
  #KNN
  ystar = knn(X[learn,], X[test,], Y[learn], k=5)
  err.knn5[v] = sum(ystar != Y[test]) / length(ystar)
}
cat(paste('>KNN5:', mean(err.knn5), '+/-', sd(err.knn5), '\n'))


#find best K for KNN
V=25; K= 30
n=nrow(X)
fold=rep(1:V, n/V)
err.knn = matrix(NA, K, V)

for (v in 1:V){
  learn = which(fold!=v)
  test = which(fold ==v)
  for (k in 1:K){
    ystar = knn(X[learn,], X[test,], Y[learn], k)
    err.knn[k,v] = sum(ystar != Y[test]) / length(ystar)
  }
}
plot(rowMeans(err.knn), type = 'b')

#on this run, we would pick k=11.
# Warning: to compare the performance of KNN in general with LDA
# we should use double cross-validation for both picking the best 
# K and evaluating the performance 


#trying the best K on the previous code
V=25
n=nrow(X)
fold=rep(1:V, n/V)
err.knn11 = err.lda = rep(NA, V)
for (v in 1:V){
  learn = which(fold!=v)
  test = which(fold ==v)
  #KNN
  ystar = knn(X[learn,], X[test,], Y[learn], k=11)
  err.knn11[v] = sum(ystar != Y[test]) / length(ystar)
}
cat(paste('>KNN11:', mean(err.knn11), '+/-', sd(err.knn11), '\n'))



#classification with SVM 'e1071' provides nice implementation of SVM
install.packages('e1071')
library(e1071)

X = iris[,-5]
Y = as.numeric(iris$Species)

#a basic use of SVM on the iris data:
xstar = c(6,2.9,5,3)
f.svm = svm(X,Y,kernel = 'radial', gamma = 1, type - "C-classification")
ystar = predict(f,svm, matrix(xstar, nrow=1))
ystar

#best gamma
V=25; Gamma = seq(0.01,1,by=0.1)
n=nrow(X)
fold=rep(1:V, n/V)
err.svm =matrix(NA, length(GAMMA), V)

for (v in 1:V) {
  learn = which(fold != v)
  test = which(fold ==v)
  for (j in 1:length(GAMMA)){
    gamma = GAMMA[j]
    f.svm = svm(X[learn,],Y[learn], kernel = 'radial', gamma=gamma,
                type = "C-classification")
    ystar = predict(f.svm, X[test,])
    err.svm[j,v]= sum(ystar != Y[test]) / length(ystar)
  }
}
plot(GAMMA, rowMeans(err.svm), type='b')
title(main=paste('Best gamma value:', GAMMA[which.min(rowMeans(err.svm))], '\n'))




#unsupervised learning
#dimension reduction with PCA
# we use iris data
# the usual manner to do PCA in R is:
pca = princomp(X)
biplot(pca)


#we can compare the observations in the original space with their projection on the new variables:
Y=predict(pca)[,1:2]

#coordinates of the 1st individual in the original space (X)
X[1,]

# coordinates of the 1st individual in the projection space (Y = X*U)
Y[1,]

plot(Y)
