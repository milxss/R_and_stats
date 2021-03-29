#unsupervised learning 

## Dimension reduction with pca 
data("iris")
X = iris[,-5]
Y = as.numeric(iris$Species)

install.packages("class")
library(class)

Y = predict(pca)[,1:2]
# rhis is equivalent to these actions:
U = pca$loadings[,1:2]
dim(U)
Y = as.matrix(X) %*% U
dim(Y)

X[1,]
Y[1,]

plot(Y,col=cls)

# another example of PCA with decathlon data

#let's first load data from internet:

decathlon = read.table("http://math.agrocampus-ouest.fr/infoglueDeliverLive/digitalAssets/108286_decathlon.csv",header=TRUE,sep=';')

dim(decathlon)

# let's now focus on athlete's performance data
X = decathlon[,2:11] # we focus on the data that is related to competitions
rownames(X) = decathlon$X

#exercise: perform a PCA and visualize the data in 2-dimentional space and 
#identify the most performant athlets.

pca = princomp(X)
biplot(pca)

# we can guess that Serble or Clay are the most performant athlets 
Y=predict(pca)[,1:2]

#coordinates of the 1st individual in the original space (X)
X[1,]

# coordinates of the 1st individual in the projection space (Y = X*U)
Y[1,]

plot(Y, type='n')

text(Y, labels = rownames(X))



#selection of the numbers of axes to retrain
# one of the rules is to retrain the nb of axes s.t. the percentage of explained
#variance is larger than 90%

summary(pca)

#here 2 PCA components is enough to explain 93% of the variance 
#the other rule is to look for a break in the eigenvalue scree:

screeplot(pca)
# on this example perhaps we would prefer $d=3$.

#############

#Kmeans and hierarchical clustering 
#In order to highlight the main feautures of kmeans and HC, let's consider the 
# 'swiss' data
data(swiss)
?swiss
#let's first try to apply HC on this data. A general call to HC in R can be done
#as follows:
library(class)
?hclust()

D = dist(swiss)
hc = hclust(D, method = "complete")
plot(hc)
#remark: we observe here that 'hclust' can be applied to any type of data
# if we are able to compute the distance for this data. Conversly, kmeans is mostly 
#restricted to continuous data due to the need to compute centers

hc_centroid = hclust(D, method = "centroid")
plot(hc_centroid)

hc_ward = hclust(D, method = "ward.D")
plot(hc_ward)

#remark: we can see here that the different methods lead to quite different 
#dendogram, leading in turn to different choices of numbers of clusters

#let's remember that clustering aims to help the analyst to understand data. 
#The choice of a method should be done according to this idea.

cl4 = cutree(hc_ward, k=2)
cl4

plot(swiss,col=cl4,pch=19)

clcompl = cutree(hc, k=2)
clcompl
plot(swiss,col=clcompl, pch=19)

clcentroid = cutree(hc_centroid, k=2)
clcentroid
plot(swiss,col=clcentroid,pch=19)



# Kmeans

# let's have a look at the same data with Kmeans

?kmeans
#a basic call to kmeans would be 


data(swiss)
out = kmeans(swiss, centers =2)
out

#ex: write a short script that plot the curve of J(k) for the dataset at hand

J = rep(NA, 15)
for (k in 1:15){
  out = kmeans(swiss, centers=k)
  J[k] = out$betweenss/out$totss
  }
plot(J, type='b')

#we observe on this small script that the curve can change with different initialization
#and we can even observe some non-increasing curves for 'unlucky' initialization 
# a way to avoid this init effect we can average on several runs for each value of k

J = matrix(NA, 10, 15)
for (k in 1:15){
  for(l in 1:10){
    out = kmeans(swiss, centers=k)
    J[l,k] = out$betweenss/out$totss
  }
}
boxplot(J,type='b')

# on this example I'd probably pick $k^*=5$ as the most appropriate nb of clusters
#ex: visualize the clustering result with a pair plot, and compare to the one 
#obtained with hclust

out5 = kmeans(swiss, centers= 5, nstart =10)
plot(swiss, col=out5$cluster,pch=19)

# The GMM and the EM algorithm

# we use 'mclust' packsge:
install.packages("mclust")
library(mclust)

#the help of the pkg is available as usual:
?Mclust
# a basic call can be performed as follows:
data(swiss)
out_m = Mclust(swiss, G =3)
out_m

summary(out_m)

out_m$parameters

out_m$classification



