rm(list=ls())
#####################
#### SECTION 1 Part a
#####################
library(ISLR)
set.seed(5082)
n = dim(OJ)[1]
train_inds = sample(1:n,800)
test_inds = (1:n)[-train_inds]
#####################
#### SECTION 1 Part b
#####################
require(e1071)
svmfit=svm(Purchase~., data=OJ[train_inds,], kernel= "linear",cost=0.01)
summary(svmfit)
#from the summary we know that this model uses a linear kernel and has 446 support vectors
#the support vectors were evenly split between the classes, with 223 for each class.
#####################
#### SECTION 1 Part c
#####################
trainpred=predict(svmfit,OJ[train_inds,])
mytable=table(truth=OJ[train_inds,]$Purchase, predict = trainpred)
#training error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#calculate test error rate
testpred=predict(svmfit,OJ[test_inds,])
mytable=table(truth=OJ[test_inds,]$Purchase, predict = testpred)
#test error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#####################
#### SECTION 1 Part d
#####################
#tune for cost (0.01 to 10)
tune.out=tune(svm,Purchase~.,data=OJ,kernel="linear",ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
best=tune.out$best.model
#from best we know that the best cost is 10
#####################
#### SECTION 1 Part e
#####################
trainpred=predict(best,OJ[train_inds,])
mytable=table(truth=OJ[train_inds,]$Purchase, predict = trainpred)
#training error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#calculate test error rate
testpred=predict(best,OJ[test_inds,])
mytable=table(truth=OJ[test_inds,]$Purchase, predict = testpred)
#test error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#####################
#### SECTION 1 Part f
#####################
#repeat b-e with a radial kernel
#b
radialfit=svm(Purchase~., data=OJ[train_inds,], kernel= "radial",cost=0.01)
summary(radialfit)
#c
trainpred=predict(radialfit,OJ[train_inds,])
mytable=table(truth=OJ[train_inds,]$Purchase, predict = trainpred)
#training error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#calculate test error rate
testpred=predict(radialfit,OJ[test_inds,])
mytable=table(truth=OJ[test_inds,]$Purchase, predict = testpred)
#test error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#d
tune.out=tune(svm,Purchase~.,data=OJ,kernel="radial",ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
best=tune.out$best.model
#e
trainpred=predict(best,OJ[train_inds,])
mytable=table(truth=OJ[train_inds,]$Purchase, predict = trainpred)
#training error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#calculate test error rate
testpred=predict(best,OJ[test_inds,])
mytable=table(truth=OJ[test_inds,]$Purchase, predict = testpred)
#test error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#####################
#### SECTION 1 Part g
#####################
#repeat b-e with a polynomial kernel and degree=2
#b
polyfit=svm(Purchase~., data=OJ[train_inds,], kernel= "polynomial",cost=0.01, degree=2)
summary(polyfit)
#c
trainpred=predict(polyfit,OJ[train_inds,])
mytable=table(truth=OJ[train_inds,]$Purchase, predict = trainpred)
#training error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#calculate test error rate
testpred=predict(polyfit,OJ[test_inds,])
mytable=table(truth=OJ[test_inds,]$Purchase, predict = testpred)
#test error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#d
tune.out=tune(svm,Purchase~.,data=OJ,kernel="polynomial",degree=2,ranges=list(cost=c(0.01, 0.1, 1, 5, 10)))
best=tune.out$best.model
#e
trainpred=predict(best,OJ[train_inds,])
mytable=table(truth=OJ[train_inds,]$Purchase, predict = trainpred)
#training error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#calculate test error rate
testpred=predict(best,OJ[test_inds,])
mytable=table(truth=OJ[test_inds,]$Purchase, predict = testpred)
#test error rate
sum(mytable[2]+mytable[3])/sum(mytable)
#####################
#### SECTION 1 Part h
#####################
#from the combination of training and test error, the model with a polynomial kernel and cost=10 gives the best results.
#The best model has training error = 0.14875 and test error = 0.14444
#while the radial kernel had a lower test error, the training error was higher

#####################
#### SECTION 2 Part a
#####################
rm(list=ls())
set.seed(5082)
# a) Preliminary Stuff:
#
K <- 3 # the number of classes
n <- 20 # the number of samples per class
p <- 50 # the number of variables
# Create data for class 1:
X1<-matrix(rnorm(n*p),nrow=n,ncol=p)
for( row in 1:n ){
  X1[row,]<-X1[row,]+1
}
# Create data for class 2:
X2<-matrix(rnorm(n*p),nrow=n,ncol=p)
for(row in 1:n){
  X2[row,]<-X2[row,]-1
}
# Create data for class 3:
X3<-matrix( rnorm(n*p), nrow=n, ncol=p )
for( row in 1:n ){
  X3[row,]<-X3[row,]+c(rep(1,p/2),rep(-1,p/2))
}
X<-rbind( X1, X2, X3 )
labels<-c(rep(1,n),rep(2,n),rep(3,n)) # the "true" labels of the points
#####################
#### SECTION 2 Part b
#####################
library(ggplot2)
data<-as.data.frame(X)
pca <- prcomp(data)
scores <- data.frame(labels, pca$x[,1:2])
#plot the first two principal component score vectors
qplot(x=PC1, y=PC2, data=scores, colour=factor(labels)) +
  theme(legend.position="none")

#####################
#### SECTION 2 Part c
#####################
#k-means with k=3
kmean.fit<-kmeans(X,3)
table(labels,kmean.fit$cluster)
#from the table we can see that each of the clusters produced by kmeans perfectly fits an actual class
#this fits the data very well
#####################
#### SECTION 2 Part d
#####################
#k-means with k=2
kmean.fit<-kmeans(X,2)
table(labels,kmean.fit$cluster)
#from this table we can see that the actual class 2 perfectly fits the kmeans cluster 1
#the actual classes 1 and 3 are combined into the kmeans cluster 2
#this is not as good a fit
#####################
#### SECTION 2 Part e
#####################
#k-means with k=4
kmean.fit<-kmeans(X,4)
table(labels,kmean.fit$cluster)
#this table shows that class 2 is matched by cluster 1 and class 3 is matched by cluster 4
#class 1, however, is split between clusters 2 and 3
#this is a poor fit for the actual classes
#####################
#### SECTION 2 Part f
#####################
#k-means with k=3 on the first two principal component score vectors
dat=pr.out$x[,1:2]
kmean.pc.fit<-kmeans(dat,3)
table(labels,kmean.pc.fit$cluster)
#the table shows us that the actual classes 1 and 3 are identified as one cluster by kmeans
#kmeans also splits class 2 into two different clusters
#####################
#### SECTION 2 Part g
#####################
#scale the data and perform k-means with k-3
X<-scale(X)
kmean.fit<-kmeans(X,3)
table(labels,kmean.fit$cluster)
#the results are essentially the same for both methods, only the kmeans cluster labels have changed
#the order is the only thing that has changed. 
