setwd("//file/UsersW$/wzh110/Home/My Documents/462/ass 3")
train = read.csv(file = 'carseatsTrain.csv')
test = read.csv(file = 'carseatsTest.csv')
library(MASS)
library(tree)
library(ISLR)
attach(Carseats)
tree.fit = tree(Sales~., train)
summary(tree.fit)
plot(tree.fit)
text(tree.fit, pretty = 0, cex = 0.5)
train.pred = predict(tree.fit, train)
train.mse = mean((train$Sales-train.pred)^2)
train.mse
test.pred = predict(tree.fit, test)
test.mse = mean((test$Sales-test.pred)^2)
test.mse

set.seed(10)
cv.fit=cv.tree(tree.fit)
plot(cv.fit$size, cv.fit$dev, main = "The relationship between CVdev and cvsize",type="b")
#choose the best one
prune.fit = prune.tree(tree = tree.fit, best = 11)
plot(prune.fit)
text(prune.fit, pretty = 0, cex = 0.5)
#after prunning
prune.train.pred = predict(prune.fit, train)
prune.train.mse = mean((train$Sales-prune.train.pred)^2)
prune.train.mse
prune.test.pred = predict(prune.fit, test)
prune.test.mse = mean((test$Sales-prune.test.pred)^2)
prune.test.mse
#Bagging
library(randomForest)
set.seed(10)
bag.fit = randomForest(Sales~., data = train, mtry = 3 , ntree = 1000 ,importance = TRUE)
bag.fit
#after bagging
bag.train.pred = predict(bag.fit, train)
bag.train.mse = mean((train$Sales-bag.train.pred)^2)
bag.train.mse
bag.test.pred = predict(bag.fit, test)
bag.test.mse = mean((test$Sales-bag.test.pred)^2)
bag.test.mse
#randomForest
rf.fit = randomForest(Sales~., data = train, importance = TRUE)
rf.fit
#after rf
rf.train.pred = predict(rf.fit, train)
rf.train.mse = mean((train$Sales-rf.train.pred)^2)
rf.train.mse
rf.test.pred = predict(rf.fit, test)
rf.test.mse = mean((test$Sales-rf.test.pred)^2)
rf.test.mse
#d
library(gbm)
set.seed(10)
boost.fit=gbm(Sales ~ . ,data=train, distribution="gaussian", n.trees = 20000, shrinkage = 0.001, cv.folds=5)
best.iter = gbm.perf(boost.fit,method='cv')
best.iter
summary(boost.fit)
boost.fit=gbm(Sales ~ . ,data=train, distribution="gaussian", n.trees = best.iter, shrinkage = 0.001, interaction.depth = 1)
summary(boost.fit)
boost.train.pred = predict(boost.fit, train, n.trees=best.iter)
boost.train.mse = mean((train$Sales-boost.train.pred)^2)
boost.train.mse
boost.test.pred = predict(boost.fit, test, n.trees=best.iter)
boost.test.mse = mean((test$Sales-boost.test.pred)^2)
boost.test.mse
#shrinkage = 0.005
boost.fit=gbm(Sales ~ . ,data=train, distribution="gaussian", n.trees = 5000, shrinkage = 0.005, cv.folds=5)
best.iter = gbm.perf(boost.fit,method='cv')
best.iter
summary(boost.fit)
boost.fit=gbm(Sales ~ . ,data=train, distribution="gaussian", n.trees = best.iter, shrinkage = 0.005, interaction.depth = 1)
summary(boost.fit)
boost.train.pred = predict(boost.fit, train, n.trees=best.iter)
boost.train.mse = mean((train$Sales-boost.train.pred)^2)
boost.train.mse
boost.test.pred = predict(boost.fit, test, n.trees=best.iter)
boost.test.mse = mean((test$Sales-boost.test.pred)^2)
boost.test.mse
#shrinkage = 0.01
boost.fit=gbm(Sales ~ . ,data=train, distribution="gaussian", n.trees = 1000, shrinkage = 0.01, cv.folds=5)
best.iter = gbm.perf(boost.fit,method='cv')
best.iter
summary(boost.fit)
boost.fit=gbm(Sales ~ . ,data=train, distribution="gaussian", n.trees = best.iter, shrinkage = 0.01, interaction.depth = 1)
summary(boost.fit)
boost.train.pred = predict(boost.fit, train, n.trees=best.iter)
boost.train.mse = mean((train$Sales-boost.train.pred)^2)
boost.train.mse
boost.test.pred = predict(boost.fit, test, n.trees=best.iter)
boost.test.mse = mean((test$Sales-boost.test.pred)^2)
boost.test.mse

#question..??plot(lambdas, test.errors, type="b", xlab = "Number of Trees", ylab="Test MSE", ylim=c(8,12))
##question 3
library(cluster)
A3data = read.csv(file = 'A3data2.csv', header = TRUE)
km.out = kmeans(A3data[, 1:2], 3, nstart = 100)
plot(A3data[, 1:2], col=(km.out$cluster), main="K-Means Clustering Results with K=3", pch =20)
legend("topright", legend = c("cluster 1", "cluster 2","cluster 3"), col =c(1,2,3), pch = 20)
#hc complete
hc.complete = hclust(dist(A3data[, 1:2]), method="complete")
plot(hc.complete, main = "Complete Linkage")                   
plot(A3data[, 1:2], col=(cutree(hc.complete, 3) ), main="The  hierarchical clustering with complete linkage and Euclidean distance k=3", pch =20)
legend("topright",
       legend = c("cluster 1", "cluster 2","cluster 3"),
       col = c(1, 2, 3), pch = 20)
#hc single
hc.single=hclust(dist(A3data[, 1:2]),method="single")
plot(hc.single, main="Single Linkage", labels=FALSE,cex=0.5)
plot(A3data[, 1:2], col=(cutree(hc.single, 3) ), main="The  hierarchical clustering with single linkage and Euclidean distance k=3", pch =20)
legend("topright",
       legend = c("cluster 1", "cluster 2","cluster 3"),
       col = c(1, 2, 3), pch = 20)
table(km.pred = km.out$cluster, actual.label = A3data$Cluster)
table(complete.pred = cutree(hc.complete, 3), actual.label = A3data$Cluster)
table(single.pred = cutree(hc.single, 3), actual.label = A3data$Cluster)
#rescale
Rescale3=scale(A3data[, 1:2], center=TRUE, scale=TRUE)
# apply k-means
set.seed(10)
Rescale3.km.out=kmeans(Rescale3,3, nstart=20)
plot(Rescale3, col = (Rescale3.km.out$cluster), main="K-Means Clustering Results with K=3 in rescaled data", pch =20)
legend("topright", legend = c("cluster 1", "cluster 2","cluster 3"), col = c(1, 2, 3), pch = 20)
# rescale HC complete
rescaled.complete = hclust(dist(Rescale3), method="complete")
plot(rescaled.complete, main = "The hc with complete Linkage in rescaled") 
plot(Rescale3, col = (cutree(rescaled.complete, 3) ), main=" HC Results with complete linkage and Euclidean distance k=3 in rescaled data", pch =20)
legend("topright",
       legend = c("cluster 1", "cluster 2","cluster 3"),
       col = c(1, 2, 3), pch = 20)
# rescale HC single
rescaled.single = hclust(dist(Rescale3), method="single")
plot(rescaled.single, main = "The hc with single Linkage in rescaled") 
plot(Rescale3, col = (cutree(rescaled.single, 3) ), main=" HC Results with single linkage and Euclidean distance k=3 in rescaled data", pch =20)
legend("topright",
       legend = c("cluster 1", "cluster 2","cluster 3"),
       col = c(1, 2, 3), pch = 20)
###
###Q4
setwd("//file/UsersW$/wzh110/Home/My Documents/462")
train.bank = read.csv("BankTrain.csv", header = TRUE)[,c("x1","x3","y")]
train.bank$y=as.factor(train.bank$y)
test.bank = read.csv("Banktest.csv", header = TRUE)[,c("x1","x3","y")]
test.bank$y=as.factor(test.bank$y)
plot(train.bank[, c('x1', 'x3')], col = train.bank$y, main = ' The class of train data')
legend("topright", legend = c("Forged banknote", "Genuine Banknote"), pch = c(1, 1), col = c(2, 1), text.col = "black")
#overlapped
library(e1071)
set.seed(10)
tune.out = tune(svm, y ~ ., data = train.bank, kernel="linear", ranges=list(cost=c(0.01,0.1,1,10,100,1000)))
summary(tune.out)
tune.out$best.parameters
bestmodel=tune.out$best.model
plot(bestmodel, test.bank)
summary(bestmodel)

summary(bestmodel, test.bank)
test.pred = predict(bestmodel, test.bank)
table(test.pred, test.bank$y)
##radial
set.seed(10)
tune.out = tune(svm, y ~ ., data = train.bank, kernel="radial", ranges=list(cost=c(0.1,1.5,10,50,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)
tune.out$best.parameters
bestmod=tune.out$best.model
summary(bestmod)
plot(bestmod, test.bank)
ytest = predict(bestmod, test.bank)
table(predict=ytest, truth=test.bank$y)

