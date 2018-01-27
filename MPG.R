

library(tree);
library(gbm);
library(randomForest);
library(ISLR);

attach(Auto);
Auto=na.omit(Auto);
Auto=Auto[,1:8];
dim(Auto)
names(Auto)
summary(Auto)
?Auto

#Multiple Linear Regression

lm.fit=lm(mpg~.,data=Auto)
summary(lm.fit)

#Training and Test Set

set.seed(1)
train.Auto=sample(1:nrow(Auto),nrow(Auto)/2)
test.Auto=Auto[-train.Auto,"mpg"]


#Linear Regression Model

lm.fit<-lm(mpg~.,data=Auto[train.Auto,])
lm.predict<-predict(lm.fit,newdata=Auto[-train.Auto,])
mean((lm.predict-test.Auto)^2)

#Regression Tree

tree.Auto=tree(mpg~., data=Auto[train.Auto,])
summary(tree.Auto)
plot(tree.Auto)
text(tree.Auto,pretty=0)
tree.Auto
tree.pred=predict(tree.Auto,newdata=Auto[-train.Auto,])
mean((tree.pred-test.Auto)^2)


#Boosted Regression Tree

boost.Auto=gbm(mpg~.,data=Auto[train.Auto,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.Auto)
par(mfrow=c(2,2))
plot(boost.Auto,i="weight")
plot(boost.Auto,i="horsepower")
plot(boost.Auto,i="year")
yhat.boost=predict(boost.Auto,newdata=Auto[-train.Auto,],n.trees=5000)
mean((yhat.boost-test.Auto)^2)


#Specify Shrinkage lambda=0.02

boost.Auto2=gbm(mpg~.,data=Auto[train.Auto,],distribution="gaussian",n.trees=5000,interaction.depth=3,shrinkage=0.02,verbose=F)
yhat.boost2=predict(boost.Auto2,newdata=Auto[-train.Auto,],n.trees=5000)
mean((yhat.boost2-test.Auto)^2)

#Specify Shrinkage lambda=0.01

boost.Auto3=gbm(mpg~.,data=Auto[train.Auto,],distribution="gaussian",n.trees=5000,interaction.depth=3,shrinkage=0.01,verbose=F)
yhat.boost3=predict(boost.Auto3,newdata=Auto[-train.Auto,],n.trees=5000)
mean((yhat.boost3-test.Auto)^2)

#Random Forest with mtry = 6

rf.Auto=randomForest(mpg~.,data=Auto,subset=train.Auto, mtry=6, importance=TRUE)
yhat.rf=predict(rf.Auto,newdata=Auto[-train.Auto,])
mean((yhat.rf-test.Auto)^2)

#Random Forest with mtry = 3

rf.Auto1=randomForest(mpg~.,data=Auto,subset=train.Auto, mtry=3, importance=TRUE)
yhat.rf=predict(rf.Auto1,newdata=Auto[-train.Auto,])
mean((yhat.rf-test.Auto)^2)
