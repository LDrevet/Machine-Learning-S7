# PW5 MACHINE LEARNING - DECSION TREE
# Lucie DREVET


#############################

#######Q1##########???
library(MASS)
library(caTools)
set.seed(18)
Boston_idx=sample(1:nrow(Boston), nrow(Boston)/2)
#?sample

Boston_train=Boston[Boston_idx,]
Boston_test=Boston[-Boston_idx,]

#############Q2###########
library(rpart)
Boston_tree<-rpart(medv~., Boston_train, method='anova')

#############Q3############???
plot(Boston_tree)
text(Boston_tree, pretty=0)
title(main="Regression Tree")

###############Q4############???
library(rpart.plot)
rpart.plot(Boston_tree)
#prp(Boston_tree)


###########Q5##########
print(Boston_tree)
summary(Boston_tree)
printcp(Boston_tree)
plotcp(Boston_tree)


##########Q5bis########
RMSE = function(m, o)
  {
  sqrt(mean((m - o)^2))
}

###########Q6############
pred.BT<-predict(Boston_tree, newdata = Boston_test[,-14], type = c("vector", "prob", "class", "matrix"))
tree_rmse=RMSE(pred.BT, Boston_test$medv)


##########Q7############.
LR<-lm(medv~., Boston_train)
pred.lm<-predict(LR, newdata = Boston_test[,-14], type="response")
lm_rmse=RMSE(pred.lm, Boston_test$medv)
#The perf of linear regression is better 

plot(pred.BT,Boston_test$medv, col = 'blueviolet' ,xlab='Predicted', ylab='Actual', main='Predicted vs Actual : Single Tree, Test Data')
abline(0,1, col='blue')

plot(pred.lm,Boston_test$medv, col = 'blueviolet' ,xlab='Predicted', ylab='Actual', main='Predicted vs Actual : Linear Model, Test Data')
abline(0,1, col='blue')


###########Q8#############
library(randomForest)
BM<-randomForest(medv~., data=Boston_train, mtry=13)

################Q9############???
pred.bagg<-predict(BM, newdata = Boston_test[,-14], type="response")
bagg_rmse=RMSE(pred.bagg, Boston_test$medv)
#Really better!!!
plot(BM, col='dodgerblue', main='Bagged Trees : Error vs Number of Trees')


##############Q10############
RF<-randomForest(medv~., data=Boston_train, mtry=4)
pred.randomforest<-predict(RF, newdata = Boston_test[,-14], type="response")
random_rmse=RMSE(pred.randomforest, Boston_test$medv)
#Less performant

###########Q11##########
importance(RF)
#Three best are lstat, rm, indus

###########Q12###########
varImpPlot(RF)


#########Boosting#########

#######Q10#########
library(gbm)
Boston_boost = gbm(medv ~ ., data = Boston_train, distribution = "gaussian", 
                   n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
Boston_boost

pred.boost<-predict(Boston_boost, newdata = Boston_test[,-14], n.trees=5000)
boost_rmse=RMSE(pred.boost, Boston_test$medv)
#Less performant

###########Q11########
summary(Boston_boost)


#############Q12############
boston_rmse = data.frame(
  Model = c("Single Tree", "Linear Model", "Bagging",  "Random Forest",  "Boosting"),
  TestError = c(tree_rmse, lm_rmse, bagg_rmse, random_rmse, boost_rmse)
)
kable(boston_rmse)

#or
plot(pred.BT,Boston_test$medv, col = 'blueviolet' ,xlab='Predicted', ylab='Actual', main='Predicted vs Actual : Single Tree, Test Data')
abline(0,1, col='blue')
plot(pred.bagg,Boston_test$medv, col = 'blueviolet' ,xlab='Predicted', ylab='Actual', main='Bagging, Test Data')
abline(0,1, col='blue')
plot(pred.randomforest,Boston_test$medv, col = 'blueviolet' ,xlab='Predicted', ylab='Actual', main='Random Forest, Test Data')
abline(0,1, col='blue')
plot(pred.boost,Boston_test$medv, col = 'blueviolet' ,xlab='Predicted', ylab='Actual', main='Boosting, Test Data')
abline(0,1, col='blue')


############CLASSIFICATION TREES###########
setwd("D:/S7/Machine Learning")
getwd()
#Auto=read.table("Auto.data",header=T,na.strings ="?")
dataset = read.csv("SPAM.csv")

str(dataset)
summary(dataset)

set.seed(703185)
data_idx=sample(1:nrow(dataset), nrow(dataset)/2)
data_train=dataset[data_idx,]
data_test=dataset[-data_idx,]

#Logistic regression model
log.rm<-glm(spam~., data=data_train, family="binomial")
pred.logrm=ifelse(predict(log.rm, data_test, "response")>0.5, "True","False")

Accuracy = function(m, o)
{
  TP=table(m, o)[1]
  TN=table(m, o)[4]
  FN=table(m, o)[2]
  FP=table(m, o)[3]
  
  Acc=(TP+TN)/(TP+TN+FP+FN)
}

LGM_acc=Accuracy(pred.logrm, data_test$spam)
LGM_acc

#Simple classification tree
classification_tree<-rpart(spam~., data = data_train, method = "class")
rpart.plot(classification_tree)

pred.classtree<-predict(classification_tree, data_test, "class")
table(predicted=pred.classtree, actual=data_test$spam)

tree_acc=Accuracy(pred.classtree, data_test$spam)
tree_acc

#Bagging
bagg<-randomForest(spam~., data=data_train, mtry=57)
pred.bagging=predict(bagg, data_test, "response")

Bagging_acc=Accuracy(pred.bagging, data_test$spam)  
Bagging_acc

#Random Forest
randomforest<-randomForest(spam~., data=data_train, mtry=sqrt(57))
pred.rf=predict(randomforest, data_test, "response")

RANDOM_acc=Accuracy(pred.rf, data_test$spam)  
RANDOM_acc

#Bosting

BOOSTING<-gbm(spam ~ ., data = data_train, distribution = "gaussian", 
              n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
pred.boosting=ifelse(predict(BOOSTING, data_test, n.trees=5000, "response")>0.5, "True","False")

Boosting_acc=Accuracy(pred.boosting, data_test$spam)  
Boosting_acc
