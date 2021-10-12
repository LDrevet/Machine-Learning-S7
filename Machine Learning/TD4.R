# PW4 MACHINE LEARNING - DLA

# Etudiants du groupe :
# Katell GOURLET
# Marion GATINOIS
# Lucie DREVET
# Ines NGUYEN
# Yan PODOLAK

############################

setwd("D:/S7/Machine Learning")
getwd()
#Auto=read.table("Auto.data",header=T,na.strings ="?")
dataset = read.csv("Social_Network_Ads.csv")

#################Q.1############???
####Describing the dataset#######

str(dataset)
summary(dataset)
#--> Numerical variable : it shows min, max, mean, first and third quarter
#   Categorical variable : The number of observation in each categorie
#No sense for User.ID because it's random number


new_dataset=dataset[3:5]
str(new_dataset)
#we have deleted gender

library(caTools)
set.seed(703185)
split=sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set=subset(new_dataset, split==TRUE)
test_set=subset(new_dataset, split==FALSE)

training_set[-3] <- scale(training_set[-3])
test_set[-3]<-scale(test_set[-3])


classifier.logreg <- glm(Purchased ~ Age + EstimatedSalary , family = binomial, data=training_set)
classifier.logreg
summary(classifier.logreg)
#Logistic regression
#We can see that age have a bigger influence on purchased than estimated salary but both have a big influence anyway (p value<5% in each case)

pred.glm = predict(classifier.logreg, newdata = test_set[c(1,2)], type="response")
pred.glm
head(pred.glm)
#prediction

pred.glm_0_1 = ifelse(pred.glm >= 0.5, 1,0)
cm = table(pred.glm_0_1, test_set[,3])
cm
#We can see in the confusion matrix that there is 16 mistakes


######Decision Boundary of logistic Regression
#############Q.2#########

#X2=?????1/??2X1?????0/??2
slope=-coef(classifier.logreg)[2]/coef(classifier.logreg)[3]
intercept=-coef(classifier.logreg)[1]/coef(classifier.logreg)[3]

plot(test_set$Age, test_set$EstimatedSalary,xlab='Age', ylab='E.S.')
abline(intercept, slope, col="red",lwd=1)

##################Q3#########"
plot(test_set$Age, test_set$EstimatedSalary, pch=21, bg=ifelse(pred.glm_0_1==1,"red","blue"),main = "Decision boundary")
abline(intercept, slope, col="red",lwd=1)

#####################Q4##################
plot(test_set$Age, test_set$EstimatedSalary, pch=21, bg=ifelse(test_set$Purchased==1,"red","blue"),main = "Decision boundary")
abline(intercept, slope, col="red",lwd=1)
#we can count that there is 5 false positive like in the confusion matrix


#####################Q5##################
library(MASS)
classifier.lda=lda(Purchased~Age+EstimatedSalary, data=training_set)

######################Q6##############
classifier.lda
classifier.lda$prior
classifier.lda$means
#it computes the prior pribability of the 2 classes of Purchased, 
#the mean of each variable in each group and  the linear combination of predictor variables that
#are used to form the LDA decision rule

###############Q7###############
pred.lda = predict(classifier.lda, newdata = test_set[,-3], type="response")
str(pred.lda)


###############Q8###############
cm2=table(pred.lda$class, test_set[,3])
cm2
#one mistake less!

##############Q9###############???
X1 = seq(min(training_set[, 1]) - 1, max(training_set[, 1]) + 1, by = 0.01)
X2 = seq(min(training_set[, 2]) - 1, max(training_set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)

colnames(grid_set) = c('Age', 'EstimatedSalary')

plot(test_set[, 1:2],
     main = 'Decision Boundary LDA',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
points(test_set[1:2], pch = 21, bg = ifelse(test_set[, 3] == 1, 'green4', 'red3'))
pred_grid = predict(classifier.lda, newdata = grid_set)$class
contour(X1, X2, matrix(as.numeric(pred_grid), length(X1), length(X2)), add = TRUE)


##################Q10#############???
#1
class0=subset(training_set, training_set$Purchased==0)
class1=subset(training_set, training_set$Purchased==1)

#2
pi0=dim(class0)[1]/dim(training_set)[1]
pi1=dim(class1)[1]/dim(training_set)[1]
#OR
#pi0=classifier.lda$prior[1]
#pi1=classifier.lda$prior[2]

#3
mu0X1=mean(class0$Age)
mu0X2=mean(class0$EstimatedSalary)
mu1X1=mean(class1$Age)
mu1X2=mean(class1$EstimatedSalary)
mu0=rbind(mu0X1,mu0X2)
mu1=rbind(mu1X1,mu1X2)
#OR
#mu0=classifier.lda$means[1,]
#mu1=classifier.lda$means[2,]

#4
sigma=(((dim(class0)[1]-1)*cov(class0[-3]))+((dim(class1)[1]-1)*cov(class1[-3])))/(dim(class0)[1]+dim(class1)[1]-2)


#5
x=rbind(1,1.5)
delta0=t(x)%*%solve(sigma)%*%mu0-0.5*t(mu0)%*%solve(sigma)%*%mu0+log(pi0)
delta1=t(x)%*%solve(sigma)%*%mu1-0.5*t(mu1)%*%solve(sigma)%*%mu1+log(pi1)
#So the class prediction for this specific x is class 1 (Purchased=1)

#6
class02=subset(test_set, test_set$Purchased==0)
class12=subset(test_set, test_set$Purchased==1)

pi02=dim(class02)[1]/dim(test_set)[1]
pi12=dim(class12)[1]/dim(test_set)[1]
#OR
#pi0=classifier.lda$prior[1]
#pi1=classifier.lda$prior[2]

mu0X12=mean(class02$Age)
mu0X22=mean(class02$EstimatedSalary)
mu1X12=mean(class12$Age)
mu1X22=mean(class12$EstimatedSalary)
mu02=rbind(mu0X12,mu0X22)
mu12=rbind(mu1X12,mu1X22)
#OR
#mu0=classifier.lda$means[1,]
#mu1=classifier.lda$means[2,]

sigma2=(((dim(class02)[1]-1)*cov(class02[-3]))+((dim(class12)[1]-1)*cov(class12[-3])))/(dim(class02)[1]+dim(class12)[1]-2)


x=rbind(1,1.5)
delta02=t(x)%*%solve(sigma2)%*%mu02-0.5*t(mu02)%*%solve(sigma2)%*%mu02+log(pi02)
delta12=t(x)%*%solve(sigma2)%*%mu12-0.5*t(mu12)%*%solve(sigma2)%*%mu12+log(pi12)
#We find again that for this x, the class is 1 (Purchased=1)

###############Q11########

classifier.qda<-qda(Purchased ~., data=training_set)

#############Q12########
pred.qda = predict(classifier.qda, newdata = test_set[,-3], type="response")
cm3=table(pred.qda$class, test_set[,3])
cm3
#11 mistakes, again less than the previous prediction with lda!

###################Q13#########
X1 = seq(min(training_set[, 1]) - 1, max(training_set[, 1]) + 1, by = 0.01)
X2 = seq(min(training_set[, 2]) - 1, max(training_set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
# Adapt the variable names
colnames(grid_set) = c('Age', 'EstimatedSalary')

# plot 'Estimated Salary' ~ 'Age'
plot(test_set[, 1:2],
     main = 'Decision Boundary QDA',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

# color the plotted points with their real label (class)
points(test_set[1:2], pch = 21, bg = ifelse(test_set[, 3] == 1, 'green4', 'red3'))

# Make predictions on the points of the grid, this will take some time
pred_grid = predict(classifier.qda, newdata = grid_set)$class

# Separate the predictions by a contour
contour(X1, X2, matrix(as.numeric(pred_grid), length(X1), length(X2)), add = TRUE)


############Q14###########
require(ROCR)


score <- prediction(pred.glm,test_set[,3]) # we use the predicted probabilities not the 0 or 1
performance(score,"auc") # y.values

plot(performance(score,"tpr","fpr"),col="green")
abline(0,1,lty=8)

score.lda <- prediction(pred.lda$posterior[,2],test_set[,3]) # we use the predicted probabilities not the 0 or 1
performance(score.lda,"auc") # y.values

par(new=TRUE)
plot(performance(score.lda,"tpr","fpr"),col="blue")

score.qda <- prediction(pred.qda$posterior[,2],test_set[,3]) # we use the predicted probabilities not the 0 or 1
performance(score.qda,"auc") # y.values

par(new=TRUE)
plot(performance(score.qda,"tpr","fpr"),col="red")