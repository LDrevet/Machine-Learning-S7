# PW3 MACHINE LEARNING - Multiple Linear Regression

# Etudiants du groupe :
# Katell GOURLET
# Marion GATINOIS
# Lucie DREVET
# Ines NGUYEN
# Yan PODOLAK

############# Q.1 ############

setwd("C:/Users/katel/OneDrive - De Vinci/ESILV_A4/Machine learning")
getwd()
#Auto=read.table("Auto.data",header=T,na.strings ="?")
network = read.csv("Social_Network_Ads.csv")


############# Q.2 ############
summary(network)
str(network)
#It is an analasy of the response of buying or not product (purshased 0 or 1)
#The M?an of The estimated salary is : 69743 
#We can compare if the salary influence the pursashed
# There are the columns : User ID,Gender,Age,EstimatedSalary,Purchased

#train = 2:401
#test = -train
variables = names(network)
variables
data = network[variables]
data

#corelation
cor(data$EstimatedSalary, data$Purchased, method="pearson")
#0.362083
#The corelation is really low : there is not important influence between these variables

cor(data$Purchased, data$Age, method="pearson")
#0.6224542
#The correlation is strong (>0.5) there is a positive correlation !

#exemple plot salary in function of the pursase
model1= lm(Purchased ~ Age, data = data)
model1
summary(model1)
#p-value: < 2.2e-16 : under 5% : the predictors are significants

plot(data$Age, data$Purchase)

plot(data$EstimatedSalary, data$Purchased)


### The value purchased is a binary variable.
### We need to used the logistic regression !


############# Q.3 ############


#install.packages('caTools')
library(caTools)
set.seed(123)

split=sample.split(network$Purchased, SplitRatio = 0.75)
training_set = subset(network, split == TRUE)
test_set = subset(network, split == FALSE)
split
training_set
test_set


############# Q.4 ############

scale(training_set$Age)
scale(training_set$EstimatedSalary)
scale(test_set$Age)
scale(test_set$EstimatedSalary)



############# Q.5 ############

model2= glm(Purchased ~ Age, family = "binomial",data = training_set)
model2
summary(model2)


plot(data$Age, data$Purchased)

B0 = -7.8807 
B1= 0.1861

# Draw the fitted logistic curve
x <- seq(0, 60)
y <- exp( -(B0 + B1*x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)



############# Q.6 ############

#We use the argument family as binomial in order to define the outcome
#as two possible types of values 
#In our cases, Purchased has two ?alue: 1 and 0

############# Q.7 ############


#B0 = -7.8807 
#B1= 0.1861 
p <- function(X){(exp(B0 + B1*(X))) / (1 + exp(B0 + B1*(X)) ) }

#Testes
p(40)
#0.3925276 : looks good
p(10) 
#0.002424506
  


############# Q.8 ############
summary(model2)
#We s?e that p-value of age is : p-value: < 2.2e-16 <5%
#This means that the variable age is significant to the model
#F-statistic: 400.1


############# Q.9 ############

AIC(model2)
#204.2479


############# Q.10 ############

plot(data$Age, data$Purchased)

B0 = -7.8807 
B1= 0.1861

# Draw the fitted logistic curve
x <- seq(0, 60)
y <- exp( -(B0 + B1*x))
y <- 1 / (1 + y)
lines(x, y, col = 2, lwd = 2)


############# Q.11 ############

model3= glm(Purchased ~ Age + EstimatedSalary, family = "binomial",data = tra?ning_set)
model3
summary(model3)



############# Q.12 ############

#P-values : 
#Intercept : 5.62e-14
#2.03e-12
#2.64e-07

#There are all under 5%, so the predictors are significant


############# Q.13 ############


#AIC model2:204.2479
#AIC model3: 16?.87

#When we compare the AIC of model2 and model 3, we see that the AIC of the model 2
#is higher that the one of the model3 so we can conclue that the model3, by adding the estimated salary
#is better


############# Q.14 ############

prob_pred =  predict(model3, newdata = test_set[c(3,4)], type="response")
prob_pred


############# Q.15 ############

y_pred = ifelse(prob_pred > 0.5, 1,0)
y_pred


############# Q.16 ############

cm = table(test_set[,5], y_pred)
cm

#Here is the confusion matrix y_pred :
#L/C:0  1
#0  94 11
#1  17 38

#We notice that there are11+17=28 errors.
#For exemple, for the 11, its said that there are 11 products purchased, 
#but in reality this 11 are not.
#Same for the 17: it is said there are 17 not purchased but this 17 are purch?sed.
#However, it find correctly that 94 was not purchased and that 38 was purchased.



########### Q.17 ############

#We used the confusion matrix 
TN = cm[1,1]
FP = cm[1,2]
FN = cm[2,1]
TP = cm[2,2] 

AUC =(TP+TN)/(TP+TN+FP+FN)
AUC
#0.825

Sensibility = TP/(TP+FN)
Sensibility
#0.6909091

Specificity = TN/(TN+FP)
Specificity
#0.8952381

precision = TP/(TP+FP)
precision
#0.7755102

Recall = TN/(TN+FN)
Recall
#0.8468468




##############Q.18########

#install.packages("ROCR")

library(ROCR)
score = prediction(prob_pred, test_set[,5])
plot(performance(score,"tpr","fpr"), col ="red")
abline(0,1,lty=8)


############# Q.19 ############

prob_pred2 =  predict(model2, newdata = test_set[c(3,4)], type="response")
prob_pred2


score2 = ?rediction(prob_pred2, test_set[,5])

plot(performance(score2,"tpr","fpr"), col ="blue")
abline(0,1,lty=8)

#2 curve in the same plot
plot(performance(score2,"tpr","fpr"), col ="blue")
abline(0,1,lty=8)
par(new=TRUE)
plot(performance(score,"tpr","fpr"), col="red")

#The best model is : the model3 (red curve)
#Because the curve is closer to the upper left corner of the plot






