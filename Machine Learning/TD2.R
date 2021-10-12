# PW2 MACHINE LEARNING - Multiple Linear Regression

# Etudiants du groupe :
# Katell GOURLET
# Marion GATINOIS
# Lucie DREVET
# Ines NGUYEN
# Yan PODOLAK

############# Q.1 ############

# Load the Boston data :
library(MASS)
dim(Boston)


############# Q.2 ############

# Split the data into traning set and testing set :
train = 1:400
test = -train

variables = names(Boston)
# variables
training_data = Boston[train, variables]
testing_data = Boston[test, variables]

# Or : 
# training_data = Boston[train, ]
# testing_data = Boston[test, ]

dim(training_data)
# Result : 400  14 -> ok

# Display the data set :
#training_data
#testing_data 


############# Q.3 ############

# Check if there is a linear relationship between the variables medv and age :
# (use cor() function)

# Syntax of the function cor() :
# corelation = cor(training_data, y = NULL, use = "everything",
#                 method = c("pearson", "kendall", "spearman"))

# Give the same result as the function below but we need to sarch the value because
# calculate for all je caraible or we only want it betweew age and medv

corelation = cor(training_data$medv,training_data$age ,method = "pearson")
# We use pearson : corre lineaire 
# Display result :
corelation
#Result : [1] -0.278153

# Interprétation :
# If the corelation is :
# -> near 1 : strong positive corelation
# -> near -1 : strong opposite correlation
# -> near 0 : no correlation at all

# The result -0.278153 is closer to 0. We probably don't have a linear relationship 
# between this two variables



############# Q.4 ############

# Fit a model of housing prices in function of age and plot 
# the observations and the regression line.

# Check the dimensions of the new dataset
dim(training_data)
# Result : 400  14 -> ok

# Our task is to predict the median house value (medv) using only one predictor : age
# Scatterplot of age vs. medv
plot(training_data$age, training_data$medv, main = "check linearity model")
# We observe that it is quite linear


# MODEL CREATION : linear line
#Syntax :
# model = lm(formula = response ~ predictor, data = data)

model1 = lm(medv ~ age, data = training_data)

# Some data we can display : 
#summary(model1)
#names(model1)
#model1$coefficients
#interval de confiance
#confint(model1, level = 0.95)

# Scatterplot of age vs. medv
plot(training_data$age, training_data$medv)
# Add the regression line to the existing scatterplot
abline(model1)

# Same but with some colors :
# Plot the model With colors :
plot(training_data$age, training_data$medv,
     xlab = "Age",
     ylab = "Median House Value Model1",
     col = "red",
     pch = 20) 

# Make the line color blue, and the line's width =3 (play with the width!)
abline(model1, col = "blue", lwd =3)

#RESULTS OK, model1 seems quite good


############# Q.5 ############

# Train a regression model using both lstat and age
# Multiple Linear Regression syntax :
# fit <- lm(y ~ x1 + x2 + x3, data=mydata)
plot(training_data$age + log(training_data$lstat), training_data$medv)

model2 = lm(medv ~ log(lstat) + age, data = training_data)
#model2

# Plot the model (With colors) :
plot(training_data$age + log(training_data$lstat), training_data$medv,
     xlab = "age",
     ylab = "Median House Value Model 2",
     col = "red",
     pch = 20)

# Make the line color blue, and the line's width =3 (play with the width!)
# abline(model2, col = "blue", lwd =3)
# error with abline : utilisation des deux premiers des 3 coefficients de régression
# The model obtained is really bad (probably because or the error)

##### 3D PLOT ######

# The regression model is a plan not an equation.
library(rgl)
rgl::plot3d(log(Boston$lstat),Boston$age, Boston$medv, type = "p",)
rgl.snapshot("rgl.TD2.Q.5")
# Results ok



############# Q.6 ############

# Print the summary of the obtained regression model :
summary(model2)

# Desplay (here juste the 3 last lines) : 
# Residual standard error: 5.292 on 397 degrees of freedom
# Multiple R-squared:  0.6684,	Adjusted R-squared:  0.6667 
# F-statistic: 400.1 on 2 and 397 DF,  p-value: < 2.2e-16


############# Q.7 ############

# Is the model as a whole significant?
# Yes it is because p value are relly close (last line of the summary : p-value: < 2.2e-16)
##They all are significant because they have a Pr(>|t|) less than 5%

# creuse la disparité : residual standard error : sigma chapeau : estimate global variance 
# multiple r-squared : close to 1 : high prportion are explain by the input, 73% is explain 
# derniere p-value : modele est bon significant , cest le p-value global : fisher test : power to modelize dataset 
# black, indus, chas no significant



# ############ Q.8 ############

# Are the predictors significant ?

# R squared means that the proportion of predictors explain Y 
# global p-value < 5% confirm that the test de fisher confirm the 
# validity of the model

#Yes, all the predictors are under 5% and the p-value is very low
# so the predictors significant


############# Q.9 ############

# Train a new model using all the variables of the dataset :

model3 = lm(medv ~ ., data = training_data)
#model3
summary(model3)

# Plot the model With color :
plot(training_data$crim + training_data$zn + training_data$indus + training_data$chas +
       training_data$nox + training_data$rm + training_data$age + training_data$dis + 
       training_data$rad + training_data$tax + training_data$ptratio + training_data$black + 
       training_data$lstat,
     training_data$medv,
     ylab = "Median House Value Model 3",
     col = "red",
     pch = 20) 

# Make the line color blue, and the line's width =3 (play with the width!)
abline(model3, col = "blue", lwd =3)
# Warning message: utilisation des deux premiers des 14 coefficients de régression
# The regression model is a plan not an equation.

# The MLR is significative to modelize our data
# MUCH BETTER PREDICTION WITH MODEL 3 


############ Q.10 ############

# When using all the variables as predictors, we didn't transform lstat. Re train the model using log(lstat) instead of lstat
model4 <- lm(medv ~ . - lstat + log(lstat), data = training_data)
summary(model4)

# Plot the model With color :
plot(training_data$crim + training_data$zn + training_data$indus + training_data$chas +
       training_data$nox + training_data$rm + training_data$age + training_data$dis + 
       training_data$rad + training_data$tax + training_data$ptratio + training_data$black + 
       log(training_data$lstat), 
     training_data$medv,
     ylab = "Median House Value Model 4",
     col = "red",
     pch = 20) 

# Make the line color blue, and the line's width =3 (play with the width!)
abline(model4, col = "blue", lwd =3) 
# Warning message: utilisation des deux premiers des 14 coefficients de régression
# Ne marche pas sur plusieurs valeures

############ Q.11 ############

#Did R^2 improve ?
summary(model3)$r.squared  
#[1] 0.7338501
summary(model4)$r.squared # higher => better
#[1] 0.7849629

#Yes r^2 improuve 
# The new model is better, with log(stats)


############ Q.12 ############

# To see if there is correlated variables print the correlation matrix using the cor() function 
# (round the correlations with 2 digits)
round(cor(training_data),2)


############ Q.13 ############

# Visualize the correlations using the corrplot package :
# To Visualize the correlations, we need to use the corrplot package. 
# To do so, install the corrplot package, load it, 
# then use the function corrplot.mixed(). See this link  
# for examples and to understand how to use it.
install.packages("corrplot")
library(corrplot)


corrplot.mixed(cor(training_data))

# diag : variance 
# else : covariance 
# rad-tax : 0.87 high , near for 1 
# positive : 2 tend to increase or decrease  
# negative : 1 can increase ans second decresa 


############ Q.14 ############

# What is the correlation between tax and rad?
corelation = cor(tax, y = rad, use = "everything", method = c("pearson", "kendall", "spearman"))
corelation

#They are very positively correlated
# 0.87 is a high correlation


############ Q.15 ############

# Run the model again without tax :
model5 <- lm(medv ~ .-lstat + log(lstat) -tax, data=training_data )

summary(model5)
summary(model5)$r.squarred < summary(model4)$r.squarred

# What happens to the R^2 ?
# R^2 didn't change but the F-statistic increased

#we need to check for the model significiance gets higher with F-statistics)
#which means the p-values gets lower anh this model more significant

#r square don't change , decrease a little
#f stat increase because use minus variables than before
#better model because fstar increase black, indus, chas no significant


############ Q.16 ############

#  Calculate the mean squared error (MSE) for the last model :
# MSE : to calculate the accuracy of estimate ^y
# Save the testing median valuse
medv = testing_data$medv
y = testing_data$medv #ce qu'on veut predir

# Compute tje predicted value
y_hat = predict(model5, data.frame(testing_data))
# we have both y and y_hat for our testing data

# now find the square error : 
error = y-y_hat
error_squared = error^2
MSE = mean(error_squared)
MSE
#23.33963


############ Q.17 ############

# How many of the suburbs in this data set bound the Charles river?

str(training_data)
table(training_data$chas)
#0   1 
#365  35 
sum(training_data$chas == 1)
#answer : 35

#35 fois le nmb 1

#Marion ?
#erreur : utilise test data 


############ Q.18 ############

# Do we observe some difference between the median value of houses with 
# respect to the neighborhood to Charles River?
boxplot(medv ~ chas, data = training_data)

# Observation :
# Yes there is some difference. The average is quite the same but
# the inerquartile is much higher


############ Q.19 ############

#  Calculate  ??i and  ??j(in one line using the function aggregate()) :

aggregate(formula = medv ~ chas, data = training_data, FUN = mean)

# We observe a higher mean price if near river (28,4)


############ Q.20 ############

# Apply an ANOVA test of medv whith respect to chas (use the function aov()). 
# Print the result and the summary of it. what do you conclude ?

chas = testing_data$chas

anovatest = aov(medv~chas, data=training_data)
anovatest
summary(anovatest)
#There is a significative difference or price if it is near the river or nor


############ Q.21 ############

# Fit a new model where the predictors are the Charles River and the Crime Rate. 
model6 <- lm(medv ~ crim + chas, data=training_data )
summary(model6)


############ Q.22 ############

# Is chas is significant as well in the presence of more predictors?

# We have :
#Multiple R-squared:  0.09019,	Adjusted R-squared:  0.0856 
#F-statistic: 19.68 on 2 and 397 DF,  p-value: 7.11e-09
# R^2 is very low and p-value less small


############ Q.23 ############

##first order interaction ???

# Fit a model whith first order interaction term where predictors are lstat and age. 
# Print its summary.
model7 <- lm(medv ~ lstat + age, data=training_data )
summary(model7)


############ Q.24 ############

# Fit a model with all the first order interaction terms
#model8 <- lm(#??#, data=training_data )
#summary(model8)


############ Q.25 ############

#Compile a report based on your script.