x <- c(1,3,2,5)
x
#ans> [1] 1 3 2 5

x = c(1,6,2)
x
#ans> [1] 1 6 2
y = c(1,4,3)
length(x)
#ans> [1] 3
length(y)
#ans> [1] 3
x+y
#ans> [1]  2 10  5

ls()
#ans> [1] "x"  "x1" "x2" "x3" "x4" "y"
rm(x)
ls()
#ans> [1] "x1" "x2" "x3" "x4" "y"

###################################1.8.2 Vectors


# A handy way of creating sequences is the operator :
# Sequence from 1 to 5
1:5
#ans> [1] 1 2 3 4 5

# Storing some vectors
vec <- c(-4.12, 0, 1.1, 1, 3, 4)
vec
#ans> [1] -4.12  0.00  1.10  1.00  3.00  4.00

# Entry-wise operations
vec + 1
#ans> [1] -3.12  1.00  2.10  2.00  4.00  5.00
vec^2
#ans> [1] 16.97  0.00  1.21  1.00  9.00 16.00

# If you want to access a position of a vector, use [position]
vec[6]
#ans> [1] 4

# You also can change elements
vec[2] <- -1
vec
#ans> [1] -4.12 -1.00  1.10  1.00  3.00  4.00

# If you want to access all the elements except a position, use [-position]
vec[-2]
#ans> [1] -4.12  1.10  1.00  3.00  4.00

# Also with vectors as indexes
vec[1:2]
#ans> [1] -4.12 -1.00

# And also
vec[-c(1, 2)]
#ans> [1] 1.1 1.0 3.0 4.0

#################################1.8.3 Matrices, data frames and lists

# A matrix is an array of vectors
A <- matrix(1:4, nrow = 2, ncol = 2)
A
#ans>      [,1] [,2]
#ans> [1,]    1    3
#ans> [2,]    2    4

# Another matrix
B <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
B
#ans>      [,1] [,2]
#ans> [1,]    1    2
#ans> [2,]    3    4

# Binding by rows or columns
rbind(1:3, 4:6)
#ans>      [,1] [,2] [,3]
#ans> [1,]    1    2    3
#ans> [2,]    4    5    6
cbind(1:3, 4:6)
#ans>      [,1] [,2]
#ans> [1,]    1    4
#ans> [2,]    2    5
#ans> [3,]    3    6

# Entry-wise operations
A + 1
#ans>      [,1] [,2]
#ans> [1,]    2    4
#ans> [2,]    3    5
A * B
#ans>      [,1] [,2]
#ans> [1,]    1    6
#ans> [2,]    6   16

# Accessing elements
A[2, 1] # Element (2, 1)
#ans> [1] 2
A[1, ] # First row
#ans> [1] 1 3
A[, 2] # Second column
#ans> [1] 3 4

# A data frame is a matrix with column names
# Useful when you have multiple variables
myDf <- data.frame(var1 = 1:2, var2 = 3:4)
myDf
#ans>   var1 var2
#ans> 1    1    3
#ans> 2    2    4

# You can change names
names(myDf) <- c("newname1", "newname2")
myDf
#ans>   newname1 newname2
#ans> 1        1        3
#ans> 2        2        4

# The nice thing is that you can access variables by its name with the $ operator
myDf$newname1
#ans> [1] 1 2

# And create new variables also (it has to be of the same
# length as the rest of variables)
myDf$myNewVariable <- c(0, 1)
myDf
#ans>   newname1 newname2 myNewVariable
#ans> 1        1        3             0
#ans> 2        2        4             1

# A list is a collection of arbitrary variables
myList <- list(vec = vec, A = A, myDf = myDf)

# Access elements by names
myList$vec
#ans> [1] -4.12 -1.00  1.10  1.00  3.00  4.00
myList$A
#ans>      [,1] [,2]
#ans> [1,]    1    3
#ans> [2,]    2    4
myList$myDf
#ans>   newname1 newname2 myNewVariable
#ans> 1        1        3             0
#ans> 2        2        4             1

# Reveal the structure of an object
str(myList)
#ans> List of 3
#ans>  $ vec : num [1:6] -4.12 -1 1.1 1 3 4
#ans>  $ A   : int [1:2, 1:2] 1 2 3 4
#ans>  $ myDf:'data.frame': 2 obs. of  3 variables:
#ans>   ..$ newname1     : int [1:2] 1 2
#ans>   ..$ newname2     : int [1:2] 3 4
#ans>   ..$ myNewVariable: num [1:2] 0 1
str(myDf)
#ans> 'data.frame': 2 obs. of  3 variables:
#ans>  $ newname1     : int  1 2
#ans>  $ newname2     : int  3 4
#ans>  $ myNewVariable: num  0 1

# A less lengthy output
names(myList)
#ans> [1] "vec"  "A"    "myDf"


#######################1.8.4 Graphics

x=rnorm(100)
# The rnorm() function generates a vector of random normal variables,
# rnorm() with first argument n the sample size. Each time we call this
# function, we will get a different answer.
y=rnorm(100)
plot(x,y)

# with titles
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",
     main="Plot of X vs Y")

#############################1.8.5 Distributions

#Compute the 90%, 95% and 99% quantiles of a  F
#distribution with df1 = 1 and df2 = 5. (Answer: c(4.060420, 6.607891, 16.258177))
quantile = c(qf(p=0.90, df1=1, df2=5), qf(p=0.95, df1=1, df2=5), qf(p=0.99, df1=1, df2=5))
quantile

#Sample 100 points from a Poisson with lambda = 5.
samplePoisson = rpois(100,5)
samplePoisson

#Plot the density of a t distribution with df = 1 
#(use a sequence spanning from -4 to 4). Add lines of different colors 
#with the densities for df = 5, df = 10, df = 50 and df = 100.
x<- seq(-4,4, length.out=100)
y<- dt(x=x, df=1)
plot(x,y, type = "l")
lines(x, dt(x=x, df=5), col="blue")
lines(x, dt(x=x, df=10), col="red")
lines(x, dt(x=x, df=50), col="green")
lines(x, dt(x=x, df=100), col="purple")
?dt #aide sur la fonction dt


##############1.9 Regression
####################1.9.1 The lm function

load("EU.RData")

# lm (for linear model) has the syntax: 
# lm(formula = response ~ predictor, data = data)
# The response is the y in the model. The predictor is x.
# For example (after loading the EU dataset)
mod <- lm(formula = Seats2011 ~ Population2010, data = EU)

# We have saved the linear model into mod, which now contains all the output of lm
# You can see it by typing
mod
#ans> 
#ans> Call:
#ans> lm(formula = Seats2011 ~ Population2010, data = EU)
#ans> 
#ans> Coefficients:
#ans>    (Intercept)  Population2010  
#ans>       7.91e+00        1.08e-06

# mod is indeed a list of objects whose names are
names(mod)
#ans>  [1] "coefficients"  "residuals"     "effects"       "rank"         
#ans>  [5] "fitted.values" "assign"        "qr"            "df.residual"  
#ans>  [9] "na.action"     "xlevels"       "call"          "terms"        
#ans> [13] "model"

# We can access these elements by $
# For example
mod$coefficients
#ans>    (Intercept) Population2010 
#ans>       7.91e+00       1.08e-06

# The residuals
mod$residuals
#ans>        Germany         France United Kingdom          Italy 
#ans>         2.8675        -3.7031        -1.7847         0.0139 
#ans>          Spain         Poland        Romania    Netherlands 
#ans>        -3.5084         1.9272         1.9434         0.2142 
#ans>         Greece        Belgium       Portugal Czech Republic 
#ans>         1.8977         2.3994         2.6175         2.7587 
#ans>        Hungary         Sweden        Austria       Bulgaria 
#ans>         3.2898         2.0163         2.0575         1.9328 
#ans>        Denmark       Slovakia        Finland        Ireland 
#ans>        -0.8790        -0.7606        -0.6813        -0.7284 
#ans>      Lithuania         Latvia       Slovenia        Estonia 
#ans>         0.4998        -1.3347        -2.1175        -3.3552 
#ans>         Cyprus     Luxembourg          Malta 
#ans>        -2.7761        -2.4514        -2.3553

# The fitted values
mod$fitted.values
#ans>        Germany         France United Kingdom          Italy 
#ans>          96.13          77.70          74.78          72.99 
#ans>          Spain         Poland        Romania    Netherlands 
#ans>          57.51          49.07          31.06          25.79 
#ans>         Greece        Belgium       Portugal Czech Republic 
#ans>          20.10          19.60          19.38          19.24 
#ans>        Hungary         Sweden        Austria       Bulgaria 
#ans>          18.71          17.98          16.94          16.07 
#ans>        Denmark       Slovakia        Finland        Ireland 
#ans>          13.88          13.76          13.68          12.73 
#ans>      Lithuania         Latvia       Slovenia        Estonia 
#ans>          11.50          10.33          10.12           9.36 
#ans>         Cyprus     Luxembourg          Malta 
#ans>           8.78           8.45           8.36

# Summary of the model
sumMod <- summary(mod)
sumMod
#ans> 
#ans> Call:
#ans> lm(formula = Seats2011 ~ Population2010, data = EU)
#ans> 
#ans> Residuals:
#ans>    Min     1Q Median     3Q    Max 
#ans> -3.703 -1.951  0.014  1.980  3.290 
#ans> 
#ans> Coefficients:
#ans>                Estimate Std. Error t value Pr(>|t|)    
#ans> (Intercept)    7.91e+00   5.66e-01    14.0  2.6e-13 ***
#ans> Population2010 1.08e-06   1.92e-08    56.3  < 2e-16 ***
#ans> ---
#ans> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#ans> 
#ans> Residual standard error: 2.29 on 25 degrees of freedom
#ans>   (1 observation deleted due to missingness)
#ans> Multiple R-squared:  0.992,   Adjusted R-squared:  0.992 
#ans> F-statistic: 3.17e+03 on 1 and 25 DF,  p-value: <2e-16


###############1.9.2 Predicting House Value: Boston dataset

# First, install the MASS package using the command: install.packages("MASS")

# load MASS package
library(MASS)
#ans> Warning: le package 'MASS' a été compilé avec la version R 3.6.3

# Check the dimensions of the Boston dataset
dim(Boston)
#ans> [1] 506  14


# Split the data by using the first 400 observations as the training
# data and the remaining as the testing data
train = 1:400
test = -train

# Speficy that we are going to use only two variables (lstat and medv)
variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]

# Check the dimensions of the new dataset
dim(training_data)
#ans> [1] 400   2

# Scatterplot of lstat vs. medv
plot(training_data$lstat, training_data$medv)

# Scatterplot of log(lstat) vs. medv
plot(log(training_data$lstat), training_data$medv)

model = lm(medv ~ log(lstat), data = training_data)
model
#ans> 
#ans> Call:
#ans> lm(formula = medv ~ log(lstat), data = training_data)
#ans> 
#ans> Coefficients:
#ans> (Intercept)   log(lstat)  
#ans>        51.8        -12.2

summary(model)

names(model)
#ans>  [1] "coefficients"  "residuals"     "effects"       "rank"         
#ans>  [5] "fitted.values" "assign"        "qr"            "df.residual"  
#ans>  [9] "xlevels"       "call"          "terms"         "model"

model$coefficients
#ans> (Intercept)  log(lstat) 
#ans>        51.8       -12.2

confint(model, level = 0.95)
#ans>             2.5 % 97.5 %
#ans> (Intercept)  49.6   53.9
#ans> log(lstat)  -13.1  -11.3

# Scatterplot of lstat vs. medv
plot(log(training_data$lstat), training_data$medv)

# Add the regression line to the existing scatterplot
abline(model)

# Scatterplot of lstat vs. medv
plot(log(training_data$lstat), training_data$medv,
     xlab = "Log Transform of % of Houshold with Low Socioeconomic Income",
     ylab = "Median House Value",
     col = "red",
     pch = 20)

# Make the line color blue, and the line's width =3 (play with the width!)
abline(model, col = "blue", lwd =3)