# PW6 MACHINE LEARNING - PCA

# Etudiants du groupe :
# Katell GOURLET
# Marion GATINOIS
# Lucie DREVET
# Ines NGUYEN
# Yan PODOLAK

#############Q1###############
#iris <- read.csv("C:/Users/DREVET/Downloads/iris.data")
#View(iris)

###################???Q2################

par(mfrow=c(2,2))
boxplot(sepal_length~class, data=iris, xlab='iris$class', ylab='sepal_length', col='orange', bordel='brown')
boxplot(sepal_width~class, data=iris, xlab='iris$class', ylab='sepal_length', col='orange', bordel='brown')
boxplot(petal_length~class, data=iris, xlab='iris$class', ylab='sepal_length', col='orange', bordel='brown')
boxplot(petal_width~class, data=iris, xlab='iris$class', ylab='sepal_length', col='orange', bordel='brown')

#################Q3###################

library(ggplot2) 

# histogram of sepal_length
ggplot(iris, aes(x=sepal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of sepal_width
ggplot(iris, aes(x=sepal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_length
ggplot(iris, aes(x=petal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_width
ggplot(iris, aes(x=petal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)

#############Q4################
pcairis=princomp(iris[,-5], cor=T)
str(pcairis)
summary(pcairis)
plot(pcairis)
biplot(pcairis)


##############Q5##################
library("factoextra")
#screeplot
fviz_eig(pcairis, choice = c("variance", "eigenvalue"), 
         geom = c("bar", "line"), barfill = "steelblue",
         barcolor = "steelblue", linecolor = "black", 
         ncp = 5, addlabels = FALSE)

#graph of individuals
fviz_pca_ind(pcairis, label='non',col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

#graph of variables
fviz_mca_var(pcairis, col.var='black')

#biplot of individuals and variables
fviz_pca_biplot(pcairis, repel=TRUE)

#variable contribution to the principal axes
fviz_contrib(pcairis, choice='var', axes=1)
fviz_contrib(pcairis, choice='var', axes=2)


##################Q6##################
x<-iris[,-5]
y<-iris[,5]


##################Q7##################
X_scaled=scale(x, center=TRUE, scale=TRUE)

##################Q8################
CovMatrix=cov(X_scaled, y=NULL, use='everything', method=c("pearson", "kendall", "spearman"))

###############Q9##################
eig=eigen(CovMatrix, only.values = FALSE, EISPACK = FALSE)
#$values
#[1] 2.91081808 0.92122093 0.14735328 0.02060771

#$vectors
#[,1]        [,2]       [,3]       [,4]
#[1,]  0.5223716 -0.37231836  0.7210168  0.2619956
#[2,] -0.2633549 -0.92555649 -0.2420329 -0.1241348
#[3,]  0.5812540 -0.02109478 -0.1408923 -0.8011543
#[4,]  0.5656110 -0.06541577 -0.6338014  0.5235463

###############Q10################
CorMatrix=cor(X_scaled, method = c("pearson", "kendall", "spearman"))
eigen(CorMatrix, only.values = FALSE, EISPACK = FALSE)

################Q11###############
CorMatrixRaw=cor(x, method = c("pearson", "kendall", "spearman"))
eigen(CorMatrixRaw, only.values = FALSE, EISPACK = FALSE)
#same value

##############Q12#################
eig.val<-get_eigenvalue(pcairis)
eig.val

##################Q13###########
fviz_eig(pcairis, addlabels = TRUE)

###############Q14############
projetcion_matrix=cbind(eig$vectors[1,], eig$vectors[2,])
projetcion_matrix

#############Q15##################
Y=X_scaled%*%projetcion_matrix


################Q16&17################

