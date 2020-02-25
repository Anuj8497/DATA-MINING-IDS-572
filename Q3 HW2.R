library(kernlab)
# Installing library kernlab for accessing dataset spam 

set.seed(1234)
data(spam)
View(spam)

#(a) 

# (1) What fraction of the e-mails are actually spam?
sum(spam$type=="spam")
1813/nrow(spam)
# 39% of the emails are actually spam

# (2) What should the constant classifier predict?
# Since most of the obserbvations are not spam, the constant classifier can be email


# (3) What is the error rate of the constant classifier?
sum(spam$type=="spam")/nrow(spam)
# The error rate for constant classifier is 39%

#**********************************************************************************************************************************************

# (b) Divide the data set into training set of 2301 rows and a testing set of 2300 rows.

# Training and test data 
training.rows = sample(1:nrow(spam),2301,replace=FALSE)
training.data = spam[training.rows,]
testing.data = spam[-training.rows,]
dim(training.data)
# 2301   58
dim(testing.data)
# 2300   58
intersect(rownames(training.data),rownames(testing.data)) 
# character(0) indicates no overlap between the two halves of training and test data
sum(training.data$type=="spam")/2301  # 0.3785311
# Since the samples would vary each time, the fraction of spam in each half would vary as well
# Considering current sample, about 38% of training data is spam
sum(testing.data$type=="spam")/2300  # 0.4095652
# Considering current sample, about 41% of test data is spam

# This is not a statistically significant difference.


#**********************************************************************************************************

# (c) 

library(tidyverse)
library(dplyr)
library(tree)
library(maptree)

# (1) Fit a classification tree to the training data and prune by cross-validation

spam.tree <- tree(factor(type) ~., data=training.data)
summary(spam.tree)
# Number of terminal nodes:  14
# Residual mean deviance:  0.4622 = 1057 / 2287 
# Misclassification error rate: 0.08518 = 196 / 2301 
# The summary here will tell us which variables are actually used in tree construction

# Using cross-validation to prune,
spam.tree.cv <- cv.tree(spam.tree,method="misclass")
# Plotting CV error vs tree size for default tree 
plot(spam.tree.cv, col="darkgreen", lwd = 2)
plot(spam.tree) # The default tree
text(spam.tree)

# Let's calculate error rate on test data
spam.tree.predictions <- predict(spam.tree, newdata = testing.data, type="class")
sum(spam.tree.predictions!= testing.data$type)/nrow(testing.data)
# The error rate on test data is 0.103913 (10.3%)
# The difference between error on test data and that on the training data (8%) is statistically significant, but not substantially huge.

spam.tree.big <- tree(factor(type) ~., data=training.data)
summary(spam.tree.big)

# # Constructing classification tree on testing data
spam.tree.pruned <- prune.tree(spam.tree.big, best=10)
summary(spam.tree.pruned)
# Variables actually used in tree construction:
# 1 charExclamation
# 2 charDollar
# 3 remove 
# 4 hp 
# 5 capitalLong
# 6 our
# 7 capitalAve
# 8 free
# 9 business
# 10 edu

# Number of terminal nodes:  15 
# Residual mean deviance:  0.4779 = 1093 / 2286 
# Misclassification error rate: 0.08518 = 196 / 2301

# Considering a case for low missclassification error and less complex classification tree

#-------------------------------------------

# (2a) Use bagging to fit an ensemble of 100 trees to the training data
library(mlbench)
library(ipred)
library(MASS)
spam.bag <- bagging(type ~ ., data = training.data, nfinal=100)
spam.bag

# Out-of-sample error (on testing data)



# (2b) Use Random Forest to fit an ensemble of 100 trees to the training data
library(randomForest)
rf <- randomForest(factor(type) ~ ., data = training.data, mtry = sqrt(ncol(training.data)-1), ntree = 100, proximity = T, importance = T)
# OOB estimate of  error rate: 5.13%
importance(rf, type = 1)
plot(rf, main = "Random Forest Plot")

# Confusion Matrix
#           nonspam spam class.error
# nonspam     1386   44  0.03076923
# spam          74   797 0.08495982

# The top 5 important variables here are: remove, charExclamation, capitalAve, charDollar, hp

# Plotting OOB error plot (additional code)
rf$err.rate[,1]
plot(rf$err.rate[,1], type = "l", main = "OOB error plot", col="orange", lwd =2)


#****************************************************************************************************************************

#Q4

# (1) What fraction of the spam e-mails in the training set did it not classify as spam?


# (2) What fraction of the genuine e-mails in the testing set did it classify as spam?



# (3) What fraction of e-mails it classified as spam were actually spam?
# From the confusion matrix, 797/(797 + 74) ~ 91.5%












