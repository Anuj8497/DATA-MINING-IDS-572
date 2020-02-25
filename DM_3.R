# Installing library kernlab for accessing dataset spam 
library(kernlab)
library(dplyr)

set.seed(1234)
data(spam)
View(spam)

#(a) 

table(spam$type)

# (1) What fraction of the e-mails are actually spam?
sum(spam$type=="spam")
1813/nrow(spam)
# 39% of the emails are actually spam

# (2) What should the constant classifier predict?
# Since most of the obserbvations are not spam, the constant classifier should predict email


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


sum(TrainData$type=="spam")/2301  # 0.3785311
# Since the samples would vary each time, the fraction of spam in each half would vary as well
# Considering current sample, about 38% of training data is spam

sum(testing.data$type=="spam")/2300  # 0.4095652
# Considering current sample, about 41% of test data is spam



#**********************************************************************************************************

# (c) 
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(tree)
install.packages("maptree")
library(maptree)

# (1) Fit a classification tree to the training data and prune by cross-validation

spam.tree <- tree(factor(type) ~., data=training.data)
summary(spam.tree)


# Number of terminal nodes:  12
# Residual mean deviance:  0.4548 = 1041 / 2289 
# Misclassification error rate: 0.08648 = 199 / 2301 
# The summary here will tell us which variables are actually used in tree construction

# Using cross-validation to prune,
spam.tree.cv <- cv.tree(spam.tree,method="misclass")


# Plotting CV error vs tree size for default tree 
plot(spam.tree.cv, col="darkgreen", lwd = 2)

# A plot of best tree 
plot(spam.tree)
text(spam.tree)

# Let's calculate error rate on test data
spam.tree.predictions <- predict(spam.tree, newdata = testing.data, type="class")
sum(spam.tree.predictions!= testing.data$type)/nrow(testing.data)
# The error rate on test data is 0.103913 (11.08%)
# The difference between error on test data and that on the training data (8%) is statistically significant, but not substantially huge.

# Variables which appear in the tree are 
# charDollar, remove, hp, charExclamation , edu, george, capitalAve, free, money, our. 

#-------------------------------------------

# Use bagging to fit an ensemble of 100 trees to the training data
library(mlbench)
library(ipred)
library(MASS)
spam.bag <- bagging(type ~ ., data = training.data, nfinal=100)
spam.bag


# Use Random Forest to fit an ensemble of 100 trees to the training data
library(randomForest)
rf <- randomForest(factor(type) ~ ., data = training.data, mtry = sqrt(ncol(training.data)-1), ntree = 100, proximity = T, importance = T)
# OOB estimate of  error rate: 5.26%
ans <- importance(rf, type = 1)

plot(rf, main = "Random Forest Plot")

# Confusion Matrix
#          nonspam spam class.error
#nonspam    1331   47  0.03410740
#spam         74  849  0.08017335 



# Reporting error rates of these methods on testing data.
error = 1 -(1331+849)/2301
# Error rate is 0.0569  i.e 5.26%

bag.predictions <- predict(spam.bag , newdata = testing.data , type = "class")
table(bag.predictions,testing.data$type)
errorr = 1 - (1341+783)/2301
# error rate is 0.0765 i.e 7.65%

# A plot of importance of the variables.
plot(ans)

# Since the error rate of both random forest and bagging is less than the error rate of the constant classifier
# Hence both of them outperform.

#****************************************************************************************************************************

#4
# Random forest has the lowest error rate. Therefore we take that.

# (1) What fraction of the spam e-mails in the training set did it not classify as spam?
# i.e false negatives.
# 47/(47+849) i.e 0.0524    
 
# (2) What fraction of the genuine e-mails in the testing set did it classify as spam?
# i.e. false positives
# 74/(74+1331) i.e. 0.0526  

# (3) What fraction of e-mails it classified as spam were actually spam?
# From the confusion matrix, 783/(783 + 69) ~ 91.90%