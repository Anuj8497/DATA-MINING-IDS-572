install.packages("arules")
install.packages("arulesViz")

library(rpart)
library(party)
library(readxl)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(arules)
library(arulesViz)

# a 

# Importing the dataset

dataset <- read_excel("salary-class.xlsx")

# substituting all "?" with NA and then removing it.

dataset$AGE[dataset$AGE == "?"] <- NA
dataset$EMPLOYER[dataset$EMPLOYER == "?"] <- NA
dataset$DEGREE[dataset$DEGREE == "?"] <- NA
dataset$MSTATUS[dataset$MSTATUS == "?"] <- NA
dataset$JOBTYPE[dataset$JOBTYPE == "?"] <- NA
dataset$SEX[dataset$SEX == "?"] <- NA
dataset$`C-GAIN`[dataset$`C-GAIN` == "?"] <- NA
dataset$`C-LOSS`[dataset$`C-LOSS` == "?"] <- NA
dataset$HOURS[dataset$HOURS == "?"] <- NA
dataset$COUNTRY[dataset$COUNTRY == "?"] <- NA
dataset$INCOME[dataset$INCOME == "?"] <- NA
dataset$EMPLOYER[dataset$EMPLOYER == "?"] <- NA
dataset <- na.omit(dataset)

dataset$MSTATUS <- as.factor(dataset$MSTATUS)
dataset$EMPLOYER <- as.factor(dataset$EMPLOYER)
dataset$DEGREE <- as.factor(dataset$DEGREE)
dataset$JOBTYPE <- as.factor(dataset$JOBTYPE)
dataset$SEX <- as.factor(dataset$SEX)
dataset$COUNTRY <- as.factor(dataset$COUNTRY)
dataset$INCOME <- as.factor(dataset$INCOME)

# Spliting the dataset 60%-40%.

set.seed(1234)
ind <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.6, 0.4))
trainData <- dataset[ind==1,]
testData <- dataset[ind==2,]

# b 

# Method = class for classification tree and anova for regression tree.


rfit = rpart(INCOME ~ AGE + EMPLOYER + DEGREE + MSTATUS + JOBTYPE + SEX 
            + `C-GAIN` + `C-LOSS` + HOURS + COUNTRY, data = trainData,
            method = "class",parms = list(split = "gini"),control = rpart.control(cp = 0.01))

rpart.plot(rfit, type = 3, cex = 0.4)
printcp(rfit)
pred_Test_class <- predict(rfit, newdata = testData, type = "class")
mean(pred_Test_class != testData$INCOME)

summary(rfit)
# the leaf nodes can be determined by viewing the output with *. 
# In all there eight leaves in the tree.

# c

# Looking at the Variable importance we can say that MSTATUS, JOBTYPE and C-GAIN are the major predictors of the variable 
# We get this information from the Variable importance field in the summary(rfit)

# d
install.packages("tidyrules")
library("tidyrules")
rules <- tidyRules(rfit)

rules

# For > 50K
# MSTATUS %in% c('Divorced', 'Married-spouse-absent', 'Never-married', 'Separa~ >50K      161      0.982 3.89 
# MSTATUS %in% c('Married-AF-spouse', 'Married-civ-spouse') & JOBTYPE %in% c('~ >50K      188      0.974 3.86 
# MSTATUS %in% c('Married-AF-spouse', 'Married-civ-spouse') & JOBTYPE %in% c('~ >50K      146      0.973 3.86 

# For <= 50k
#  MSTATUS %in% c('Divorced', 'Married-spouse-absent', 'Never-married', 'Separa~ <=50K    9390      0.951 1.27 
# the other two rules don't meet the criteria.
# so the best two rules are as follows
# MSTATUS %in% c('Married-AF-spouse', 'Married-civ-spouse') & JOBTYPE %in% c('~ <=50K    4125      0.740 0.989
# MSTATUS %in% c('Married-AF-spouse', 'Married-civ-spouse') & JOBTYPE %in% c('~ <=50K    1722      0.560 0.749




# e

# Second decision tree
# We are not pruning this tree, allowing it to grow

rfit1 = rpart(INCOME ~ AGE + EMPLOYER + DEGREE + MSTATUS + JOBTYPE + SEX 
             + `C-GAIN` + `C-LOSS` + HOURS + COUNTRY, data = trainData,
             method = "class",parms = list(split = "gini"),control = rpart.control(minsplit = 0, minbucket = 0 , cp = 0))
rpart.plot(rfit)
printcp(rfit1)
pred_Test_class <- predict(rfit1, newdata = testData, type = "class")


# Third decision tree
# We are asigning 500 records to the parent branch and 100 records to the child branch.
rfit2 = rpart(INCOME ~ AGE + EMPLOYER + DEGREE + MSTATUS + JOBTYPE + SEX 
             + `C-GAIN` + `C-LOSS` + HOURS + COUNTRY, data = trainData,
             method = "class",parms = list(split = "gini"),control = rpart.control(minsplit = 500, minbucket = 100 , cp = 0.01))

rpart.plot(rfit2)
printcp(rfit2)
pred_Test_class <- predict(rfit2, newdata = testData, type = "class")
mean(pred_Test_class != testData$INCOME)
rfit2$cptable

# Calculating accuracies for the trees

pred_Test_class <- predict(rfit, newdata = testData, type = "class")
pred_Train_class <- predict(rfit, newdata = trainData, type = "class")

table(pred_Test_class,testData$INCOME)
# testing set accuracy is 84.79%
table(pred_Train_class,trainData$INCOME)
# training set accuracy is 84.45%

pred_Test_class_1 <- predict(rfit1, newdata = testData, type = "class")
pred_Train_class_1 <- predict(rfit1, newdata = trainData, type = "class")

table(pred_Test_class_1,testData$INCOME)
# testing set accuracy is 81%
table(pred_Train_class_1,trainData$INCOME )
# training set accuracy is 97.5%

pred_Test_class_2 <- predict(rfit2, newdata = testData, type = "class")
pred_Train_class_2 <- predict(rfit2, newdata = trainData, type = "class")

table(pred_Test_class_2,testData$INCOME)
# testing set accuracy is 84.79%
table(pred_Train_class_2,trainData$INCOME )
# training set accuracy is 84.45%

# for the third decision tree lets take cp = 0.131 and again plot the tree.

rfit2_new = rpart(INCOME ~ AGE + EMPLOYER + DEGREE + MSTATUS + JOBTYPE + SEX 
              + `C-GAIN` + `C-LOSS` + HOURS + COUNTRY, data = trainData,
              method = "class",parms = list(split = "gini"),control = rpart.control(minsplit = 500, minbucket = 100 , cp = 0.131))
pred_Test_class_2_new <- predict(rfit2_new, newdata = testData, type = "class")
pred_Train_class_2_new <- predict(rfit2_new, newdata = trainData, type = "class")
table(pred_Test_class_2_new,testData$INCOME)
# testing set accuracy is 80.8%
table(pred_Train_class_2_new,trainData$INCOME )
# training set accuracy is 81.43%

# for the third decision tree lets take cp = 0.039
rfit2_new_1 = rpart(INCOME ~ AGE + EMPLOYER + DEGREE + MSTATUS + JOBTYPE + SEX 
                  + `C-GAIN` + `C-LOSS` + HOURS + COUNTRY, data = trainData,
                  method = "class",parms = list(split = "gini"),control = rpart.control(minsplit = 500, minbucket = 100 , cp = 0.039))
pred_Test_class_2_new_1 <- predict(rfit2_new_1, newdata = testData, type = "class")
pred_Train_class_2_new_1 <- predict(rfit2_new_1, newdata = trainData, type = "class")

table(pred_Test_class_2_new_1,testData$INCOME)
# testing set accuracy is 81.72%
table(pred_Train_class_2_new_1,trainData$INCOME)
# training set accuracy is 82.43%



# What do we infer?
# The three trees differ briefly when it comes to overfitting. Since for the second decision tree i.e one with no pruning
# has accuracy of about ~97% when it comes to training set. But falls drastically when we check it for testing set.
# So if we do not perform pruning at all then overfitting occurs.

# Decision tree with no pruning is the most accurate on the training data.
# Decision trees i.e default and the third one when we keep cp = 0.01 is the most accurate on the testing data.
