install.packages("caret", dependencies = TRUE)
install.packages("e1071", dependencies = TRUE)
install.packages("rlang")
install.packages("ROCR")

library("NHANES")
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(rlang)
library(ROCR)
library(pROC)

data <- glimpse(NHANES)
subset <- data %>%  select(SleepHrsNight,Depressed,SleepTrouble)

# removing na from the subset
subset <- na.omit(subset)

# spliting 70-30 for training and testing
set.seed(1234)
index <- sample(2, nrow(subset), replace = T, prob = c(0.7,0.3))
TrainData <- subset[index == 1, ]
TestData <- subset[index == 2, ]

# a
rfit = rpart(SleepTrouble ~ ., data = TrainData,
              method = "class",parms = list(split = "gini"),control = rpart.control(minsplit = 0, minbucket = 0 , cp = 0))

# b
rpart.plot(rfit,type = 3, under = TRUE)
summary(rfit)
rfit

# For CART since we use gini to measure impurity. Therefore we calculate gini index for for both SleepHrsNight, Depressed
# And for whichever predictor variable the gini index is lowest, we select that variable for the split. Lower value of 
# gini indicates more purer subsets. By viewing the plot of the tree, in all there are 12 terminal nodes.
# Nodes which are terminal are as follows.
# Depressed=None 3304  647 No (0.8041768 0.1958232) *
# SleepHrsNight< 11 253   67 No (0.7351779 0.2648221) *
# SleepHrsNight>=11 1    0 Yes (0.0000000 1.0000000) *
# SleepHrsNight< 7.5 348  119 No (0.6580460 0.3419540) *
# SleepHrsNight>=7.5 56   21 No (0.6250000 0.3750000) *
# SleepHrsNight< 6.5 68   31 No (0.5441176 0.4558824) *
# SleepHrsNight>=6.5 50   21 Yes (0.4200000 0.5800000) *
# SleepHrsNight< 10.5 4    1 No (0.7500000 0.2500000) *
# SleepHrsNight>=10.5 4    1 Yes (0.2500000 0.7500000) *
# SleepHrsNight< 9.5 20    6 Yes (0.3000000 0.7000000) *
# Depressed=None 384  158 No (0.5885417 0.4114583) *
# Depressed=Several,Most 198   70 Yes (0.3535354 0.6464646) *


# how to predict new observation
# We create a dataframe containg values of the new input case.
test <- data.frame(SleepHrsNight = 10 , Depressed = "None")
# we then feed this dataframe to the model trained.
predict(rfit, test)
#        No       Yes
#  0.8041768 0.1958232
# we can say that then when a person sleeps for 10 hours in night and is never depressed, then the chances of it 
# suffering from sleep trouble is no with a probability of 80%.

# c
# proportion of subject in the NHANES data having sleep trouble 
table(subset$SleepTrouble)
# 1729 cases out of 6660 suffer from SleepTrouble.

# d 
# 75% - 25% split
index1 <- sample(2, nrow(subset), replace = T, prob = c(0.75,0.25))
TrainData1 <- subset[index1 == 1, ]
TestData1 <- subset[index1 == 2, ]

# building the classification tree
rfit1 = rpart(SleepTrouble ~ ., data = TrainData1,
              method = "class",parms = list(split = "gini"),control = rpart.control(minsplit = 0, minbucket = 0 , cp = 0))


result = table(predict(rfit1, type = 'class', newdata = TestData1), TestData1$SleepTrouble)
answer <- predict(rfit1, type = 'prob', newdata = TestData1)

first <- table(answer[,2] >= 0.5,TestData1$SleepTrouble)
row.names(first) <- c("No","Yes")
first

second <- table(answer[,2] >=0.26, TestData1$SleepTrouble)
row.names(second) <- c("No","Yes")
second

# for cut point 0.5

# True positive rate = tp/tp+fn  = 73/73+347 = 0.174
# True negative rate = 1 - False positive rate =  0.953
# False positive rate = fp/fp+tn  = 57/57+1157 = 0.047
# False negative rate = 1 - true positive rate = 0.826
# Accuracy = tp+tn/total = 73+1157/1634 = 75.27%
 


# for cut point 0.26

# True positive rate = tp/tp+fn = 206/206+214 = 0.49
# True negative rate = 1 - False positive rate = 0.78
# False positive rate = fp/fp+tn  = 268/268+946 = 0.22 
# False negative rate = 1 - true positive rate = 0.51
# Accuracy = tp+tn/total =206+946/1634 = 70.5%

# Which values change and which values are the same for different cut-points? Explain.

# All the values changes when we change the cut-point from 0.5 to 0.26.


# e

answer_new <- predict(rfit, type = 'prob', newdata = TestData)
# to avoid padding in the plot we use the following
par(pty = "s")
roc(TestData$SleepTrouble, answer_new[,2], plot = TRUE, legacy.axes = TRUE , col = "#377eb8")
# to find the cut point to classify a person as having sleep trouble we us coords
object <- roc(TestData$SleepTrouble, answer_new[,2])
coords(object, x = "best", input="threshold", best.method="closest.topleft",transpose = TRUE)
# threshold specificity sensitivity 
# 0.2229116   0.7732877   0.4745098 

# we select the point as (0.226,0.4745). Because this point gives us optimal balance of true positive 
# and false positives
# Now this point means that 47% of all the instances suffering from sleeptrouble are predicted correctly and  
# 22.6% of instances are predicted incorrectly.


# f 

index2 <- sample(2, nrow(data), replace = T, prob = c(0.75,0.25))
TrainData2 <- data[index2 == 1, ]
TestData2 <- data[index2 == 2, ]


rfit2 = rpart(SleepTrouble ~ ., data = TrainData2,
              method = "class",parms = list(split = "gini"),control = rpart.control(minsplit = 0, minbucket = 0 , cp = 0))

a <- predict(rfit2, type = 'prob', newdata = TestData2)
par(pty = "s")
roc(TestData2$SleepTrouble, a[,2], plot = TRUE, legacy.axes = TRUE , col = "#377eb8")


b <- predict(rfit1, type = 'prob' , newdata = TestData1)
par(pty = "s")
roc(TestData1$SleepTrouble, b[,2], plot = TRUE, legacy.axes = TRUE , add = TRUE)

# add = TRUE creates both plots together.
# the classifier for which we consider all the variables has higher accuracy then the one for which we only considered
# two variables as the predictor variables
# area under the curve gives us the accuracy of the classifier

# for classifier with all the variables, 1484 were not having Sleep trouble and 486 were having Sleep trouble
# for classifier with only two variables 1214 were not having Sleep trouble and 420 were having Sleep trouble. 




