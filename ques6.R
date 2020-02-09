library(rpart)
library(party)
library(readxl)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

remove(list = ls())
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

fit <- rpart(INCOME ~ AGE + EMPLOYER + DEGREE + MSTATUS + JOBTYPE + SEX + `C-GAIN` + `C-LOSS` + HOURS + COUNTRY,
             method="class",parms = list(split = "gini"),data=trainData)

rfit = rpart(INCOME ~ AGE + EMPLOYER + DEGREE + MSTATUS + JOBTYPE + SEX 
            + `C-GAIN` + `C-LOSS` + HOURS + COUNTRY, data = trainData,
            method = "class",parms = list(split = "gini"))

print(fit)
# the leaf nodes can be determined by viewing the output with *. 
# In all there eight leaves in the tree.

plot(fit)
rpart.plot(fit, type = 3, cex = 0.4)
summary(fit)



# c

# Looking at the Variable importance we can say that MSTATUS, JOBTYPE and C-GAIN are the major predictors of the variable 
# We get this information from the Variable importance field in the summary(fit)



# d

# Three rules who is likely to have an income > 50k
# IF MSTATUS = "adefg" and C.GAIN < 7140 THEN INCOME >50k
# IF MSTATUS = "adefg" and JOBTYPE = adfghijo and C.GAIN < 5096 THEN INCOME >50k
# IF MSTATUS = "adefg" and JOBTYPE = adfghijo and DEGREE = abcdefghilp and C.GAIN   
