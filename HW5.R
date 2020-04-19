install.packages("readxl")
library(XLConnect)
library(readxl)
data<-"IMB579-XLS-ENG.xlsx"

sample.data$C.MANIPULATOR <- as.factor(sample.data$C.MANIPULATOR)

#Assigning excel sheets to individual datasets
manipulator<-readWorksheet(loadWorkbook(data),sheet=1) #39 obs of 11 variables
nonmanipulator<-readWorksheet(loadWorkbook(data),sheet=2) #1200 obs of 11 variables
complete.data<-readWorksheet(loadWorkbook(data),sheet=3) #1239 obs of 11 variables
sample.data<-readWorksheet(loadWorkbook(data),sheet=4) #220 obs of 11 variables

# Manipulator EDA
str(sample.data)
summary(manipulator)
#======================================================================================================================================================

# (c) Stepwise Logistic Regression model for predicting probability of earnings manipulation

library(ISLR)
# Using sample data with 220 cases including 39 manipulators, running logistic regression
attach(sample.data)
par(mfrow =c (2,2))
model <- glm(C.MANIPULATOR~DSRI+GMI+AQI+SGI+ACCR+LEVI+DEPI+SGAI,data=sample.data, family="binomial")
summary(model)
plot(model)
# The summary shows that LEVI, DEPI, SGAI have no statistical significance than the rest of the variables
# Therefore, we keep those variables out of our models from the models and their analysis

# Given that we have an unbalanced dataset, for better accuracy, apply an undersampling technique to balance the data
# Splitting the data in training and testing datasets

set.seed(1234)
index<-sample(2,nrow(sample.data),replace=TRUE,prob=c(0.8,0.2))
train<-sample.data[index==1,] 
test<-sample.data[index==2,]

bm <- glm(C.MANIPULATOR ~ DSRI+GMI+AQI+SGI+ACCR, data = train, family = "binomial")
options(scipen = 99)
exp(coef(bm))
summary(bm)

model1<-step(object = bm, direction = "both", trace=0) #stepwise regression by AIC 
summary(model1)

# After balancing the sample dataset, we observe GMI has a further lower statistical significance than the rest of the variables
# Of 11 variables, the variables of importance from stepwise regression model are DSRI, AQI, SGI and ACCR.

# ============================================================================================================================================================================

# (d) Cut-off probability and ROC model

prob<-predict(bm,test,type="response")
pred<-ifelse(prob>=0.5,1,0)
pred<-factor(pred,levels=c(0,1),order='TRUE')

library(ROCR)
library(pROC)
roc.small<-roc(test$C.MANIPULATOR,prob)
x <- 1-roc.small$specificities
y <- roc.small$sensitivities
auc.small<-roc.small$auc

ggplot(data=NULL,mapping=aes(x=x,y=y))+geom_line(colour='pink')+geom_abline(intercept=0,slope=1)+annotate('text',x=0.4,y=0.4,label=paste('AUC=',round(auc.small,digit=2)))+labs(x='False positive rate',y='sensitivity',title='ROC curve')

# ============================================================================================================================================================================================================================================================================================================

# (e) Youden’s index

confusion<-table(test$C.MANIPULATOR, pred)
confusion

specificity <- 34/(34 + 1)
sensitivity <-  2/(2 + 5)

youden <- sensitivity + specificity -1
youden # 0.2571429

# Cut-off point by Youden's index is 0.26

#-----------

# Cost based strategy
exp(coef(sample.data))

#============================================================================================================================================================================================================================================================================================================

# (f) suggest a M-score

mScore <- -4.84 + (0.920*DSR) + (0.528*GMI) + (0.404*AQI) + (0.892*SGI) + (0.115*DEPI) - (0.172*SGAI) + (4.679*ACCRUALS) - (0.327*LEVI)

#============================================================================================================================================================================================================================================================================================================


# (g) CART model

library(rpart)
library(rpart.plot)

cart<-rpart(C.MANIPULATOR~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data=train, method="class", parms = list(split = "gini")) #by using the sample one
manipulator.pred<-predict(cart,test,type='class') #without type, output is probability
rpart.plot(cart,type=3, extra =101)
cart

# root                178 32        No (0.8202247 0.1797753)  
# LEVI>=0.4978521     171 25        No (0.8538012 0.1461988)  
# SGI< 1.472588       155 16        No (0.8967742 0.1032258) 
# SGI>=1.472588       16  7         Yes (0.4375000 0.5625000) 
# LEVI< 0.4978521     7  0          Yes (0.0000000 1.0000000)


# The decision rules that can be observed are:
# If LEVI < 0.5, the company is likely to be a manipulator with 100% confidence
# If LEVI >= 1.5, SGI < 1.5, the company will not be a manipulator with confidence
# If LEVI >= 1.5, SGI >= 1.5,  the company is likely to be a manipulator with 56% confidence

cart1<-prune(cart, cp= cart$cptable[which.min(cart$cptable[,"xerror"]),"CP"])  
rpart.plot(cart1,type=3,extra=100) # The company will not be a manipulator 
summary(cart)
cart$cptable #confusion matrix
  
# =================================================================================================================================================================================================================================\

# (h) Regression model  using the complete data set (1200 non-manipulators and 39 manipulators)

set.seed(1234)
index.complete<-sample(2,nrow(complete.data),replace=TRUE,prob=c(0.8,0.2))
train.data<-complete.data[index.complete==1,] 
test.data<-complete.data[index.complete==2,]

bm.complete<-glm(C.MANIPULATOR~ DSRI+GMI+AQI+SGI+ACCR,data=train.data,family="binomial")
summary(bm.complete)

prob<-predict(bm.complete,test.data,type="response")
pred<-ifelse(prob>=0.5,1,0)
pred<-factor(pred,levels=c(0,1),order='TRUE')
confusion<-table(test.data$C.MANIPULATOR,pred)
confusion #confusion matrix

library(gmodels)
confusion.matrix<-CrossTable(test.data$C.MANIPULATOR,pred)

#ROC curve
library(pROC)
library(ROCR)
#pred.2<-prediction(predictions=as.matrix(pred),labels=test.data$C.MANIPULATOR))
roc.curve<-roc(test.data$C.MANIPULATOR,prob)
x<-1-roc.curve$specificities
y<-roc.curve$sensitivities

library(ggplot2)
auc<-roc.curve$auc
ggplot(data=NULL,
       mapping=aes(x=x,y=y))+geom_line(colour='red')+geom_abline(intercept=0,slope=1)+annotate('text',x=0.4,y=0.4,
      label=paste('AUC=',round(auc,digit=2)))+labs(x='False Positive Rate',y='True Positive Rate',title='ROC curve')

# Much much better ROC curve!!

#===================================================================================================================================================================================================================================================================================

# (i) Random Forest and Adaboosting

# Random forest
library(randomForest)
set.seed(300)
rf <- randomForest(factor(C.MANIPULATOR)~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data=sample.data, proximity = T, importance = T)
# OOB error is 9.09%
ans <- importance(rf, type = 1)
plot(rf, main = "Random Forest Plot")
plot(ans) # A plot of importance of the variables.  

#Top 3 variables of importance from Random Forest are: LEVI, SGI, ACCR, DSRI

#---------------

# Adaboosting

library(adabag)
adaboost<-boosting(C.MANIPULATOR~DSRI+GMI+AQI+SGI+DEPI+SGAI+ACCR+LEVI, data=sample.data, boos=TRUE, mfinal=20,coeflearn='Breiman')
summary(adaboost)
adaboost$trees
adaboost$weights
adaboost$importance
# ACCR       AQI      DEPI      DSRI       GMI      LEVI      SGAI       SGI 
# 15.827958  7.339082  7.694184 21.202729  8.718549 19.379824  8.790567 11.047107 

predict(adaboost,sample.data)
t1<-adaboost$trees[[1]]
library(tree)
plot(t1)
text(t1)

# #Top 3 variables of importance from Adaboosting model are: ACCR, DSRI, LEVI, SGI

# We can say that the top 4 variables for both Random Forest and Adaboost are the same: ACCR, DSRI, LEVI, SGI
# However, by the logistic regression, we see LEVI is not a variable of importance. ACCR, SGI, DSRI, GMI
# Per the CART, variable of importance are ACCR, LEVI and SGI

#===================================================================================================================================================================================================================================================================================

# (j) Final recommendation

# Variables ACCR, SGI and LEVI can be considered important for predicting earnings manipulators

