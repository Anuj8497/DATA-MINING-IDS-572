#importing libraries
library(corrplot)
library(dplyr)
library(gmodels)
library(gplots)
library(psych)
library(corrplot) # for correlation plot
library(Hmisc)
library(ggplot2) # for plots
library(ggthemes)
install.packages("corrplot")
library(corrplot)
# Assigning dataframe to the data variable
data <- read.csv("Auto.csv")

# Checking the data 
View(data) # There are some cases where "?" are present in the data. We need to remove them 

# we will first replace all "?" with "NA" and then remove all NA.
data[data == "?"] <- NA

# counting number of NA
sum(is.na(data)) # there are five instances of misisng values.

# removing all the missing values.
data <- na.omit(data)

View(data)	
# dataframe is now free of all missing values.

# (b)

# for quantitative we can write
is.numeric(datasetname$variablename)
# for qualitative we can write
is.factor(datasetname$variablename)

# Initially, mpg,cylinders, displacement, weight,acceleration, year, origin are numeric
# Horsepower and name are factors.

# Now by doing analysis finally whether variables are quantitative or qualitative is decided. 

# mpg. How to check it?
psych::describe(data$mpg) # mean and median both are close to one another indicating near normal distribution.
# Since mean and median are almost equal, it indicates near normal distribution.
# Let's plot density plot.
plot(density(data$mpg)) # Proves our speculation.
# Also we can perform mathematical operations on the mpg values and it will still hold relevance.
# hence mpg should be quantitative. 

# cylinders. How to check it?
psych::describe(data$cylinders)
# Negative value of kurtosis indicates there might be some abnormality in the curve.
# Let's plot density plot.
plot(density(data$cylinders)) # Proves our speculation.
# The plot has three peaks i.e. a tri-modal distribution
# It would be wise to check whether it can be qualitative or not. 
dummy <- as.factor(data$cylinders)
# We are getting five defined levels.hence cylinders should not be quantitative. Rather it should be qualitative.
# Converting cylinders into qualitative
data$cylinders <- as.factor(data$cylinders)

# displacement. How to check it?
psych::describe(data$displacement)
# Let's plot density plot.
plot(density(data$displacement)) 
range(data$displacement) # we cannot define displacement in less no of defined levels. Also when we perform mathematical
# operations on the displacement values it will hold some relevance. Hence displacement should be quantitative.

# horsepower. How to check it?
table(data$horsepower) # It would be wise to convert it into quantitative since we cannot segregagte the horsepower
# into less number of defined levels.
# Hence it should be quantitative. Again performing mathematical operations on the horsepower will hold some relevance.
data$horsepower <- as.numeric(data$horsepower)

# weight. How to check it?
psych::describe(data$weight)
summary(data$weight) # weight describes weight of cars, Different cars have different weights. Hence it should be numeric
# we cannot categorize it into defined levels. Arithmethic operation on the weight will hold relevance. 

# acceleration. How to check it?
psych::describe(data$acceleration)
summary(data$weight) # acceleration describes speed of cars, Different cars have different accelerations. Hence it should be numeric
# we cannot categorize it into defined levels. Arithmethic operation on the acceleration will hold relevance. 

summary(data$year)
plot(density(data$year))
# Performing arithmetic operations on the year variable will hold no relevance. Hence, year should be qualitative.
# converting year to qualitative
data$year <- as.factor(data$year)

summary(data$origin)
plot(density(data$origin)) # we se an abnormal curve. Hence it would be wise to convert origin into factor.
# Also performing artihtmetic operations on origin will hold no relevance.
# converting origin to qualitative
data$origin <- as.factor(data$origin)

#c

psych::describe(data$mpg)
# range for mpg is 37.6

psych::describe(data$displacement)
# range for displacement is 387

psych::describe(data$horsepower)
# range for horsepower is 92

psych::describe(data$weight)
# range for weight is 3527

psych::describe(data$acceleration)
# range for acceleration is 16.8


#d

# mean of mpg is 23.45. standard deviation is 7.81
# mean of displacement is 194.41. standard deviation is 104.64
# mean of horsepower is 52.16. standard deviation is 29.5
# mean of weight is 2977.58. standard deviation is 849.4
# mean of acceleration is 15.54. standard deviation is 2.76

#e

newdata <- data[-c(10:84),]

psych::describe(newdata$mpg) # range is 35.6 , mean is 24.37, standard deviation is 7.88.

psych::describe(newdata$displacement) # range is 387 , mean is 187.75, standard deviation is 99.94.

psych::describe(newdata$horsepower) # range is 92 , mean is 51.63, standard deviation is 29.73.

psych::describe(newdata$weight) # range is 3348 , mean is 2939.64, standard deviation is 812.65.

psych::describe(newdata$acceleration) # range is 16.3 , mean is 15.72, standard deviation is 2.69.

#f draw plots.

# lets analyze relation between no of cylinders and displacement

boxplot(data$displacement ~ data$cylinders, data=data, main="Effect of cylinders on engine displacement", xlab="No of cylinders", ylab="Displacement",col=c("orange", "lightblue4"))
# from the boxplots we can say that more the no of cylinders, more is the displacement produced by it.



# lets analyze relation between mpg and displacement

relation <- lm(mpg ~ displacement, data = data)
plot(data$mpg ~ data$displacement, col="lightgray", main="Relationship between mpg & displacement", xlab="Displacement", ylab="Miles per gallon", pch=16)
abline(relation, col = "coral" , lwd = 2.5)
# from the plot we see that vehicles which produce more engine displacement tend to have low fuel economy i.e mpg.

# lets analyze relation between no of cylinders and weight

boxplot(data$weight ~ data$cylinders, data=data, main="Effect of cylinders on the weight of the vehicle", xlab="No of cylinders", ylab="Weight",col=c("orange", "lightblue4"))
# from the boxplots we see that as no of cylinders increase the weight of the vehicle also increases.


# lets analyze relation between no of cylinders and miles per gallon

boxplot(data$mpg ~ data$origin, data=data, main="Relation between origin & miles per gallon", xlab="Origin", ylab="mpg",col=c("orange", "lightblue4"))
# From the boxplots we see that vehicles with origin 3 have mpg greater than those with origin 2 and origin 1.

# lets analyze the relation between horsepower and miles per gallon

relation1 <- lm(mpg ~ horsepower , data = data)
plot(data$mpg ~ data$horsepower, col="lightgray", main="Relationship between mpg & horsepower", xlab="Horsepower", ylab="Miles per gallon", pch=16)
abline(relation1, col = "coral" , lwd = 2.5)

# from the plots we can say that vehicles with more horsepower tend to have high fuel economy i.e mpg

# lets analyze the relation between weight and miles per gallon

relation2 <- lm(mpg ~ weight , data = data)
plot(data$mpg ~ data$weight, col="lightgray", main="Relationship between mpg & weight", xlab="Weight", ylab="Miles per gallon", pch=16)
abline(relation2, col = "coral" , lwd = 2.5)

# from the plot we can say that as weight of vehicle increases mpg decreases.
# this means that heavy vehicles are not fuel efficient.

relation3 <- lm(mpg ~ acceleration , data = data)
plot(data$mpg ~ data$acceleration, col="lightgray", main="Relationship between mpg & acceleration", xlab="Acceleration", ylab="Miles per gallon", pch=16)
abline(relation3, col = "coral" , lwd = 2.5)

# from the plots we see that miles per gallon increseases as acceleration increases.   

# g

# By looking at the plots we created in part f, we can say that displacement, weight, horsepower, cylinders, year 
# acceleration,year can be used to predict the mpg. But not sure.
# lets use regression to determine best predictors

model <- lm(mpg ~ cylinders+displacement+horsepower+weight+acceleration+year+origin,data = data)
model
summary(model)

par(mfrow=c(2,2))
plot(model)
# we do see some outliers. Lets remove them and plot a new model.


outliers <- c(387, 323, 328, 275, 245)
new <- data[-outliers,]


# now only variables have highest significance i.e '***' are selected. Since They are the strongest predictors 
# of the mpg. For the other factor variables we were getting high significance for only some levels. So we 
# completely removed it.

model1 <- lm(mpg ~ weight+year+origin, data= new)
summary(model1)

