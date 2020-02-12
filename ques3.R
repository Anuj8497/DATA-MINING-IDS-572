library(dplyr) # data aggregates 
library(gplots) # plot means with CI 

#a
College = read.csv("College.csv")
dummy = read.csv("College.csv")
str(College)

#There are a total of 777 observations of 19 variables

#b

rownames (College) -> College [,1]
College = subset(College, select = -c(X) )

#c

summary(College$Apps)
#Mean(3002) is greater than median(1558). Data is right skewed. Range is from 81 to 48094

summary(College$Accept)
#Mean(2019) is greater than median(1110). Data is right skewed. Range is from 71 to 26330

summary(College$Enroll)
#Mean(780) is greater than median(434). Data is right skewed. Range is from 35 to 6392

summary(College$Top10perc)
#Mean(27.56) is close to median(23).It is not completely normally distributed.It is skewed to right

summary(College$Top25perc)
#Median(54) is close to mean(55.8). It is close to being normally distributed.Range is 9 to 100

summary(College$F.Undergrad)
#Mean (3700) is greater tha median(1707). It is right skewed. range us from 39 to 31643

summary(College$P.Undergrad)
#Mean(855)is greater than median(353). It is right skewed. range is 1 to 21836

summary(College$Outstate)
#Mean is 10441, median is 9990. It is right skewed.Range is from 2340 to 21700.

summary(College$Room.Board)
#Mean(4358) is close to median(4200). It is close to being normally distributed. Range is 1780 to 8124

summary(College$Books)
#Mean(549) is greater than median(500). It is not normally distributed. Range is from 96 to 2340

summary(College$Personal)
#Mean(1341) is greater than median(1200). It is right skewed. Range is from 250 to 6800

summary(College$PhD)
#Median(75) is grater than mean(72.66). Data is left skewed.Range is 8 to 103

plot(density(College$Grad.Rate))
summary(College$Terminal)
#Median (82) is greater than mean(79.7). It is left skewed. Range is 24 to 100

summary(College$S.F.Ratio)
#MEan(14.09) is close to median(13.6). it is close to bein normally distributed. range is 2.5 to 39.8

summary(College$perc.alumni)
#Mean is 22.74, Median is 21. Range is 0 to 64

summary(College$Expend)
#Mean(9660) is greater than median(8377). It is right skewed. range is 3186 to 56233

summary(College$Grad.Rate)
#Mean(65.46) is close to median(65).It is close to being normally distributed.Range is 10 to 118

#d.
acceptRate<-(College$Accept/College$Apps)
College <- cbind(College, acceptRate)
dummy <- cbind(dummy, acceptRate)

#e.
# By most selective we mean selecting universities with lowest acceptance rate.

College1 <- dummy[order( acceptRate),]
top5 <- top_n(College1,-5)

# Now we print five most selective public institutions.
# Step 1: First subsetting only public institutions.

College2 <- dummy[which(College$Private == "No"),]

# Step 2: Selecting five most selective
top5_1 <- top_n(College2,-5)

#f.

table(College1$Private)
mean(College1$acceptRate[College1$Private=="Yes"])#0.754
mean(College1$acceptRate[College1$Private=="No"])#0.726
#Public institutions are more selective on average. Since average acceptance 
#rate of public universities is 72.6% and private institution is 75.4%

#g.

matricRate<-College$Enroll/College$Accept
College <- cbind(College1, matricRate)

#H.
plot(College$matricRate ~ College$acceptRate, col = c("red", "blue"), 
     main="Relationship of accept rate and enroll rate", 
     xlab="Accepted rate", 
     ylab=" Matric Rate", 
     pch=16) 
abline(lm(College$matricRate ~ College$acceptRate), col="coral", lwd=2.5)
lines(lowess(College1$Grad.Rate ~ College1$matricRate), col="green", lwd=2.5) 
legend("topright", fill= c("red","blue"),
       legend = c("Private", "Public"), 
       col = par("col"))


#I.
acc1 <- College$acceptRate[College$Private=="Yes"]
mat1 <- College$matricRate[College$Private=="Yes"]

acc2 <- College$acceptRate[College$Private=="No"]
mat2 <- College$matricRate[College$Private=="No"]

install.packages('corrplot')
library(corrplot)
cor.test(acc1,mat1)
#correlation between acceptance rate and matriculation of private institution is weakly correlated.cor=0.04
cor.test(acc2,mat2)
#correlation between acceptance rate and matriculation of public institution is weakly negatively correlated.cor= -0.064


#J
library(car) # advanced scatter plots 

scatterplotMatrix(~Apps+Accept+Enroll+Top10perc+Top25perc+F.Undergrad+P.Undergrad+Outstate+Room.Board+Books, data=College1, main="Correlations of Numeric Variables in the College Data")
#

#K

boxplot(College1$Outstate~College1$Private, col = c("green", "orange") )#public institutions has few outliers above upper threshold, and private institutions
#has very few outliers above upper threshold
legend("topright", fill= c("green","orange"),
       legend = c("Public", "Private"), 
       col = par("col"))
# We move along each row from left to right, to find relationship between the two variables.
# For example in case of Apps and Accept, the plot at position 1*2 represents the relations between the two.
# If the plot shows an uphill pattern from left to right, this indicates a positive relation.
# If the plot shows a downhill pattern from left to right, this indicates a negative relation.
# if the plot doesn't show any kind of pattern then no relationship exists.

#L
Elite<- rep ("No",nrow(College))
#Elite is the variable created. rep replicates the values in stated in first argument.(ie. No). the second argument is 
#number of times, which is Number of rows in this case.

Elite[College$Top10perc > 50] <- "Yes"
#Above code is used for binning Top 10 perc in two categories ie. above and below 50%. High school exceeding 50% is categorized to Yes
#Below 50% is categorized to NO

Elite <- as.factor(Elite)
#as.factor is used to convert Elite variable fro character to factor data type. It is categorized in two levels(ie. Yes and No)

College1 <- data.frame(College1,Elite)
#its is used to combine variables and create a single data frame. In this case we join College dataset and Elite variable.

summary(College1$Elite)
# There are 78 Elite Universities.

boxplot(College1$Outstate~College1$Elite, col = c("darkblue", "maroon"), xlab="Elite/Non Elite",ylab="Instate/Outstate" )
#Non-elite has few outliers above upper threshold, and elite universities has no outliers. 
legend("topright", fill= c("darkblue","maroon"),
       legend = c("No", "Yes"), 
       col = par("col"))


#M

par(mfrow=c(2,2))

hist(College1$Apps, col=c("steelblue", "red"), freq=F) 
hist(College1$Apps, col=c("steelblue", "red"), freq=F, breaks = 6) 

hist(College1$Accept, col=c("steelblue", "red"), freq=F) 
hist(College1$Accept, col=c("steelblue", "red"), freq=F, breaks = 6) 

hist(College1$Enroll, col=c("steelblue", "red"), freq=F)
hist(College1$Enroll, col=c("steelblue", "red"), freq=F, breaks = 6)

hist(College1$PhD, col=c("steelblue", "red"), freq=F) 
hist(College1$PhD, col=c("steelblue", "red"), freq=F, breaks = 6) 
