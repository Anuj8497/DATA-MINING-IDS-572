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
library(feather)
dataset = read.csv("gun_deaths.csv")
dummy = dataset
# a 

# For determining the no of deaths by month we first need to convert the months into qualitative.

dataset$month <- as.factor(dataset$month)
new <- data.frame(summary(dataset$month))

#output
 
#  1    2    3    4    5    6    7    8    9   10   11   12 
# 8273 7093 8289 8455 8669 8677 8989 8783 8508 8406 8243 8413 

dummy$label[dummy$month==1]<-"Jan"

# b

# Adding new column in the dataset named label. It store month name for each corresponding month.

dataset$label[dataset$month==1]<-"Jan"
dataset$label[dataset$month==2]<-"Feb"
dataset$label[dataset$month==3]<-"Mar"
dataset$label[dataset$month==4]<-"Apr"
dataset$label[dataset$month==5]<-"May"
dataset$label[dataset$month==6]<-"Jun"
dataset$label[dataset$month==7]<-"Jul"
dataset$label[dataset$month==8]<-"Aug"
dataset$label[dataset$month==9]<-"Sept"
dataset$label[dataset$month==10]<-"Oct"
dataset$label[dataset$month==11]<-"Nov"
dataset$label[dataset$month==12]<-"Dec"

# now lets create a bar plot
options(scipen = 99)

x <- table(as.factor(dataset$id),dataset$label)
barplot(x)
dev.off()

# c

y <- data.frame(dataset$id, dataset$intent)
summary(y)

z <- data.frame(aggregate(dataset$id ~ dataset$intent, y, sum))

var <- z[order(-z$dataset.id),c(1,2)]

ggplot(data = var, mapping = aes(x =var$dataset.intent, y = var$dataset.id))+geom_bar(stat ="identity" , fill = "red")

# plot with unarranged bars. 
ggplot(var,aes(var$dataset.intent,var$dataset.id))+geom_bar(stat = "identity", fill = "red")

# using order function with -ve sign we arrange the bars from highest to lowest. But the x ticks gets
# replaced by numbering 1,2,3,4 since we used the order function.

ggplot(var,aes(order(-var$dataset.intent),var$dataset.id))+geom_bar(stat = "identity", fill = "red")

# d

boxplot(dataset$age ~dataset$sex, data=dataset, main="Age of gun death victims by sex", xlab="Sex", ylab="Age",col=c("orange", "lightblue4"))
# following line of code gives us only gun deaths of female.

female <- dataset[ which(dataset$sex=='F'), ]

# now lets calculate average age of female gun deaths
female <- na.omit(female)
ans  <- mean(female$age)

summary(dataset$education)

# e 

ans <- count(dataset[ which(dataset$education=='HS/GED' & dataset$race =='White' & dataset$year == 2012 & dataset$sex == "M"), ])

# 7912 were the no of deaths in 2012 who were male, white with atleast a high school education.

# f

# creating a new variable named seaso storing the season associated with each month

dataset$season[dataset$label=="Jan" | dataset$label == "Feb" | dataset$label == "Mar"]<-"Winter"
dataset$season[dataset$label=="Apr" | dataset$label == "May" | dataset$label == "Jun"]<-"Spring"
dataset$season[dataset$label=="Jul" | dataset$label == "Aug" | dataset$label == "Sept"]<-"Summer"
dataset$season[dataset$label=="Oct" | dataset$label == "Nov" | dataset$label == "Dec"]<-"Fall"

max(table(dataset$season))

# from the table it is clear that Summer is the season with most gun deaths.

# g

# Are whites who are killed by guns more likely to die because of suicide or homicide? How does
# this compare to blacks and Hispanics?

# In case of Whites

count(dataset[ which(dataset$intent=='Suicide' & dataset$race =='White'), ])
#55372
count(dataset[ which(dataset$intent=='Homicide' & dataset$race =='White'), ])
#9147

55372/100798 # i.e. 54.9%
9147/100798 # i.e 9.07 %

# Hence in case of white gun violences are more likely to happen due to suicide.

# In case of Blacks

count(dataset[ which(dataset$intent=='Suicide' & dataset$race =='Black'), ])

# 3332

count(dataset[ which(dataset$intent=='Homicide' & dataset$race =='Black'), ])

# 19510

3332/100798 # i.e. 3.3%
19510/100798 # i.e 19.35%

# Hence in case of blacks gun violences are more likely to happen due to homicide

# In case of Hispanics

count(dataset[ which(dataset$intent=='Suicide' & dataset$race =='Hispanic'), ])

# 3171

count(dataset[ which(dataset$intent=='Homicide' & dataset$race =='Hispanic'), ])

# 5634

3171/100798 # i.e. 3.1%
5634/100798 # i.e 5.5%

# Hence in case of Hispanic gun violences are more likely to happen due to homicide

# h

# we can see that variable police is numeric which doesnot makes sense. It should be factor.

dataset$police <- as.factor(dataset$police)

# Now in order to check whether the police involved gun deaths are significantly different from other gun
# deaths, we will use chi square test. The reason being since both intent and police are factors.

pol <- dataset[dataset$police == 1,]
no_pol <- dataset[dataset$police == 0,]

stab<- table(pol$police, no_pol$police)
chisq.test(stab) 
