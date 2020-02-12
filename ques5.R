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
dataset = na.omit(dataset)
dummy = dataset

# a 

# For determining the no of deaths by month we first need to convert the months into qualitative.

dataset$month <- as.factor(dataset$month)
new <- data.frame(summary(dataset$month))

#output

#  1    2    3    4    5    6    7    8    9   10   11   12 
# 8273 7093 8289 8455 8669 8677 8989 8783 8508 8406 8243 8413 


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

tabb <- table(dataset$intent)
tabb<-sort(tabb,decreasing = TRUE)
# Let's add margins for a better picture
ptab <-addmargins(prop.table(tabb))
addmargins(prop.table(tabb))
barplot(tabb, main = "Bar Plot", col=c("cadetblue", "gold"))


# d

boxplot(dataset$age ~dataset$sex, data=dataset, main="Age of gun death victims by sex", xlab="Sex", ylab="Age",col=c("orange", "lightblue4"))
# following line of code gives us only gun deaths of female.

female <- dataset[ which(dataset$sex=='F'), ]

# now lets calculate average age of female gun deaths
ans  <- mean(female$age)

summary(dataset$education)

# e 
  
ans <- count(dataset[ which(dataset$education=='HS/GED' & dataset$race =='White' & dataset$year == 2012 & dataset$sex == "M"), ])
plot(ans)

# 7794 were the no of deaths in 2012 who were male, white with atleast a high school education.

# f

# creating a new variable named season storing the season associated with each month

dataset$season[dataset$label=="Jan" | dataset$label == "Feb" | dataset$label == "Mar"]<-"Winter"
dataset$season[dataset$label=="Apr" | dataset$label == "May" | dataset$label == "Jun"]<-"Spring"
dataset$season[dataset$label=="Jul" | dataset$label == "Aug" | dataset$label == "Sept"]<-"Summer"
dataset$season[dataset$label=="Oct" | dataset$label == "Nov" | dataset$label == "Dec"]<-"Fall"


table <- table(dataset$season)
barplot(table, main = "Bar Plot", col=c("cadetblue", "gold"))


# from the table & plot it is clear that Summer is the season with most gun deaths.

# g

# Are whites who are killed by guns more likely to die because of suicide or homicide? How does
# this compare to blacks and Hispanics?

# In case of Whites

count(dataset[ which(dataset$intent=='Suicide' & dataset$race =='White'), ])
#56415
count(dataset[ which(dataset$intent=='Homicide' & dataset$race =='White'), ])
#8293

56415/98015 # i.e. 57.5%
8293/98015 # i.e 8.4 %

# Hence in case of white gun violences involving suicide are more likely to happen.  

# In case of Blacks

count(dataset[ which(dataset$intent=='Suicide' & dataset$race =='Black'), ])

# 3285

count(dataset[ which(dataset$intent=='Homicide' & dataset$race =='Black'), ])

# 18956

3285/98015 # i.e. 3.35%
19510/98015 # i.e 19.91%

# Hence in case of blacks gun violences involving homicide are more likely to happen. 

# In case of Hispanics

count(dataset[ which(dataset$intent=='Suicide' & dataset$race =='Hispanic'), ])

# 3120

count(dataset[ which(dataset$intent=='Homicide' & dataset$race =='Hispanic'), ])

# 5269

3120/98015 # i.e. 3.1%
5634/98015 # i.e 5.7%

# Hence in case of Hispanic gun violences involving homicide are more likely to happen. 

# h

# we can see that variable police is numeric which doesnot makes sense. It should be factor.

dataset$police <- as.factor(dataset$police)

# Now in order to check whether the police involved gun deaths are significantly different from other gun
# deaths, we will use chi square test. The reason being since both intent and police are factors.


table(dataset$police)

#  0     1 
# 97996    19
# The police invloved gun deaths are very less as compared to the non-police involvement.

# Accesing relationships between police involvement and other variables.

# with intent

table(dataset$police,dataset$intent)

# from the table we see all the gundeaths in which police were involved are homicide.Same is not the case when police were not involved.

# with year

table(dataset$year,dataset$police)

# from this we can see that from 2012 to 2014 non police involvement gun violences were more than police involved
# gun violences.

# with sex

table(dataset$sex,dataset$police)

# From this we get the information that all police invloved gun deaths involved males.
# Whereas in non police involved gun deaths involving males was almost six times more as compared to females 

table(dataset$race,dataset$police)

# In case of police involvement of non-police involvement, majority of victims belonged to White race.
# with education

table(dataset$education,dataset$police)

# In both the cases, majority of the victims were atleast High School graduates.

# with age

aggregate(dataset$age, by = list(dataset$police) ,FUN = mean)

# In police involved gun deaths the average age of victims is ~ 36 and for non police invloved is ~ 44.  

# with place

table(dataset$place,dataset$police)

# police involved gun violence were mostlty at Other unspecified place.
# in non police involved gun violences were mostly at Home.


# Since all the police involved gun violences were Homicide, so keeping homicide category aside they are different 
# from other intent types.
