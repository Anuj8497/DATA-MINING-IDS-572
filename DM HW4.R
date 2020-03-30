View(qwe)
str(qwe)

boxplot(qwe$`Customer Age (in months)`~ qwe$`Churn (1 = Yes, 0 = No)`, 
        xlab="Churn",
        ylab="Customer Age",
        main="Comparative boxplot", col=c("cadetblue", "gold"))


qwe[qwe == "-"] <- "0"
qwe[qwe == "1.0"] <- "1"

qwe$`Churn (1 = Yes, 0 = No)`<- as.factor( qwe$`Churn (1 = Yes, 0 = No)` )

avg_churn<-qwe %>% 
  group_by(`Customer Age (in months)`) %>% 
  summarise(avg_churn=mean(as.numeric(as.character(`Churn (1 = Yes, 0 = No)`)))) #count the average churn rate of different customer age group   

age_churn<-qwe %>% 
  filter(`Churn (1 = Yes, 0 = No)`==1) %>% #keep only churn out customers' data
  ggplot(aes(x=`Customer Age (in months)`))+
  geom_bar(fill="cadetblue",alpha=.8)+#change the color and transparency
  labs(title = "Number of churn customers \nby customer age",x="Cutomer age in months",y = "Number of churn customers")#add title and axis labels

plot(age_churn)

#==================================
#B

qwe[qwe == "-"] <- "0"
High<-as.factor(ifelse(qwe$`Days Since Last Login 0-1` >= 0, "YES", "NO"))
Data<-data.frame(qwe, High)
Data<-Data[,-1]
colnames(Data)[13] <- "Target"

set.seed(456)
index<-sample(2,nrow(Data), replace = T, prob = c(0.8, 0.2))
train<-Data[index == 1, ]
test<-Data[index == 2, ]

logitModel<- glm(Target~., data=train, family = "binomial")
summary(logitModel)

options(scipen=99)
Pred <- predict(logitModel, newdata = test, type = "response")
Pred
Class <- ifelse(Pred >=0.5, "YES", "NO")
Class










