View(qwe)
str(qwe)

boxplot(qwe$`Customer Age (in months)`~ qwe$`Churn (1 = Yes, 0 = No)`, 
        xlab="Churn",
        ylab="Customer Age",
        main="Comparative boxplot", col=c("cadetblue", "gold"))

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

qwe$ID<- as.factor(qwe$ID)

logitModel <- glm(`Churn (1 = Yes, 0 = No)` ~ . -ID, data = qwe, family= "binomial")
summary(logitModel)

data0 <- qwe[qwe$ID== 672 , ]
data1 <- qwe[qwe$ID== 354 , ]
data2 <- qwe[qwe$ID== 5203 , ]
TestingData <- rbind(data0, data1, data2)

pred <- predict(logitModel, newdata = TestingData, type="response")
pred
# The predicted probability that Customer 672 will leave between December 2011 and February 2012 is 0.058
Class <- ifelse(pred >= 0.5, "YES", "NO")
Class
# The class indicates NO and observing the variables CHI score, Login information, it seems the customer did not leave, actually.
# The predicted probability that Customer 354 will leave is 
# The predicted probability that Customer 5023 will leave is 

# NO for both the customers since prob < 0.5

#==================================
#C

Pred <- rev(sort(predict(logitModel, newdata = qwe, type="response")))
test <- data.frame(Pred)
# Top 100 customers with the highest churn probabilities
head(Pred, 100)

# We know that values below 0.05 indicates significance, which means the coefficient or so called parameters that are estimated by our model are reliable. 
# And our model fits those variables better
# From the regression model, we see that CHI score Month 0 has the highest significance, followed by Customer Age (in months) and CHI Score 0-1


