library(readr)
Company<-read.csv("C://Users//Gany//Desktop//Decision Trees//Company_Data.csv")
View(Company)
str(Company)
sale<-NULL
sale<-ifelse(Company$Sales<10, "No", "Yes")
mydata<-data.frame(Company, sale)
CD <- mydata[,2:12]
View(CD)
CD$ShelveLoc<-as.factor(CD$ShelveLoc)
CD$Urban<-as.factor(CD$Urban)
CD$US<-as.factor(CD$US)
CD$sale<-as.factor(CD$sale)

library(caret)
library(C50)

# Data partion for model building and testing#

training <- mydata[1:200,]
View(training)
testing <- mydata[201:400,]
table(testing$sale)

#model building
model <- C5.0(training$sale~ CompPrice+Income+Advertising+Population+Price+ShelveLoc+Age+Education+Urban+US,data = training,trails = 40)


##Generating the model summary
summary(model)
plot(model)
pred <- predict.C5.0(model,testing)
table(pred)
a <- table(testing$sale,pred)
a #Here positive class is No refers sales less than 10 and there is some misclassification
#where 16 No is predicted as yes and 19 yes is predicted as No
sum(diag(a)/sum(a))  #The model Accuracy is 82.5%
plot(model)

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(mydata$sale,p=.85,list=F)
  training1<-mydata[inTraininglocal,]
  testing<-mydata[-inTraininglocal,]
  
  fittree<-C5.0(training1$sale~CompPrice+Income+Advertising+Population+Price+ShelveLoc+Age+Education+Urban+US,data=training1)
  pred<-predict.C5.0(fittree,testing)
  a<-table(testing$sale,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

acc
summary(acc)
