library(readr)
fraud_check<-read.csv("C://Users//Gany//Desktop//Decision Trees//Fraud_check.csv")
View(fraud_check)
str(fraud_check)
Income<-NULL
Income<-ifelse(fraud_check$Taxable.Income<=30000,"Risky","Good")
View(Income)
mydata = data.frame(fraud_check,Income)
View(mydata)
mydata$Undergrad <- as.factor(mydata$Undergrad)
mydata$Marital.Status <- as.factor(mydata$Marital.Status)
mydata$Urban <- as.factor(mydata$Urban)
mydata$Income <- as.factor(mydata$Income)
library(caret)
library(C50)

# Data partion for model building and testing#

training <- mydata[1:300,]
View(training)
testing <- mydata[301:600,]
table(testing$Income)

#model building
model <- C5.0(training$Income~Undergrad + Marital.Status+City.Population+Work.Experience+Urban,data = training,trails = 40)

##Generating the model summary
summary(model)
plot(model)
pred <- predict.C5.0(model,testing)
table(pred)
a <- table(testing$Income,pred)
a   #Here Risky is classified as Good and positive class is Good because we
#are getting 246 good as correctly classified as Good and 54 is misclassified
sum(diag(a)/sum(a))  #Accuracy of this model is 82%
plot(model)

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(mydata$Income,p=.85,list=F)
  training1<-mydata[inTraininglocal,]
  testing<-mydata[-inTraininglocal,]
  
  fittree<-C5.0(training1$Income~.,data=training1)
  pred<-predict.C5.0(fittree,testing)
  a<-table(testing$Income,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

acc
summary(acc)


