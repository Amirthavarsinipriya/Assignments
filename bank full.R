library(readr)
bank_full<-read.csv("C:/Users/Gany/Desktop/Logistic Regression/bank-full (1).csv",header = T,sep = ";")
View(bank_full)
attach(bank_full)
summary(bank_full)
colnames(bank_full)
str(bank_full)
head(bank_full)

#here we are having some categorical data we are changing it as numeric values#
bank_full$job = factor(bank_full$job,levels = c( "admin.","unknown","unemployed","management","housemaid","entrepreneur","student", "blue-collar", "self-employed","retired","technician", "services"),labels = c(1, 2, 3,4,5,6,7,8,9,10,11,12))
bank_full$marital=factor(bank_full$marital,levels = c("married","divorced","single"),labels = c(1,2,3))
bank_full$education=factor(bank_full$education,levels = c("unknown","secondary","primary","tertiary"),labels = c(1,2,3,4))
bank_full$default=factor(bank_full$default,levels = c("yes","no"),labels = c(1,2))
bank_full$housing=factor(bank_full$housing,levels = c("yes","no"),labels = c(1,2))
bank_full$loan=factor(bank_full$loan,levels = c("yes","no"),labels = c(1,2))
bank_full$contact=factor(bank_full$contact,levels=c("unknown","telephone","cellular"),labels = c(1,2,3))
#str(bank_full$month)
bank_full$month=factor(bank_full$month,levels = c("jan","feb","mar","apr","may","june","july","aug","sep","oct","nov","dec"),labels=c(1,2,3,4,5,6,7,8,9,10,11,12))
#str(bank_full$poutcome)
bank_full$poutcome=factor(bank_full$poutcome,levels=c( "unknown","other","failure","success"),labels=c(1,2,3,4))
bank_full$y=factor(bank_full$y,levels = c("yes","no"),labels = c(1,2))
bank<-bank_full
View(bank)

datamodel<-glm(y~., data=bank,family = binomial)
summary(datamodel)

# Linear regression technique can not be employed
prob1 <- predict(datamodel,type="response")
#View(prob1)

# Confusion matrix table 
#The predictions based on model that we created using the data Those predictions are pitted against
#the existing actual outcomes of our target variable and we create a confusion matrix with it
prob <- predict(datamodel,type=c("response"),bank)
prob
# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,bank$y)
probo<-prob>0.5
table(probo)

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
#Here the accuracy is 89.51%
Error <- 1-Accuracy
Error
#Here Error =10.48%

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained#


