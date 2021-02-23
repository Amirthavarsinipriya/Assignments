salary_test<-read.csv("C://Users//Gany//Desktop//Naive Bayes//SalaryData_Test.csv" ,stringsAsFactors = F)
View(salary_test)
str(salary_test)
salary_test$educationno <- as.factor(salary_test$educationno)
salary_test$ workclass<-as.factor(salary_test$ workclass)
salary_test$education<-as.factor(salary_test$education)
salary_test$maritalstatus<-as.factor(salary_test$maritalstatus)
salary_test$occupation<-as.factor(salary_test$occupation)
salary_test$relationship<-as.factor(salary_test$relationship)
salary_test$race<-as.factor(salary_test$race)
salary_test$sex<-as.factor(salary_test$sex)
salary_test$native<-as.factor(salary_test$native)
salary_test$Salary<-as.factor(salary_test$Salary)
class(salary_test)
str(salary_test)
salary_train<-read.csv("C://Users//Gany//Desktop//Naive Bayes//SalaryData_Train.csv",stringsAsFactors = F)
View(salary_train)
str(salary_train)
salary_train$educationno <- as.factor(salary_train$educationno)
salary_train$workclass<-as.factor(salary_train$workclass)
salary_train$education<-as.factor(salary_train$education)
salary_train$maritalstatus<-as.factor(salary_train$maritalstatus)
salary_train$occupation<-as.factor(salary_train$occupation)
salary_train$relationship<-as.factor(salary_train$relationship)
salary_train$race<-as.factor(salary_train$race)
salary_train$sex<-as.factor(salary_train$sex)
salary_train$native<-as.factor(salary_train$native)
salary_train$Salary <-as.factor(salary_train$Salary)
str(salary_train)
class(salary_train)

library(naivebayes)
library(ggplot2)    
library(caret)
library(psych)
library(e1071)
#Visualization 
# Plot and ggplot 
ggplot(data=salary_train,aes(x=salary_train$Salary, y = salary_train$age, fill = salary_train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")


# Naive Bayes Model
library(e1071)
model <-naiveBayes(salary_train$Salary~.,data=salary_train)
model
model_pred <- predict(model,salary_test)
mean(model_pred==salary_test$Salary)
confusionMatrix(model_pred,salary_test$Salary)
# Here <=50K is misclassified some 1919 are misclassified as >50k and
#811 of >50K are misclassified as <=50K and the positive class is <=50k 
#Accuracy of the model is 81.87%
#so majority salary is <=50K