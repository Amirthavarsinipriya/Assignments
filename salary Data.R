setwd("C://Users//Gany//Desktop//Support Vector Machines")
library(readr)
test_sal<-read.csv("C://Users//Gany//Desktop//Support Vector Machines//SalaryData_Test(1).csv")
View(test_sal)
colnames(test_sal)
str(test_sal)
test_sal$educationno<-as.factor(test_sal$educationno)
train_sal<-read.csv("C://Users//Gany//Desktop//Support Vector Machines//SalaryData_Train(1).csv")
View(train_sal)
colnames(train_sal)
str(train_sal)
train_sal$educationno<-as.factor(train_sal$educationno)

# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 


# kvsm() function uses gaussian RBF kernel 

# Building model 

library(kernlab)
library(caret)
model1<-ksvm(train_sal$Salary~.,data= train_sal, kernel = "vanilladot")
model1

salary_prediction <- predict(model1, test_sal)
table(salary_prediction,test_sal$Salary)
# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"
# kernel = rfdot 
model_rfdot<-ksvm(train_sal$Salary~.,data= train_sal,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test_sal)
mean(pred_rfdot==test_sal$Salary)  #Accuracy is 85.19%

# kernel = vanilla
model_vanilla<-ksvm(train_sal$Salary~.,data= train_sal,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=test_sal)
mean(pred_vanilla==test_sal$Salary)  # Accuracy is 84.64%

# kernal = besseldot
model_besseldot<-ksvm(train_sal$Salary~.,data= train_sal,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=test_sal)
mean(pred_bessel==test_sal$Salary)   #Accuracy is 78.97%

# kernel = polydot

model_poly<-ksvm(train_sal$Salary~.,data= train_sal,kernel = "polydot")
pred_poly<-predict(model_poly,newdata=test_sal)
mean(pred_poly==test_sal$Salary)  #Accuracy is 84.64%


# kernal=tanhdot 
model_tanh<-ksvm(train_sal$Salary~.,data= train_sal,kernel = "tanhdot")
pred_tanh<-predict(model_tanh,newdata=test_sal)
mean(pred_tanh==test_sal$Salary)   #Accuracy is 63.87%

# kernal=laplacedot
model_laplace<-ksvm(train_sal$Salary~.,data= train_sal,kernel = "laplacedot")
pred_laplace<-predict(model_laplace,newdata=test_sal)
mean(pred_laplace==test_sal$Salary) #Accuracy is 85.10%

# kernal=anovadot
model_anova<-ksvm(train_sal$Salary~.,data= train_sal,kernel = "anovadot")
pred_anova<-predict(model_anova,newdata=test_sal)
mean(pred_anova==test_sal$Salary)

# Kernal=splinedot
model_spline<-ksvm(train_sal$Salary~.,data= train_sal,kernel = "splinedot")
pred_spline<-predict(model_spline,newdata=test_sal)
mean(pred_spline==test_sal$Salary)

# kernal=stringdot 
model_string<-ksvm(train_sal$Salary~.,data= train_sal,kernel = "stringdot")
pred_string<-predict(model_string,newdata=test_sal)
mean(pred_string==test_sal$Salary)

#High accuracy is the best model 
