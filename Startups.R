setwd("C://Users//Gany//Desktop//Neural Networks")
library(readr)
startups<-read.csv("C://Users//Gany//Desktop//Neural Networks//50_Startups.csv")
View(startups)
str(startups)
attach(startups)
library(plyr)
startups$State <- as.numeric(revalue(startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
str(startups)
class(startups)

# Exploratory data Analysis :
plot(R.D.Spend, Profit)
plot(Administration,Profit)
plot(Marketing.Spend,Profit)
plot(State,Profit)
pairs(startups)

# Correlation coefficient - Strength & Direction of correlation
cor(startups)
round(cor(startups),2)
summary(startups)

#Normalize the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
startup_norm<-as.data.frame(lapply(startups,FUN=normalize))
View(startup_norm)
summary(startup_norm$Profit)

#Data Partition
library(caret)
set.seed(123)
ind <- sample(2, nrow(startup_norm), replace = TRUE, prob = c(0.7,0.3))
startups_train <- startup_norm[ind==1,]
startups_test  <- startup_norm[ind==2,]

# Using multilayered feed forward neural network
# package nueralnet
#install.packages("neuralnet")
#install.packages("nnet")
library(neuralnet)
library(nnet)

# Building model
startup_model <- neuralnet(Profit~.,data = startups_train)
str(startup_model)
plot(startup_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(startup_model,startups_test[1:4])
predicted_Profit <- model_results$net.result
predicted_Profit
model_results$neurons
cor(predicted_Profit,startups_test$Profit)
plot(predicted_Profit,startups_test$Profit)

# New model
model_5<-neuralnet(Profit~.,data= startup_norm,hidden = c(1,2))
plot(model_5)
model_5_res<-compute(model_5,startups_test[1:4])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,startups_test$Profit)
plot(pred_strn_5,startups_test$Profit)

model_5<-neuralnet(Profit~.,data= startup_norm,hidden = c(2,3))
plot(model_5)
model_5_res<-compute(model_5,startups_test[1:4])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,startups_test$Profit)
plot(pred_strn_5,startups_test$Profit)

model_5<-neuralnet(Profit~.,data= startup_norm,hidden = c(5,3))
plot(model_5)
model_5_res<-compute(model_5,startups_test[1:4])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,startups_test$Profit)
plot(pred_strn_5,startups_test$Profit)
# SSE has reduced and training steps had been increased as the number of neurons 
# under hidden layer are increased