setwd("C://Users//Gany//Desktop//Neural Networks")
library(readr)
forestfire<-read.csv("C://Users//Gany//Desktop//Neural Networks//forestfires.csv")
View(forestfire)
str(forestfire)
colnames(forestfire)
attach(forestfire)
library(plyr)
forestfire$size_category <- as.numeric(forestfire$size_category,
                                     c("large"="1", "small"="2"))
View(forestfire)
forestfire$month <- as.numeric(as.factor(forestfire$month))
forestfire$day <- as.numeric(as.factor(forestfire$day))

# Exploratory data Analysis :
plot(area,RH)
plot(area,wind)
plot(area,rain)
plot(area,temp)
library(dplyr)
glimpser(forestfire)
#using glimpse()in the data, we notice the area of the burn has a lot of zeroes. We investigate further with a histogram.
#We may want to log(area+1) transform the area due to the heavy skew and many zeroes (fires that burnt less than a hectare)
hist(forestfire$area)
#forestfire<-mutate(forestfire,y=log(area+1))
#View(forestfire)
#hist(forestfire$y)
str(forestfire)

# Correlation coefficient - Strength & Direction of correlation
cor(forestfire)
round(cor(forestfire),2)
summary(forestfire)

#Normalize the data
#requires only four direct weather inputs (i.e. temperature, rain, relative humidity and wind speed) is capable of predicting small fires, 
#which constitute the majority of the fire occurrences.
#We  need to normalise the continuous variables between zero and one to control for different ranges
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))  # subtract the min value in x and divide by the range of values in x.
}
library(caret)
forestfire_norm<-as.data.frame(lapply(forestfire,FUN=normalise))
summary(forestfire_norm$temp)

#splitting the data into train and test
forestfire_train<-forestfire_norm[1:400,]
forestfire_test<-forestfire_norm[401:517,]

# Using multilayered feed forward nueral network
# package nueralnet
#install.packages("neuralnet")
#install.packages("nnet")
library(neuralnet)
library(nnet)

# Building model
forestfire_model <- neuralnet(temp ~ FFMC+DC+ISI+RH+wind+area+rain+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthmay+monthnov+monthnov+monthoct+monthsep,data = forestfire_train)
str(forestfire_model)
plot(forestfire_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(forestfire_model,forestfire_test[1:30])
predicted_strength <- model_results$net.result
predicted_strength
model_results$neurons
cor(predicted_strength,forestfire_test$temp)
plot(predicted_strength,forestfire_test$temp)
model_5<-neuralnet(temp ~ FFMC+DC+ISI+RH+wind+area+rain+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthmay+monthnov+monthnov+monthoct+monthsep,data= forestfire_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,forestfire_test[1:30])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,forestfire_test$temp)
plot(pred_strn_5,forestfire_test$temp)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased