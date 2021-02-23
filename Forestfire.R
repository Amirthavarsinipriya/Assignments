setwd("C://Users//Gany//Desktop//Support Vector Machines")
library(readr)
forestfire<-read.csv("C://Users//Gany//Desktop//Support Vector Machines//forestfires.csv")
View(forestfire)
colnames(forestfire)
str(forestfire)
library(dplyr)
glimpse(forestfire)
#using glimpse()in the data, we notice the area of the burn has a lot of zeroes. We investigate further with a histogram.
#We may want to log(area+1) transform the area due to the heavy skew and many zeroes (fires that burnt less than a hectare)
hist(forestfire$area)
forestfire<-mutate(forestfire,y=log(area+1))
View(forestfire)
hist(forestfire$y)


#we can limit our data preparation to a few variables.The proposed solution, which is based in a SVM 
#requires only four direct weather inputs (i.e. temperature, rain, relative humidity and wind speed) is capable of predicting small fires, 
#which constitute the majority of the fire occurrences.
#We  need to normalise the continuous variables between zero and one to control for different ranges
normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))  # subtract the min value in x and divide by the range of values in x.
}

forestfire$temp<-normalise(forestfire$temp)
forestfire$rain<-normalise(forestfire$rain)
forestfire$RH<-normalise(forestfire$RH)
forestfire$wind<-normalise(forestfire$wind)

# note, our earlier transformation was redundant, $area gives the same results
sum(forestfire$area < 5)  # ln(0 + 1) = 0
sum(forestfire$area >= 5)
#splitting large and small area using ifelse
forestfire$size=NULL
forestfire$size <- factor(ifelse(forestfire$area < 5, 1, 0),
                      labels = c("small", "large"))

#splitting the data into train and test
library(caret)
set.seed(123)
ind <- sample(2, nrow(forestfire), replace = T, prob = c(0.7, 0.3))
train <- forestfire[ind==1,]
test <- forestfire[ind==2,]

# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 

# kvsm() function uses gaussian RBF kernel 

# Building model 
library(kernlab)
model1<-ksvm(size_category~temp+rain+wind+RH, 
             data= train,kernel = "vanilladot")
model1

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(size_category~temp+rain+wind+RH, 
                  data= train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test)
mean(pred_rfdot==test$size_category)  #Accuracy=68.49%

# kernel = vanilladot
model_vanilla<-ksvm(size_category~temp+rain+wind+RH, 
                    data= train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=test)
mean(pred_vanilla==test$size_category) # Accuracy=67.80%

# kernal = besseldot
model_besseldot<-ksvm(size_category~temp+rain+wind+RH, 
                      data= train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=test)
mean(pred_bessel==test$size_category)  #Accuracy=67.80%

# kernel = polydot
model_poly<-ksvm(size_category~temp+rain+wind+RH, 
                 data= train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = test)
mean(pred_poly==test$size_category)  #Accuracy=67.80%
#High accuracy is the best model 