set.seed(1)
library(readr)
Zoo<-read.csv("C://Users//Gany//Desktop//KNN//Zoo.csv",sep=",", header = TRUE)
View(Zoo)
str(Zoo)
Zoo = data.frame(Zoo)

#Phylogenic traits used for classification
names(Zoo) <- c("animal", "hair", "feathers", "eggs", "milk", "airborne",
              "aquatic", "predator", "toothed", "backbone", "breathes", "venomous",
              "fins", "legs", "tail", "domestic", "size", "type")
types <- table(Zoo$type)
Zoo_target <- Zoo[, 18]
Zoo_key <- Zoo[, 1]
Zoo$animal <- NULL

##Exploratory Investigation#
names(types) <- c("mammal", "bird", "reptile", "fish", "amphibian", "insect", "crustacean")
types
summary(Zoo)


#normalize the data
normalize_data<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

Zoo_n<-as.data.frame(lapply(Zoo,normalize_data))
View(Zoo_n)

# for predicting the accuracy of the model we will first split the data into training and testing 
Zoo_train<-Zoo_n[1:51,]
dim(Zoo_train)
Zoo_test<-Zoo_n[51:101,]
dim(Zoo_test)

Zoo_train_labels<-Zoo[1:51,1] # train labels
View(Zoo_train_labels)
class(Zoo_train_labels)
Zoo_test_labels<-Zoo[51:101,1]
View(Zoo_test_labels)
class(Zoo_test_labels)

# class package for classification which is a k-nn implementation and we will use knn() to to implement
# knn algorithm. knn() takes 4 parameters - train,test,class,k
library(class)
Zoo_test_pred<-knn(Zoo_train,Zoo_test,cl=Zoo_train_labels,k=5) 
class(Zoo_test_pred)

# evaluating the model performance
# By crosstable function from gmodels package
library(gmodels)
# cross table
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred,
           prop.chisq=FALSE)

library(caret)
confusionMatrix(table(Zoo_test_pred,Zoo_test_labels))
class(Zoo_test_labels) # factor
class(Zoo_test_pred) # factor 
##if K=17 this model provides 86.27%
#if K=5 this model provides 98.04% accuracy .so we can use k=5
