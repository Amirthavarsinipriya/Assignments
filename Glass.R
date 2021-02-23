install.packages("caret")
install.packages("pROC")
install.packages("mlbench")
install.packages("lattice")
install.packages("gmodels")
install.packages("class")
library(caret)
library(readr)
glass<-read.csv("C://Users//Gany//Desktop//KNN//glass.csv")
View(glass)


#table on different types of glasses 
table(glass$Type)

glass$type = as.factor(glass$Type)
str(glass)

# Type1- 70, Type2-76,Type3-17,Type5-13,Type6-9,Type7-29

# table or proportation of enteries in the datasets. What % of glass of Type 1 and what % of glass of Type 2
round(prop.table(table(glass$Type))*100,1)
summary(glass[c("RI","Na","Mg")])

#Create a function to normalize the data
normalize_data <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
glass_n<-as.data.frame(lapply(glass[1:9],normalize_data))
View(glass_n)

# glass_n <- cbind(glass$Type,glass_n[1:9])

#create training and test datasets
set.seed(123)
ind <- sample(2, nrow(glass_n), replace = TRUE, prob = c(0.7,0.3))
glass_train <- glass_n[ind==1,]
glass_test <-  glass_n[ind==2,]


#Get labels for training and test datasets
set.seed(123)
ind1 <- sample(2, nrow(glass), replace = TRUE, prob = c(0.7,0.3))
glass_train_labels <- glass[ind1==1,10]
glass_test_labels <-  glass[ind1==2,10]


# Build a KNN model on taining dataset

# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=5)
table(glass_test_pred,glass_test_labels)
mean(glass_test_pred==glass_test_labels)

# Evaluating Model Performance ----

# load the gmodel library

CrossTable(x=glass_test_labels,y=glass_test_pred,prop.chisq = FALSE) 
#Accuracy is 65.07%
