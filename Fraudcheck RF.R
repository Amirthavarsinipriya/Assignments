# Using Random Forest
library(randomForest)
library(readr)
fraud_check<-read.csv("C://Users//Gany//Desktop//Random Forests//Fraud_check.csv")
View(fraud_check)
str(fraud_check)
hist(fraud_check$Taxable.Income, main = "Sales of Companydata",xlim = c(0,100000),
     breaks=c(seq(40,60,80)), col = c("blue","red", "green","violet"))
Income<-NULL
Income<-ifelse(fraud_check$Taxable.Income<=30000,"Risky","Good")
fraud_check[,"Income"] <- Income
fraud_check$Undergrad <- as.factor(fraud_check$Undergrad)
fraud_check$Marital.Status <- as.factor(fraud_check$Marital.Status)
fraud_check$Urban <- as.factor(fraud_check$Urban)
fraud_check$Income <- as.factor(fraud_check$Income)
attach(fraud_check)
View(fraud_check)
# Splitting data into training and testing. As the Income are in order 
# splitting the data based on Income 
fraud_risky <- fraud_check[fraud_check$Income == "Risky",] 
fraud_not_risky <- fraud_check[fraud_check$Income == "Good",]

data_train <- rbind(fraud_risky[1:93,], fraud_not_risky[1:357,])
data_test <- rbind(fraud_risky[94:124,], fraud_not_risky[357:476,])

# Building a random forest model on training data 
fit.forest <- randomForest(Income~.,data=data_train, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree
plot(fit.forest)
# Training accuracy 
mean(data_train$Income==predict(fit.forest,data_train)) #100% Accuracy
#Prediction on train data
pred_train <- predict(fit.forest,newdata=data_train)
mean(pred_train==data_train$Income)

# Predicting test data 
pred_test <- predict(fit.forest,newdata=data_test)
mean(pred_test==data_test$Income) 
library(gmodels)
# Cross table 
rf_perf<-CrossTable(data_test$Income, pred_test,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))
#Here positive class is Good and accuracy is 100%