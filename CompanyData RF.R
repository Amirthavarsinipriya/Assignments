# Using Random Forest
library(randomForest)
library(readr)
company_data<-read.csv("C://Users//Gany//Desktop//Random Forests//Company_Data.csv")
View(company_data)
str(company_data)
#Histogram of company Data
hist(company_data$Sales, main = "Sales of Companydata",xlim = c(0,20),
     breaks=c(seq(10,20,30)), col = c("blue","red", "green","violet"))
sale<-NULL
sale<-ifelse(company_data$Sales > 7.490,1,0) # if greater than 8 then high sales else Low
company_data[,"sale"] <- sale
#Here we want to read factor function as factor
company_data$ShelveLoc<-as.factor(company_data$ShelveLoc)
company_data$Urban<-as.factor(company_data$Urban)
company_data$US<-as.factor(company_data$US)
company_data$sale<-as.factor(company_data$sale)

# Splitting data into training and testing. As the sale are in order 
# splitting the data based on sale
sales_high <- company_data[company_data$sale == "1",] 
sales_low <- company_data[company_data$sale == "0",]

data_train <- rbind(sales_high[1:150,], sales_low[1:150,])
data_test <- rbind(sales_high[151:199,], sales_low[151:201,])

# Building a random forest model on training data 
fit.forest <- randomForest(sale ~.,data=data_train, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree
plot(fit.forest)

# Training accuracy 
mean(data_train$sale==predict(fit.forest,data_train)) 

#Prediction on train data
pred_train <- predict(fit.forest,newdata=data_train)
mean(pred_train==data_train$sale)

# Predicting test data 
pred_test <- predict(fit.forest,newdata=data_test)
mean(pred_test==data_test$sale) #99 % Accuracy
library(gmodels)
# Cross table 
rf_perf<-CrossTable(data_test$sale, pred_test,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))
#Here the positive class is 0.so the sales is low(whuch is less than 8) and 99% Accuracy and one is misclassified where 1 is predicted as 0. 