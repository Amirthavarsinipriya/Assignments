library(readr)
cococola<-read.csv("C:/Users/Gany/Desktop/Forecasting/CocaCola Sales.csv")
View(cococola)
windows()
plot(cococola$Sales,type="o")# Seasonality Q1,Q2,Q3 and Q4 
# So creating 12 dummy variables
Q1 <-  ifelse(grepl("Q1",cococola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",cococola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",cococola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",cococola$Quarter),'1','0')
cococoladata<-cbind(cococola,Q1,Q2,Q3,Q4)
View(cococoladata)
colnames(cococoladata)
cococoladata["t"]<-c(1:42)
cococoladata["log_Sales"]<-log(cococoladata["Sales"])
cococoladata["t_square"]<-cococoladata["t"]*cococoladata["t"]
attach(cococoladata)
View(cococoladata)
train<-cococoladata[1:36,]
test<-cococoladata[37:42,]

########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model<-lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

######################## Multiplicative Additive Seasonality ##########################

multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

# Preparing table on model and it's RMSE values 
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#Additive Seasonality with Quadratic trend  has least RMSE value
new_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=cococoladata)
new_model_pred<-data.frame(predict(new_model,newdata=cococoladata,interval='predict'))
new_model_fin <- new_model$fitted.values
View(new_model_fin)
Quarter <- as.data.frame(cococoladata$Quarter)
Final <- as.data.frame(cbind(Quarter,cococoladata$Sales,new_model_fin))
colnames(Final) <-c("Quarter","Sales","New_Pred_Value")
plot(Final$Sales,main = "ActualGraph", xlab="Sales(Actual)", ylab="Quarter",
     col.axis="blue",type="o") 
View(Final)

write.csv(Final,file="Final.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
setwd("C:/Users/Gany/Desktop/Forecasting")
test_data<-read.csv("C:/Users/Gany/Desktop/Forecasting/Final.csv")
View(test_data)
pred_new<-predict(Add_sea_Quad_model,newdata=test_data,interval = 'predict')
pred_new
