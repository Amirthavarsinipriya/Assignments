library(readr)
emp_data <- read_csv("emp_data.csv")
View(emp_data)

#EDA
summary(emp_data)

#scatterplot
plot(emp_data$Salary_hike,emp_data$Churn_out_rate)  #plot(x,y)
attach(emp_data)

#correlation coefficient
cor(Salary_hike,Churn_out_rate) #cor(x,y)

#here correlation coefficient r is greater than 0.85 .The value is -0.91 there is a good relationship but in -ve direction
 #simple Linear Regression model
reg <-lm(Churn_out_rate~Salary_hike)  #lm(y~x)

summary(reg)
#here multiple R squared value is 0.8312 which is greater than 0.81 and pvalue is 0.00023 which is less than 0.05
#so this model will prdict the output 83.12% correct