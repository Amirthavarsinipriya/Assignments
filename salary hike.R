library(readr)
Salary_Data <- read_csv("Salary_Data.csv")
View(Salary_Data)

#EDA
summary(Salary_Data)

#scatter plot
plot(Salary_Data$YearsExperience,Salary_Data$Salary) #plot(x,y)
attach(Salary_Data)

#correlation coefficient9r)
cor(YearsExperience,Salary)  #cor(x,y)
#here Correlation coefficient is greater than 0.85 

#simple  linear regression model
reg<-lm(Salary~YearsExperience) #(y~x)

summary(reg)
#here we are getting p value <0.05 and multiple R squared value is greater than 0.81
#the value of multiple R squared is 0.957 so this model predict 95.7% correct. 