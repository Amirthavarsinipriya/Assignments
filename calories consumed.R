library(readr)
calories_consumed <- read_csv("calories_consumed.csv")
View(calories_consumed)
#EDA
summary(calories_consumed)
#scatter plot
plot(calories_consumed$`Calories Consumed`,calories_consumed$`Weight gained (grams)`) #plot(x,y)
attach(calories_consumed)

 #correlation coefficient(r)
cor(`Calories Consumed`,`Weight gained (grams)`) #cor(x,y)
#here correlation coefficient r is greater than o.85.So there is agood relationship between 2 variables

#simple Linear Regression model
reg<-lm(`Weight gained (grams)`~`Calories Consumed`) #lm(y~x)

summary(reg)
#here multiple R squared value is 0.8968 and p value is less than 0.05#so this model will predict the output 89.68% time correct
plot(reg)