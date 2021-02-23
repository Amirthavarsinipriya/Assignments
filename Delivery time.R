library(readr)
delivery_time <- read_csv("delivery_time.csv")
View(delivery_time)
#EDA
summary(delivery_time)

#scatter plot
plot(delivery_time$`Sorting Time`,delivery_time$`Delivery Time`) #plot(x,y)

attach(delivery_time)

#correlation coefficient(r)
cor(`Sorting Time`,`Delivery Time`) #cor(x,y)
#here correlation coefficient r =0.825 which is little less than 0.85 mostly moderate correlation

#simple linear regression model
reg<-lm(`Delivery Time`~`Sorting Time`) #lm(y~x)

summary(reg)
plot(reg)
#Here multiple R squared value is 0.6823 which is less than 0.81 and P value is less than 0.05 .That is this model
#will predict the output 68.23% time correct but we have to increase the multiple R squared value

Pred<-predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(delivery_time))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")

# Logrithamic Model

# x = log(`Sorting Time`); y = `Delivery Time`

plot(log(`Sorting Time`), `Delivery Time`)
cor(log(`Sorting Time`), `Delivery Time`)

reg_log <- lm(`Delivery Time` ~ log(`Sorting Time`))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)
#here also multiple R squared value is 69.54% so we have to increase multiple R squared value

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(delivery_time))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

# Exponential Model

# x = `Sorting Time`; y = log(`Delivery Time`)
plot(`Sorting Time`, log(`Delivery Time`))
cor(`Sorting Time`, log(`Delivery Time`))
reg_exp <- lm(log(`Delivery Time`) ~`Sorting Time` )  #lm(log(Y) ~ X)
summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

log(`Delivery Time`) <- predict(reg_exp)
`Delivery Time` <- exp(log(`Delivery Time`))

#here we are getting multiple R squared value 0.7109 so this model will predict 71.09% correct
#we need more variables to increase multiple R squared value so we can use influence function


library(mvinfluence)
influenceIndexPlot(reg)
delivery_time <- lm(`Delivery Time` ~ `Sorting Time`, data = delivery_time[c(-5,-9,-21),])
summary(delivery_time)

# after removing 3 points multiple r squared value is increased 0.8332.so this model will predict 83.32% time correct

