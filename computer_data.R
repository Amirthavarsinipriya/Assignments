library(readr)
library(car)
Computer_Data <- read_csv("C:/Users/Gany/Desktop/Datascience materials/Assignments/10.Multi Linear Regression/Computer_Data.csv")
View(Computer_Data)
#EDA#
summary(Computer_Data)
library(plyr)
Computer_Data$cd <- as.factor(revalue(Computer_Data$cd,c("yes"=1, "no"=0)))
Computer_Data$ multi <- as.factor(revalue(Computer_Data$ multi,c("yes"=1, "no"=0)))
Computer_Data$ premium  <- as.factor(revalue(Computer_Data$ premium ,c("yes"=1, "no"=0)))
View(Computer_Data)
str(Computer_Data)


#Find the Correlation b/n o/p(price)&i/p(speed,hd,screen,cd,multi,premium,ads,trend )
pairs(Computer_Data)
plot(Computer_Data)

#Correlation Coefficient matrix - Strength & Direction of Correlation
cor(Computer_Data)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Data))


# The Linear Model of interest#
model <- lm(price ~ speed + hd + ram + screen + ads + trend + cd + multi + premium, data = Computer_Data)
summary(model)
#Here VIFis not greater than 10 so there is no colinearity between variables#


## Variance Inflation factor to check collinearity b/n variables 
vif(model)

avPlots(model)
#This model is the perfect model#