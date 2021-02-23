library(readr)
library(car)
ToyotaCorolla <- read_csv("ToyotaCorolla.csv")
View(ToyotaCorolla)
Corolla<-ToyotaCorolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Corolla)

# Exploratory Data Analysis(60% of time)
# 1. Measures of Central Tendency
# 2. Measures of Dispersion
# 3. Third Moment Business decision
# 4. Fourth Moment Business decision
# 5. Probability distributions of variables
# 6. Graphical representations
#  > Histogram,Box plot,Dot plot,Stem & Leaf plot, 
#     Bar plot
summary(Corolla)
str(Corolla)
attach(corolla)


# 7. Find the correlation b/n Output (Price) & ("Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight" )-Scatter plot
pairs(Corolla)
plot(Corolla)

#Correlation Coefficient matrix - Strength & Direction of Correlation#
round(cor(Corolla),2)

##Pure Correlation  b/n the varibles
library(corpcor)
cor2pcor(cor(Corolla))

## Building linear regression model
model.car <- lm(Price ~ ., data = Corolla)
summary(model.car)

# cc and Doors are influence to each other
model.car1 <- lm(Price ~ cc)
summary(model.car1) # Its significat to output

model.car2 <- lm(Price ~ Doors)
summary(model.car2)# Its significat to output

## Build model with cc and Doors
model.car3 <- lm(Price ~ cc + Doors)
summary(model.car3) # Both are significant to each other

# Find out the influencial record
influence.measures(model.car)
influenceIndexPlot(model.car)
influencePlot(model.car)
#Deleting influential Records and build the model
model1 <- lm(Price ~ ., data = Corolla[-c(81),])
summary(model1)

## Variance Inflation factor to check collinearity b/n variables 
vif(model1)
#Here VIF we are getting less than 10 so this model1 is the perfect model#

## Added Variable plot to check correlation b/n variables and o/p variable
avPlots(model1)
