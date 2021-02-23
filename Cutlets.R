library(readxl)
library(openxlsx)
library(WriteXLS)


#Cutlets.xlsx data#
library(readxl)
cutlets <- read_excel("Hypothesis Testing/cutlets.xlsx")
View(cutlets)
attach(cutlets)

#Normality Test#
shapiro.test(`Unit A`)
# p value is 0.32>0.05 so p is high null fly # so data follows normal distribution

shapiro.test(`Unit B`)
#p value is 0.5225>0.05 so p is high null fly # so data follows normal distribution

#external conditions are not same so we are going for variance test#
#Variance test#
var.test(`Unit A`,`Unit B`)
#Here P value is high p=0.3136>0.05 p is high null fly From the p value we can say variance are equal#
#so we are going for 2 sample t test for equal variance#

t.test(`Unit A`,`Unit B`,alternative = "two.sided",conf.level = 0.95,correct=TRUE)
# alternative = "two.sided" means we are checking for equal and unequal
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
#Here p value is 0.4723>0.05 so accept Null hypothesis.
#There is no significant Difference in diameter of the cutlet between two units#