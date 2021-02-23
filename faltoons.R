library(readxl)
library(openxlsx)
library(WriteXLS)


##Faltoons.Xlsx##
Faltoons <- read_excel("Faltoons.xlsx")
attach(Faltoons)
View(Faltoons)

#Here dependent and independent are discrete and we have 2 independent variables so we are going for
#2-Proportion Test#
table1 <- table(Weekend,Weekdays)
table1
prop.test(x=c(167,66),n=c(287,113),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
#Here P value is 0.9681>0.05 so accept null hypothesis.
#so %males Versus %females walking in to the store are equal