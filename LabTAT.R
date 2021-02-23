library(readxl)
library(openxlsx)
library(WriteXLS)

#LabTAT xlsx data#
library(readxl)
LabTAT <- read_excel("LabTAT.xlsx")
attach(LabTAT)
View(LabTAT)


#Normality Test#
shapiro.test(`Laboratory 1`)
#p value is 0.5508>0.05 so null fly #data follows normal distribution

shapiro.test(`Laboratory 2`)
#pvalue is 0.8637>0.05 so null fly #data follows normal distribution

shapiro.test(`Laboratory 3`)
#pvalue is 0.4205>0.05 so null fly #data follows normal distribution

shapiro.test(`Laboratory 4`)
#pvalue is 0.6619>0.05 so null fly #data follows normal distribution

#variance Test#
#Here we are having lab 1,lab 2,Lab 3,Lab 4 for variance test we will get 8 combinations of data so we are using
#grouping that is stack #
LabTAT<-read_excel(file.choose())  
View(LabTAT)
stack.data <- stack(LabTAT)
attach(stack.data)
View(stack.data)
var.test(ind,values)
Anova_results <- aov(values~ind,data = stack.data)
summary(Anova_results)
#Here P value is less than 0.05 so alternative hypothesis will fly
#so there is a difference in average Turn around Time

