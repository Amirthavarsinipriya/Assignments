library(readxl)
library(openxlsx)
library(WriteXLS)


##Customer Order Form##
cof<-read_excel(file.choose()) # customer_order(unstacked).xlsx
View(cof) # countries are in their own columns; so we need to stack the data #Here both 
#independent and dependent variables are discrete so we are using Chi square test
stacked_cof<-stack(cof)
attach(stacked_cof)
View(stacked_cof)
table(stacked_cof$ind,stacked_cof$values)
chisq.test(table(stacked_cof$ind,stacked_cof$values))
#Here P value is 0.2771>0.05 accept null hypothesis.So there is equal defective in all the countries
