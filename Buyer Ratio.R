library(readxl)
library(openxlsx)
library(WriteXLS)


####Buyer ratio###
library(readxl)
Buyer_Ratio <- read_excel("Buyer Ratio.xlsx")
View(Buyer_Ratio)
attach(Buyer_Ratio)
table(East,West,North,South)
chisq.test(table(table(East,West,North,South)))
##Here P value is 0.002>0.05 so all proportions are not equal ##
##so male_female buyer ratio is not similar across the regions##