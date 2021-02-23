library(arules)
library(arulesViz)
# Whenever we have data containing item names, then load that data using 
# read.transactions(file="path",format="basket",sep=",")
# use this to form association rules 

##groceries##
groceries<-read.transactions("C://Users//Gany//Desktop//Association Rules//groceries.csv",format="basket")
summary(groceries)
inspect(groceries[1:5])
inspect(groceries[1:10])
class(groceries)

#Examine the frequency of items#
itemFrequency(groceries[, 1:3])
itemFrequencyPlot(groceries,support=0.002)
itemFrequencyPlot(groceries,topN=20)

apriori(groceries)

rules<-apriori(groceries,parameter=list(support=0.2,confidence=0.05,minlen=3))
rules
inspect(rules[1:10])
##if we give 0.2 we are getting no rules so we have to decrease the value for support##
rules<-apriori(groceries,parameter=list(support=0.02,confidence=0.05,minlen=3))
rules
inspect(rules[1:10])
##still we are getting no rules so we need to decrease the support value to minimum##
rules<-apriori(groceries,parameter=list(support=0.002,confidence=0.05,minlen=3))
rules
inspect(rules[1:10])
plot(rules)
summary(rules)
##for this we got 118 rules##
##To find redundant rules##
rules
redundant_rules<-is.redundant(rules)
summary(redundant_rules)
##There are 4 redundant rules from the set of 118 rules##
#we are removing the redundant rules##
gr_rules<-rules[!redundant_rules]
gr_rules
inspect(gr_rules[1:10])
##Here bag,product,shopping are most of the customer buying##where the lift value is high #
plot(gr_rules,method = graph)
