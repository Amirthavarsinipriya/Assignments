library(arules)
library(arulesViz)
movies<-read.csv("C://Users//Gany//Desktop//Association Rules//my_movies.csv")
View(movies)
class(movies)

movie_trans<-as(as.matrix(movies[,6:15]),"transactions")
inspect(movie_trans)
size(head(movie_trans)) #number of items in each observation
inspect(head(movie_trans))

rules<-apriori(as.matrix(movies[,6:15]),parameter=list(support=0.0001,confidence=0.7))
inspect(rules)
#we have to change the support to 0.1
rules<-apriori(as.matrix(movies[,6:15]),parameter=list(support=0.1,confidence=0.7))
inspect(rules)

frequentItems <- eclat (as.matrix(movies[,6:15]), parameter = list(supp = 0.1, maxlen=3))
inspect(frequentItems)

rules_conf <- sort (rules, by="confidence", decreasing=FALSE) # 'high-confidence' rules.
inspect(head(rules_conf)) # show the support, lift and confidence for all rules

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift))

# Apriori algorithm 
plot(rules)

#Here we are getting the best rule from this values.##
