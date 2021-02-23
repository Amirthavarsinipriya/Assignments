library(data.table)
library(readxl)
EastwestAirlines<-read_xlsx("C:/Users/Gany/Desktop/Clustering/EastWestAirlines.Xlsx",sheet="data")
View(EastwestAirlines)

#colnames(EastwestAirlines)
#ncol(EastwestAirlines)
#Here we are removing ID column#
myinput<-EastwestAirlines[,2:12]
View(myinput)

#Normalise Data#
normalised_data<-scale(myinput)
View(normalised_data)

# Hirerachical Clustering#
#Here we are finding Euclidean distance#
d <- dist(normalised_data, method = "euclidean") # distance matrix
d
fit <- hclust(d, method="complete")
plot(fit) # display dendrogram
plot(fit, hang=-1)

groups <- cutree(fit, k=5) # cut tree into 5 clusters
table(groups) #no of clusters in the groups

rect.hclust(fit, k=5, border="red")
#Here the clusters are added to the data#
membership<-as.matrix(groups)
final <- data.frame(myinput, membership)

library(data.table)
attach(final)
final1<-setcolorder(final,neworder = c("membership"))
View(final1)

final2 <- cbind(EastwestAirlines, groups)
setnames(final2, 'groups')
aggregate(final2[,2:12],by= list(final2$groups), FUN = mean)

##K-means Clustering##
library(data.table)
library(readxl)
EastwestAirlines<-read_xlsx("C:/Users/Gany/Desktop/Clustering/EastWestAirlines.Xlsx",sheet="data")
View(EastwestAirlines)

#colnames(EastwestAirlines)
#ncol(EastwestAirlines)
#Here we are removing ID column#
myinput<-EastwestAirlines[,2:12]
View(myinput)
#Normalise Data#
normalised_data<-scale(myinput)
View(normalised_data)

#elbow curve & k ~ sqrt(n/2) to decide the k value
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:12) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

#we got K=5#
fit <- kmeans(normalised_data, 5) # 5 cluster solution
str(fit)
# TO see all centroid point 
fit$centers
final2<- data.frame(myinput, fit$cluster) # append cluster membership
final2
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(myinput[,2:11], by=list(fit$cluster), FUN=mean)

# k clustering alternative for large dataset - Clustering Large Applications (Clara)
install.packages("cluster")
library(cluster)
xcl <- clara(normalised_data,5)
clusplot(xcl)
#using Partition Arround Medoids to find cluster
xpm <- pam(normalised_data,5)
clusplot(xpm)
