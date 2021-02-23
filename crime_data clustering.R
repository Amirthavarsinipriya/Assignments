library(readxl)
crime_data <- read_excel("C:/Users/Gany/Desktop/Clustering/crime_data.xlsx",1)
View(crime_data)

#ncol(crime_data)
crime_data1<-crime_data[,2:5]
View(crime_data1)
#Here we are removing the 1st column#

#Normalise the data#
normalized_data<-scale(crime_data[,2:5]) #excluding the 1st columnn before normalizing
View(normalized_data)

d <- dist(normalized_data, method = "euclidean") # distance matrix
d
fit <- hclust(d, method="complete")
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=4) # cut tree into 4 clusters
table(groups) #no of clusters in the groups

rect.hclust(fit, k=4, border="red")
#Here the clusters are added to the data#
membership<-as.matrix(groups)
final <- data.frame(crime_data1, membership)

library(data.table)
attach(final)
final1<-setcolorder(final,neworder = c("membership"))
View(final1)

#library(WriteXLS)
#getwd()
#WriteXLS(final1, file="Hcluster.xlsx")

crime_data_final <- cbind(crime_data, groups)
View(crime_data_final)
aggregate(crime_data_final[,2:6], by=list(crime_data_final$groups), FUN = mean)
#As per summary we can say group 2 have the higher rate of crime#
#here 4 clusters are formed#