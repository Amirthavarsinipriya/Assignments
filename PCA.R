#Loading Wine dataset#
library(readr)
wine <- read_csv("C:/Users/Gany/Desktop/PCA/wine.csv")
View(wine)

cor(wine)

# Model Building#
pcaObj<-princomp(wine[,-1], cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)
##As per the summary first 7 variables contribute 90% of data hence this 15 variables is further reduced
#to 7 varaiables#
#The other variables can be included in case we intend to have more accurate analysis/forcasting/prediction#
str(pcaObj)

plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

library(cluster)
library(fpc)
library(NbClust)
library(factoextra)
#Cluster Analysis-All variables
no_of_Clusters = NbClust(wine, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")

#Hierarchical clustering-All variables#
normalized_data<-scale(wine) 
View(normalized_data)

d <- dist(normalized_data, method = "euclidean") # distance matrix
d
fit <- hclust(d, method="complete")
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=5) # cut tree into 5 clusters
table(groups)

#K-means Clustering-all variables#
#elbow curve & k ~ sqrt(n/2) to decide the k value
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 1:13) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:13, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

#we got K=5#
fit <- kmeans(normalized_data, 5) # 5 cluster solution
str(fit)
# TO see all centroid point 
fit$centers
final2<- data.frame(wine, fit$cluster) # append cluster membership
final2
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(wine, by=list(fit$cluster), FUN=mean)

km.7 = eclust(wine, "kmeans", k = 5, nstart = 25, graph = FALSE)
fviz_cluster(km.7, geom = "point", frame.type = "norm")

#Cluster Analysis - PCA Suggested Components
##Number of clusters suggested by the NbClust function are 7

wine.pca = wine[,2:14]
no_of_Clusters = NbClust(wine.pca, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")
#Plot bar chart for the clusters
fviz_nbclust(no_of_Clusters) + theme_minimal()

#optimal Number of clusters=7#

#Hierarchical clustering-PCA suggested components#
normalized_data<-scale(wine.pca) 
View(normalized_data)

d <- dist(normalized_data, method = "euclidean") # distance matrix
d
fit <- hclust(d, method="complete")
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=7) # cut tree into 7 clusters
table(groups)

#K-Means clustering - PCA Suggested Components

#elbow curve & k ~ sqrt(n/2) to decide the k value
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:14) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:13, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

#we got K=7#
fit <- kmeans(normalized_data, 7) # 7 cluster solution
str(fit)
# TO see all centroid point 
fit$centers
final2<- data.frame(wine, fit$cluster) # append cluster membership
final2
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
aggregate(wine, by=list(fit$cluster), FUN=mean)

km.7 = eclust(wine.pca, "kmeans", k = 5, nstart = 25, graph = FALSE)
fviz_cluster(km.7, geom = "point", frame.type = "norm")
#When PCA was applied on the entire set of varibles (13); PCA suggested that 90% of the information can be inferred from the first 7 variables# 
#We plotted dendrogram for both 13 variables ,7 variables data and found that the number of cluster required are 7 and the dendrogram seem identical#
