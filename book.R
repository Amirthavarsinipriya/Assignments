#install.packages("recommenderlab", dependencies=TRUE)
#install.packages("Matrix")
library("recommenderlab")
library(caTools)

#Book data#
books<-read.csv("C:/Users/Gany/Desktop/Recommendation system/books.csv")
View(books)
book_rate_data=books[,3:6]
View(book_rate_data)
class(book_rate_data)
str(book_rate_data)
#table(book_rate_data$Book.Title)
#table(book_data$Book.Author)
#table(book_data$Publisher)
#table(book_data$ratings...3.)

#rating distribution
hist(book_rate_data$rating)
#From histogram more rating is between 0 t0 1.5#

#the datatype should be realRatingMatrix inorder to build recommendation engine
book_rate_data_matrix <- as(book_rate_data, 'realRatingMatrix')

#Popularity based 

book_recomm_model1 <- Recommender(book_rate_data_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(book_recomm_model1,book_rate_data_matrix[2:3], n=5)
as(recommended_items1, "list")


## Popularity model recommends the same books for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

book_recomm_model2 <- Recommender(book_rate_data_matrix, method="UBCF") #UCBF=User Based Collaborative Filtering

#Predictions for two users 
recommended_items2 <- predict(book_recomm_model2, book_rate_data_matrix[2:3], n=5)
as(recommended_items2, "list")
