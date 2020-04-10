
#Installing and loading the libraries
#install.packages("recommenderlab", dependencies=TRUE)
#install.packages("Matrix")
library("recommenderlab")
library(caTools)

#books rating data
books <- read.csv(file.choose())

#metadata about the variable
str(books)


#rating distribution
hist(books$rating)

#the datatype should be realRatingMatrix inorder to build recommendation engine
books_matrix <- as(books, 'realRatingMatrix')

#Popularity based 

books_recomm_model1 <- Recommender(books_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(books_recomm_model1, books_matrix[2015:2016], n=5)
as(recommended_items1, "list")


## Popularity model recommends the same books for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

books_recomm_model2 <- Recommender(books_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(books_recomm_model2, books_matrix[2015:2016], n=5)
as(recommended_items2, "list")

