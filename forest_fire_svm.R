getwd()

forest_fire <- read.csv(file.choose())
View(forest_fire)

str(forest_fire)

table(forest_fire$size_category)
prop.table(table(forest_fire$size_category))

library(caTools)
set.seed(123)
forest_split <- sample.split(forest_fire$size_category,SplitRatio = 0.8)
train <- subset(forest_fire,forest_split== TRUE)
test <- subset(forest_fire,forest_split == FALSE)

prop.table(table(train$size_category))
prop.table(table(test$size_category))


library(kernlab)
library(caret)

model_1 <- ksvm(size_category ~., data = train, kernel = "vanilladot")
model_1


# kernel = rfdot 
model_rfdot<-ksvm(size_category ~., data = train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test)
mean(pred_rfdot==test$size_category) # 78.84

# kernel = vanilladot
model_vanilla<- ksvm(size_category ~., data = train,kernel = "vanilladot")
pred_vanilladot <-predict(model_vanilla ,newdata=test)
mean(pred_vanilladot==test$size_category) # 88.461


# kernal = besseldot
model_besseldot<-ksvm(size_category ~., data = train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=test)
mean(pred_bessel==test$size_category) # 65.38

# kernel = polydot

model_poly<-ksvm(size_category ~., data = train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = test)
mean(pred_poly==test$size_category) # 88.461


































