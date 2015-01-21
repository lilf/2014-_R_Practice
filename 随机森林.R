install.packages("ggplot2")
library(ggplot2)
head(iris,n=10)
str(iris)
dim(iris)
names(iris)
attributes(iris)
iris[1:5,]
iris[1:10,"Sepal.Width"]
iris$Sepal.Widt[1:10]
summary(iris)
table(iris$Species)
pie(table(iris$Species))
?pie
var(iris$Sepal.Length)
cor(iris$Sepal.Length,iris$Petal.Length)
hist(iris$Petal.Length)

install.packages("party")
library(party)
str(iris)

install.packages("randomForest")
library("randomForest")
??randomForest
head(iris)
set.seed(100)
?sample
?set.seed
ind=sample(2,nrow(iris),replace=TRUE,prob=c(0.8,0.2)
iris.rf=randomForest(Species~.,iris[ind==1,],ntree=50,nPerm=10,mtry=3,proximity=TRUE,importance=TRUE)
print(iris.rf)
iris.pred=predict(iris.rf,iris[ind==2,])
table(observed=iris[ind==2,"Species"],predicted=iris.pred)
unique(iris$Species)













