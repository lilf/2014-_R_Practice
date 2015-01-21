# 划分法
# K-means
x <- c(3,3,3,5,5,6,6,3,5,3,7,9,5,7)
x <- matrix(x, nrow = 7)
t <- kmeans(x, 2)#分为两个簇
summary(t)
?kmeans
#打印出质心
plot(x)
summary(t)
points(t$centers, pch = 8)
plot(x, col = t$cluster)
points(t$centers, pch = 8, col = t$cluster)
plot(x, col = t$cluster)
points(t$centers, pch = 8, col = 1:2)
methods(points)

library(DMwR)
??DMwR
??switch
head(algae.sols)
dist.to.knn

















