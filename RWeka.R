dataSet <- read.table("C:\\Users\\lanfeng\\Desktop\\practice\\20141127-01\\C15kw.csv", header = TRUE, sep = ';')
dataSet <- as.data.frame(dataSet)
dataSet
write(dataSet, "C:\\Users\\lanfeng\\Desktop\\practice\\20141127-01\\dataSet.txt")
head(dataSet)
tail(dataSet)
head(dataSet)
library(rJava)

install.packages("RWeka")
library(RWeka)

library(rpart)
??rpart
?J48


install.packages("ISLR")
library(ISLR)
data("Hitters")
str(Hitters)

install.packages("vcd")
library(vcd)
data(Federalist)
str(Federalist)
Federalist



setwd("C:\\Users\\lanfeng\\Documents\\R\\win-library\\3.0")
getwd()














