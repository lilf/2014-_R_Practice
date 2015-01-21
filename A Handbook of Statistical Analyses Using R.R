##Chapter1
install.packages("HSAUR")
library("HSAUR")
vignette("HSAUR")
vignette("Ch_introduction_to_R", package = "HSAUR") #加载到第一章节
??HSAUR
install.packages("RJSONIO")
library("RJSONIO")
vignette("RJSONIO")
x <- data("Forbes2000", package = "HSAUR")
x
ls()
rm(list = ls())
x
data("Forbes2000", package = "HSAUR")
ls()
print(Forbes2000)
head(Forbes2000)
str(Forbes2000)
??Forbes2000
class(Forbes2000)
dim(Forbes2000)
nrow(Forbes2000)
ncol(Forbes2000)
names(Forbes2000)
class(Forbes2000[,"rank"])
length(Forbes2000[,"rank"])

1:3
c(1,2,3)
seq(from = 1, to = 3, by = 1)
class(Forbes2000[,"name"])
length(Forbes2000[,"name"])
Forbes2000[,"name"][1]
head(Forbes2000,n=1)

class(Forbes2000[,"category"])
nlevels(Forbes2000[,"category"])  #水平数
levels(Forbes2000[,"category"])  #水平列表

class(Forbes2000[,"sales"])
median(Forbes2000[,"sales"])
mean(Forbes2000[,"sales"])
range(Forbes2000[,"sales"])
summary(Forbes2000[,"sales"])
??read.table

csvForbes2000 <- read.table("Forbes2000.csv", header = TRUE, sep = ",", row.names = 1)
csvForbes2000

companies <- Forbes2000[,"name"]
companies[1]
head(companies)
companies[1:3]
Forbes2000[1:3, c("name", "sales", "profits", "assets")]

companies <- Forbes2000$name
companies
companies <- Forbes2000[,"name"]

order(head(Forbes2000$sales))
order_sales <- order(Forbes2000$sales)
order_sales
companies[order_sales[1:3]]
Forbes2000[order_sales[c(2000, 1999, 1998)], c("name", "sales", "profits", "assets")]
Forbes2000[Forbes2000$assets > 1000, c("name", "sales", "profits", "assets")]

table(Forbes2000$assets > 1000)

na_profits <- is.na(Forbes2000$profits)
table(na_profits)
length(na_profits)
Forbes2000[na_profits == TRUE, c("name", "sales", "profits", "assets")]

table(complete.cases(Forbes2000))

UKcomp <- subset(Forbes2000, country == "United Kingdom")
dim(UKcomp)

summary(Forbes2000)

lapply(Forbes2000, summary)   ##lapply对每一个对象使用summary函数

myprofits <- tapply(Forbes2000$profits, Forbes2000$category, median, na.rm =TRUE)  #对因素，进行计算
myprofits
?lapply

?quantile   #分位数计算
?diff
iqr <- function(x) {
  q<- quantile(x, prob = c(0.25, 0.75), names = FALSE)
  return(diff(q))
}
xdata <- rnorm(100)
q<- quantile(xdata, prob = c(0.25, 0.75), names = FALSE)
q
xdata
iqr(xdata)
na.rm = TRUE
iqr_profits <- tapply(Forbes2000$profits, Forbes2000$category, iqr, na.rm= TRUE )  ##?na.rm unused
iqr_profits
?tapply 


bcat <- Forbes2000$category
head(bcat)
iqr_profit2 <- numeric(nlevels(bcat))
nlevels(bcat)
numeric(nlevels(bcat))
?numeric   #产生一个0向量，其长度为nlevels(bcat)，
iqr_profit2
for (cat in levels(bcat)){            ##等同于tapply函数
  catprofit <- subset(Forbes2000, category == cat)$profit
  this_iqr <-iqr(catprofit, na.rm = TRUE)
  iqr_profits2[levels(bcat) == cat] <- this_iqr
}

#Exercises
#1.1
head(Forbes2000)
levels(Forbes2000$country)
nlevels(Forbes2000$country)
str(Forbes2000)
SomeCoun_data <- Forbes2000[Forbes2000$country %in% c("United States","United Kingdom", "France", "Germany"),]
head(SomeCoun_data)
length(SomeCoun_data$country)
unique(SomeCoun_data$country)

table(Forbes2000$country %in% c("United States","United Kingdom", "France", "Germany"))
tail(Forbes2000)
tail(Forbes2000[Forbes2000$country %in% c("United States","United Kingdom", "France", "Germany"),])
??count
count(Forbes2000)

levels(SomeCoun_data$country)

levels(SomeCoun_data$country)
SomeCoun_data
head(SomeCoun_data)
SomeCoun_data[c(SomeCoun_data$profits,SomeCoun_data$country)]
str(SomeCoun_data)
tapply(SomeCoun_data$profits, SomeCoun_data$country, median, na.rm = TRUE)
head(SomeCoun_data)
SomeCoun_data <- data.frame(SomeCoun_data$profits, SomeCoun_data$country)
head(SomeCoun_data)
levels(SomeCoun_data$country)
tapply(SomeCoun_data$profits, SomeCoun_data$country, median, na.rm=TRUE)

unqiue(Forbes2000$country)
levels(Forbes2000$country)
str(Forbes2000)

Forbes2000[Forbes2000$country == "Germany" & Forbes2000$profits < 0,]$name

str(Forbes2000)
Bermuda_cat <- table(Forbes2000[Forbes2000$country=="Bermuda",]$category)
Bermuda_cat <- Bermuda_cat[Bermuda_cat > 0]
rank(Bermuda_cat)
Bermuda_cat <- Bermuda_cat[Bermuda_cat==10]
Bermuda_cat
Bermuda_cat <- Bermuda_cat[Bermuda_cat==]

head(Forbes2000)

rank(Forbes2000$profits)
??top
sort(Forbes2000$profits)
?order
data_new <- Forbes2000[sort.list(Forbes2000$profits,decreasing = FALSE), ]
data_new
head(data_new)

??aggregrate
Forbes2000$profits

data_new <- Forbes2000[Forbes2000$profits > 5,]
data_new <- data_new[complete.cases(data_new),]
data_new
aggregate(Forbes2000, Forbes2000 )


##chapter
USmelanoma
USmelanoma
?with




















