?seq_along

install.packages('methods')
library(methos)

Sys.Date()
sys.function

?addTclPath
?fix

fix(summary.data.frame)
summary(women)

success <- c(13,12,11,14,14,11,13,11,12)
failure <- c(0,0,0,0,0,0,0,2,2)
resp <- cbind(success, failure)
predictor <- c(0, 5^(0:7))
glm(resp ~ 0+predictor, family = binomial(link="log"))
traceback()

?rapply
?search
search()
searchpaths()

X <- list(list(a = pi, b = list(c = 1:1)), d = "a test")
X
rapply(X, function(x) x, how = "replace")

rapply(X, sqrt, classes = "numeric", how = "replace")

?structure

mean
methods("mean")
mean.deafult()
mean.default
mean.difftime
plot
methods("plot")
plot.default
plot.xy

getS3method("lm","default")
?getS3method
?getAnywhere
getAnywhere("mean.default")

library(devtools)
install_github("pryr")

library(Rtool)

install_github('devtools')
library(devtools)
?install_github



help.search(keyword = "character", package = "base")

library(Rwordseg)



























