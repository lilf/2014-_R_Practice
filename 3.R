install.packages("dplyr")
library(dplyr)
??intesect

mtcars$model <- rownames(mtcars)
first <- mtcars[1:20, ]
first
second <- mtcars[10:32, ]
second

intersect(first, second)
union(first, second)
setdiff(first, second)
setdiff(second, first)

setequal(mtcars, mtcars[32:1, ])

mtcars
slice(mtcars, 1L)
?slice
??slice

??nse
??arrange
??summarise_

head(mtcars)
summarise(group_by(mtcars, cyl), m = mean(disp), sd = sd(disp))

by_species <- iris %>% group_by(Species)
by_species %>% summarise_each(funs(length))

?chain

??equal_data_frame
methods('all.equal')
all.equal.default
all.equal.language

?mode

requireNamespace

call. 

requireNamespace
eval_tbls

?bench_tbls
?seq_along
??compare
?paste0
install.packages("testthat")
library(testthat)
??testthat::expect_true
??invisible
stop
?as.call

cbind_list
??cbind_list__impl

chain
chain_q

parent.frame()
?eval

new.env
?%.%
%>%
?inherits
?mode


x <- 1
x
mode(X)
storage.mode(x)
mode(x)
typeof(x)

?inherits
?invisible

stopifnot
?deparse

?stopifnot

?trunc
?format

library(plyr)
?as.quoted
(X <- as.quoted(c("a", "b", "log(d)")))
X
as.quoted(a ~ b + log(d))

?colwise

head(baseball)

head(baseball, n = 100)
count(baseball[1:100,], vars = "id")

?create_progress_bar
(l_ply(1:100, identity, .progress = "none"))
(l_ply(1:100, identity, .progress = "tk"))

(l_ply(1:100, identity, .progress = "text"))
(l_ply(1:10000, identity, .progress = progress_text(char = ".")))
?ddply

each(min, max)(1, 10,100)

?liply
l_ply(1:100, identity, .progress = "text")
l_ply(1:100, function(x) Sys.sleep(.01), .progress = "time")
round_any(135, 10)
round_any(Sys.time() + 1:10, 5)


??splitter_d

?split

library(reshape2)

install.packages("reshape")
library(reshape)
?sweep


?melt


head(airquality)
(names(airquality) <- tolower(names(airquality)))
(melt(airquality, id=c("month", "day")))


?nulldefault

?mapply

mapply(rep, 1:4, 4:1)
?merge_recurse

?namerows
?by
?reshape
??stats
??guess_value
?deparse
reshape

?attr


a <- as.list(1:4)
length(a)
a
melt(a)
?varname
attr(a, "varname") <- "ID"
a
melt(a)
attr(a,"t") <-"ddd"
melt(a)
attr(a, 't')

?mapply

mapply(rep, times = 1:4, MoreArgs = list(x = 42))
mapply(rep, times = 1:4, x = 4:1)

(mapply(function(x, y) seq_len(x) + y,
        c(a =  1, b = 2, c = 3),  # names from first
        c(A = 10, B = 0, C = -10))
)

(x <- c(a =  1, b = 2, c = 3))
length(x)
length(c(a = 1))
x
seq_len(x)
seq_len(c(a =  1, b = 2, c = 3))

?preserve.na
?melt_check

?data


head(airquality)

airquality[, 'month', drop = FALSE]

?data.frame


library(reshape2)
parse_formula

library(Rwordseg)

library(tm)


library(TSA)

?stats:::acf
?acf
??xaxp

??mapply

??base

















