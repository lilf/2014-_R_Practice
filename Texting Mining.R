install.packages("tm")
library(tm)
vignette('tm')
??tm
#指定文件的路径
txt <- system.file("texts", "txt", package = "tm")
txt
?DirSource #创建一个目录源
?Corpus #动态集合中数据结构及操作
ovid <- Corpus(DirSource(txt), readerControl = list(language = "lat"))
ovid
??tm
?PCorpus

#同样可从字符向量创建语料库
docs <- c("This is a text", "This is another one.")
docs
Corpus(VectorSource(docs))


#根据动态文档创建一个语料库
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- Corpus(DirSource(reut21578), readerControl = list(reader = readReut21578XML))
reuters
writeCorpus(ovid)

inspect(ovid[1:2])
#对于单个文档的提取需要使用[[，既可通过位置也可通过名称
identical(ovid[[2]], ovid[["ovid_2.txt"]])
ovid[[2]]
ovid[["ovid_2.txt"]]
str(ovid)
attributes(ovid)

?mappings

?tm_map
reuters <- tm_map(reuters, as.PlainTextDocument)
reuters

#去除多余的空白
reuters <- tm_map(reuters, stripWhitespace)
reuters
#小写变化
returs <- tm_map(reuters, tolower)
#gsub中有更广泛的字符操作
??gsub
#停止词去除
reuters <- tm_map(reuters, removeWords, stopwords("english"))
#填充
#需要Snowball包(并行计算)支持
tm_map(reuters, stemDocument)

??tm

#假如需找出ID等于237，表头（heading）包含"INDONESIA SEEN AT CROSSROADS OVER ECONOMIC CHANGE"字符的文本文件
query <- "id == '237' & heading == 'INDONESIA SEEN AT CROSSROADS OVER ECONOMIC CHANGE'"
tm_filter(reuters, FUN = sFilter, query)

#还可进行全文过滤
tm_filter(reuters, pattern = "company")
tm_filter(reuters, FUN = searchFullText, "company")

library(tm)
DublinCore(crude[[1]], tag = "creator") <- "Ano Nymous"
DublineCore(crude[[1]])
meta(crude[[1]])

meta(crude, tag = "test", type = "corpus") <- "test meta"
meta(crude, type = "corpus")


meta(crude, "foo") <- letters[1:20]
meta(crude)

?DocumentTermMatrix
data("crude")
crude

reuters
dtm <- DocumentTermMatrix(reuters)
inspect(dtm[1:5, 100:105])

?findFreqTerms
#findFreqTerms在document-term或term-document矩阵中查找terms的频次
findFreqTerms(dtm, 5)

?findAssocs  #可接受一般的矩阵，对一般矩阵，可将矩阵直接转化为相关阵
#从给定的document-term或tem-document矩阵中查找出与给定的term有关联的
findAssocs(dtm, "opec", 0.8)
#词条-文档关系矩阵数据集一般较大，可采用删减稀疏条目的方法，如有些条目在很少的文档中出现
inspect(removeSparseTerms(dtm, 0.4)) #剔除了低于40%的稀疏条目

library(tm)
Dictionary
(d <- Dictionary(c("prices", "crude", "oil")))
?DocumentTermMatrix
inspect(DocumentTermMatrix(reuters, list(dictionary = d)))

??tm

?read.csv



































