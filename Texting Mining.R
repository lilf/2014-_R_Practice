install.packages("tm")
library(tm)
vignette('tm')
??tm
#ָ���ļ���·��
txt <- system.file("texts", "txt", package = "tm")
txt
?DirSource #����һ��Ŀ¼Դ
?Corpus #��̬���������ݽṹ������
ovid <- Corpus(DirSource(txt), readerControl = list(language = "lat"))
ovid
??tm
?PCorpus

#ͬ���ɴ��ַ������������Ͽ�
docs <- c("This is a text", "This is another one.")
docs
Corpus(VectorSource(docs))


#���ݶ�̬�ĵ�����һ�����Ͽ�
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- Corpus(DirSource(reut21578), readerControl = list(reader = readReut21578XML))
reuters
writeCorpus(ovid)

inspect(ovid[1:2])
#���ڵ����ĵ�����ȡ��Ҫʹ��[[���ȿ�ͨ��λ��Ҳ��ͨ������
identical(ovid[[2]], ovid[["ovid_2.txt"]])
ovid[[2]]
ovid[["ovid_2.txt"]]
str(ovid)
attributes(ovid)

?mappings

?tm_map
reuters <- tm_map(reuters, as.PlainTextDocument)
reuters

#ȥ������Ŀհ�
reuters <- tm_map(reuters, stripWhitespace)
reuters
#Сд�仯
returs <- tm_map(reuters, tolower)
#gsub���и��㷺���ַ�����
??gsub
#ֹͣ��ȥ��
reuters <- tm_map(reuters, removeWords, stopwords("english"))
#���
#��ҪSnowball��(���м���)֧��
tm_map(reuters, stemDocument)

??tm

#�������ҳ�ID����237����ͷ��heading������"INDONESIA SEEN AT CROSSROADS OVER ECONOMIC CHANGE"�ַ����ı��ļ�
query <- "id == '237' & heading == 'INDONESIA SEEN AT CROSSROADS OVER ECONOMIC CHANGE'"
tm_filter(reuters, FUN = sFilter, query)

#���ɽ���ȫ�Ĺ���
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
#findFreqTerms��document-term��term-document�����в���terms��Ƶ��
findFreqTerms(dtm, 5)

?findAssocs  #�ɽ���һ��ľ��󣬶�һ����󣬿ɽ�����ֱ��ת��Ϊ�����
#�Ӹ�����document-term��tem-document�����в��ҳ��������term�й�����
findAssocs(dtm, "opec", 0.8)
#����-�ĵ���ϵ�������ݼ�һ��ϴ󣬿ɲ���ɾ��ϡ����Ŀ�ķ���������Щ��Ŀ�ں��ٵ��ĵ��г���
inspect(removeSparseTerms(dtm, 0.4)) #�޳��˵���40%��ϡ����Ŀ

library(tm)
Dictionary
(d <- Dictionary(c("prices", "crude", "oil")))
?DocumentTermMatrix
inspect(DocumentTermMatrix(reuters, list(dictionary = d)))

??tm

?read.csv


































