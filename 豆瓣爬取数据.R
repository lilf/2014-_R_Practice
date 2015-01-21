install.packages("Rcurl","XML")
library(XML)
install.packages("Rcurl")
library("MASS")
install.packages("RCurl")
library("RCurl")
install.packages("XML")
library("XML")
movieScore <- function(x) {
  stopifnot(is.character(x))
  search <- getForm("http://movie.douban.com/subject_search", search_text = x)
  searchweb <- htmlParse(search)
  resnodes <-getNodeSet(searchweb, "//div[@id='wrapper']//table[1]//a")
  if (is.null(resnodes))
    return(NULL) else resurl <- xmlGetAttr(resnodes[[1]], name = "#href")
  
  resweb <-getURL(resurl, encoding = "UTF-8")
  content <-htmlParse(resweb, encoding = "UTF-8")
  resnodes <- getNodeSet(content, "div[@id='interest_sectl']//p[@class='rating_self clearfix']//strong")
  namenodes <-getNodeSet(content, "//div[@id='content']//h1//span")
  score <- xmlValue(resnodes[[1]])
  name <- xmlValue(namenodes[[1]])
  return(list(name = name, score = score))
}
movieScore("??????")

movieScore <- function(x) {
  stopifnot(is.character(x)) #?????????tru????????????stop
  # ????????????????????????
  search <- getForm("http://movie.douban.com/subject_search", search_text = x) #search_text???x???name-value??????
  searchweb <- htmlParse(search)#???content?????????HTML???????????????htmlTreeParse
  # ????????????????????????
  resnodes <- getNodeSet(searchweb, "//div[@id='wrapper']//table[1]//a") #getNodeSet??????XML??????nodes
  if (is.null(resnodes)) 
    return(NULL) else resurl <- xmlGetAttr(resnodes[[1]], name = "href")
  # ????????????????????????????????????
  resweb <- getURL(resurl, .encoding = "UTF-8")
  content <- htmlParse(resweb, encoding = "UTF-8")
  resnodes <- getNodeSet(content, "//div[@id='interest_sectl']//p[@class='rating_self clearfix']//strong")
  namenodes <- getNodeSet(content, "//div[@id='content']//h1//span")
  # ??????????????????
  score <- xmlValue(resnodes[[1]])
  name <- xmlValue(namenodes[[1]])
  return(list(name = name, score = score))
}
search <- getForm("http://movie.douban.com/subject_search", search_text = "??????")
search
searchweb <- htmlParse(search)
searchweb
resnodes <- getNodeSet(searchweb, "//div[@id='wrapper']//table[1]//a")
resnodes
resurl <- xmlGetAttr(resnodes[[1]], name = "href")
resurl
resweb <- getURL(resurl, .encoding = "UTF-8")
resweb
content <- htmlParse(resweb, encoding = "UTF-8")
content
resnodes <- getNodeSet(content, "//div[@id='interest_sectl']//p[@class='rating_self clearfix']//strong")
resnodes
score <- xmlValue(resnodes[[1]])
score
name <- xmlValue(namenodes[[1]])
name

install.packages("RJSONIO")
library(RJSONIO)
library(RCurl)
library(XML)
movieScoreapi <- function(x) {
  api <- "https://api.douban.com/v2/movie/search?q={"
  url <- paste(api, x, "}", sep = "")
  res <- getURL(url)
  reslist <- fromJSON(res)
  name <- reslist$subjects[[1]]$title
  score <- reslist$subjects[[1]]$rating$average
  return(list(name = name, score = score))
}
movieScoreapi("?????????")
























