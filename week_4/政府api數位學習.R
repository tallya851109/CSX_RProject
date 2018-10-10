#清空
rm(list=ls(all.names = TRUE))
#叫出library
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)


# Taipei api http://oops.gov.taipei/
library(jsonlite)
library(RCurl)

# 學習型機構資料集描述json url
url <- 'https://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=24c9f8fe-88db-4a6e-895c-498fbc94df94'
jsonData<-fromJSON(getURL("https://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=24c9f8fe-88db-4a6e-895c-498fbc94df94"))
str( jsonData)

#Get it with jsonlite package
#jsonData <- fromJSON(url, flatten = TRUE)

#Write it into csv.
#docs <-write.csv(file = 'test.csv', jsonData , fileEncoding = 'utf-8')
#docs <- read.table(docs, header = T, row.names = 1, sep = ",")
#docs <- as.matrix(docs)
filenames <- list.files(jsonData)
files <- lapply(filenames, readLines, encoding="UTF-8")
docs <- Corpus(VectorSource(files))

#移除可能有問題的符號
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)

jsonData <- tm_map(jsonData, toSpace, "※")
jsonData <- tm_map(jsonData, toSpace, "◆")
jsonData <- tm_map(jsonData, toSpace, "[a-zA-Z]")
#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
jsonData <- tm_map(jsonData, removePunctuation)
jsonData <- tm_map(jsonData, removeNumbers)
jsonData <- tm_map(jsonData, stripWhitespace)
#語詞詞幹化 (stemmization)
#以英文為例
#https://zh.wikipedia.org/wiki/%E8%AF%8D%E5%B9%B2%E6%8F%90%E5%8F%96
#library(SnowballC)
#確保任何形式的單字只會轉換成相同詞性出現一次
#docs <- tm_map(docs, stemDocument)
mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))

par(family=("Heiti TC Light"))

wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),min.freq=10,max.words=50,
          random.order=FALSE, random.color=TRUE, 
          rot.per=0, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
