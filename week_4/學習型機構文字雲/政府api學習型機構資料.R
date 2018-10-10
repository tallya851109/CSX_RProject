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
library(rvest)
# 學習型機構資料集描述json url

link <- url(paste('https://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=24c9f8fe-88db-4a6e-895c-498fbc94df94'))
phwash = read_html(link)
docs <- Corpus(VectorSource(phwash))


#url <- 'https://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=24c9f8fe-88db-4a6e-895c-498fbc94df94'
#jsonData<-fromJSON(getURL("https://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=24c9f8fe-88db-4a6e-895c-498fbc94df94"))
#str( jsonData)

#Get it with jsonlite package
#jsonData <- fromJSON(url, flatten = TRUE)

#Write it into csv.
#docs <-write.csv(file = 'test.csv', jsonData , fileEncoding = 'utf-8')
#docs <- read.table(docs, header = T, row.names = 1, sep = ",")
#docs <- as.matrix(docs)
#filenames <- list.files(jsonData)
#files <- lapply(filenames, readLines, encoding="UTF-8")
#docs <- Corpus(VectorSource(files))

ptdm<- TermDocumentMatrix(phwash.doc)
pm <- as.matrix(ptdm)
pv <- sort(rowSums(pm),decreasing=TRUE)
pd <- data.frame(word = names(pv),freq=pv)

#freq <- rowSums(as.matrix(ptdm))
#pd.select<-head(sort(freq, decreasing = T))
d.select <- head(pd)
library(ggplot2)

ggplot(d.select, aes(x = word, y = freq)) + geom_bar(stat = "identity")
#移除可能有問題的符號
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)

#移除可能有問題的符號
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs, toSpace, "。")
docs <- tm_map(docs, toSpace, "，")
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "至")
docs <- tm_map(docs, toSpace, "子")
docs <- tm_map(docs, toSpace, "用")
docs <- tm_map(docs, toSpace, "有")
docs <- tm_map(docs, toSpace, "之")
docs <- tm_map(docs, toSpace, "段號")
docs <- tm_map(docs, toSpace, "在")
docs <- tm_map(docs, toSpace, "為")
docs <- tm_map(docs, toSpace, "用")
docs <- tm_map(docs, toSpace, "段")
docs <- tm_map(docs, toSpace, "巷")
docs <- tm_map(docs, toSpace, "號")
docs <- tm_map(docs, toSpace, "區")
docs <- tm_map(docs, toSpace, "街")
docs <- tm_map(docs, toSpace, "與")
docs <- tm_map(docs, toSpace, "在")
docs <- tm_map(docs, toSpace, "及")
docs <- tm_map(docs, toSpace, "士")
docs <- tm_map(docs, toSpace, "路")
docs <- tm_map(docs, toSpace, "之")
docs <- tm_map(docs, toSpace, "日")
docs <- tm_map(docs, toSpace, "中")

#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)

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

