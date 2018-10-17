library(NLP)
library(tm)
library(ggplot2)
library(stats)
library(proxy)
library(dplyr)
library(readtext)
library(jiebaRD)
library(jiebaR)
library(slam)
library(Matrix)
library(tidytext)
library(RColorBrewer)
library(wordcloud)

setwd("~/Desktop/CSX_RProject/week_5/迪士尼攻略txt檔")
rawData = readtext("*.txt")
docs = Corpus(VectorSource(rawData$text))
length(docs)
docs[[1]]
# data clean
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
})
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, toSpace, "[a-zA-Z]")

#移除可能有問題的符號
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs, toSpace, "個")
docs <- tm_map(docs, toSpace, "小")
docs <- tm_map(docs, toSpace, "下")
docs <- tm_map(docs, toSpace, "不")
docs <- tm_map(docs, toSpace, "中")
docs <- tm_map(docs, toSpace, "之")
docs <- tm_map(docs, toSpace, "再")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "們")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "在")
docs <- tm_map(docs, toSpace, "像是")
docs <- tm_map(docs, toSpace, "先祝")
docs <- tm_map(docs, toSpace, "內")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "出")
docs <- tm_map(docs, toSpace, "到")
docs <- tm_map(docs, toSpace, "去")
docs <- tm_map(docs, toSpace, "又")
docs <- tm_map(docs, toSpace, "和")
docs <- tm_map(docs, toSpace, "就")
docs <- tm_map(docs, toSpace, "看")
docs <- tm_map(docs, toSpace, "很")
docs <- tm_map(docs, toSpace, "我")
docs <- tm_map(docs, toSpace, "來")
docs <- tm_map(docs, toSpace, "著")
docs <- tm_map(docs, toSpace, "為")

#結巴字典
mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))

# words cut
keywords = read.csv("keywords.csv")
mixseg = worker()
keys = as.matrix(keywords)
new_user_word(mixseg, keys)

jieba_tokenizer = function(d){
  unlist(segment(d[[1]], mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))

d.corpus <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(d.corpus)
print( tf <- as.matrix(tdm) )
DF <- tidy(tf)

#Take a look at the summary .
docs

#Analyse how frequently terms appear by summing the content of all terms (i.e., rows).
freq=rowSums(as.matrix(tdm))
head(freq,10)

tail(freq,10)

#Plot those frequencies ordered.
plot(sort(freq, decreasing = T),col="red",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")

#See the ten most frequent terms.
tail(sort(freq),n=10)

#Show most frequent terms and their frequencies in a bar plot.
high.freq=tail(sort(freq),n=10)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 

ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")


#文字雲
library("jiebaR")
Sys.setlocale(category = "LC_ALL", locale = "cht")


library(wordcloud)

par(family=("HeitiTC Light"))

mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
par(family=("Heiti TC Light"))

wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),min.freq=10,max.words=50,
          random.order=FALSE, random.color=TRUE, 
          rot.per=0, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)


#形成TermDocumentMatrix, DocumentTermMatrix，把出現頻率大於3次的詞畫成Frequent Plot
hist(x=DF$X9, 
     main="出現頻率大於3次的詞",         # 圖片的名稱
     xlab="字詞",                      # X軸的名稱
     ylab="次數")


#docs.dfm <- dfm(docs, tolower = FALSE, remove = stopwords())
tf <- as.matrix(tdm) 
tdm <- t(tf)
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=3)
library(ggplot2)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip()

---------
#創建應用TF-IDF加權而不是術語頻率的TDM。
  
tf <- function(row){
    row / sum(row)
  }
idf <- function(col){
  corpus.size <- length(col)
  doc.count <- length(which(col>0))
  log10(corpus.size / doc.count)
}
tf.idf <- function(tf,idf){
  tf * idf
}
docs.df <- apply(dtm, 1, tf)
docs.idf <- apply(dtm, 2, idf)
docs.tfidf <- apply(docs.df, 2, tf.idf, idf <- docs.idf)


N = tdm$ncol
tf <- apply(tdm, 2, sum)
idfCal <- function(word_doc)
{ 
  log2( N / nnzero(word_doc) ) 
}
idf <- apply(tdm, 1, idfCal)

#tf-idf
doc.tfidf <- as.matrix(tdm)
for(x in 1:nrow(tdm))
{
  for(y in 1:ncol(tdm))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y]) * idf[x]
  }
}

#畫出前6個詞在text1中的tfidf

a <- as.data.frame(head(docs.tfidf[,1:5]))
ggplot(a,aes(y=a$text1,x=rownames(a)))+geom_bar(stat = "identity")+ xlab("word")+ ylab("TFIDF")

#畫出text1中tfidf最高的關鍵字
b <- as.data.frame(sort(docs.tfidf[,1],decreasing = TRUE)[1:5])
colnames(b) <- "text1keyword"
ggplot(b,aes(y=b$text1keyword, x = rownames(b))) +geom_bar(stat = "identity")+ xlab("word")+ ylab("TFIDF")+ coord_flip()

#畫出trump在300篇文章中的tfidf變化

c <- as.data.frame(docs.tfidf["trump",])
colnames(c) <- "text"
ggplot(c,aes(x = rownames(c), y = c$text)) + geom_bar(stat = "identity")+ xlab("text")+ ylab("TFIDF")


findZeroId = as.matrix(apply(doc.tfidf, 1, sum))
tfidfnn = doc.tfidf[-which(findZeroId == 0),]

write.csv(tfidfnn, "show.csv")


