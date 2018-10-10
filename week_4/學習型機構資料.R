#清空所有記憶體
rm(list=ls(all.names = TRUE))
library(NLP)
library(tm) #文字探勘
library(jiebaRD)
library(jiebaR) #中文斷詞 
library(RColorBrewer)
library(wordcloud) #產生文字雲


filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines, encoding="UTF-8")
docs <- Corpus(VectorSource(files))

#語詞詞幹化 (stemmization)
#以英文為例
#https://zh.wikipedia.org/wiki/%E8%AF%8D%E5%B9%B2%E6%8F%90%E5%8F%96
#library(SnowballC)
#確保任何形式的單字只會轉換成相同詞性出現一次
#docs <- tm_map(docs, stemDocument)
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


#移除標點符號 (punctuation)
#移除數字 (digits)、空白 (white space)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)


#結巴字典
mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))



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


