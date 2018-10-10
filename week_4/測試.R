install.packages("rJava")
install.packages("Rwordseg", repos="http://R-Forge.R-project.org")
install.packages("tm")
install.packages("tmcn", repos="http://R-Forge.R-project.org", type="source")
install.packages("wordcloud")
install.packages("XML")
install.packages("RCurl")
Sys.setenv(JAVA_HOME="C:\\Program Files (x86)\\Java\\jre1.8.0_71\\bin\\client\\")

library(rJava)
install.packages("Rwordseg", repos = "http://R-Forge.R-project.org")
library(Rwordseg)
library(tm) # 啟用Corpus使用
library(tmcn)  
library(rjson)
library(tm)
segmentCN("今天一直下雨")  # 測試

d.corpus <- Corpus(DirSource(directory ="C:/Users/vaio/Desktop/Wordlog/"), list(language = NA)) # 讀取文字檔(ANSI)，指定到資料夾層級) 
d.corpus <- tm_map(d.corpus, removePunctuation) # 移除標點符號
d.corpus <- tm_map(d.corpus, removeNumbers) # 移除數字
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})  # 移除大小寫英文

inspect(d.corpus)


d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE) # 進行中文斷詞

d.corpus <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) == "n"]
  })
  unlist(noun)
}) # 選取名詞


myStopWords <- c(stopwordsCN(), "貼圖", "圖片")
d.corpus <- tm_map(d.corpus, removeWords, myStopWords)


inspect(d.corpus)
d.corpus <- tm_map(d.corpus, PlainTextDocument)
d.corpus <- Corpus(VectorSource(d.corpus)) # 轉向量矩陣

inspect(d.corpus)

tdm <- TermDocumentMatrix(d.corpus, control = list(wordLengths = c(2, Inf)))
inspect(tdm)


par(family=("Heiti TC Light"))

wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.5),min.freq=10,max.words=50,
          random.order=FALSE, random.color=TRUE, 
          rot.per=0, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)
    
    