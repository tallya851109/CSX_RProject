# install.packages(c('httr', 'rvest', 'XML', 'magrittr', 'DT', 'stringr', 'jsonlite', 'RCurl','data.table',
#                    'progress', 'plyr'))

######################## library loaded ##############################
library(httr) # 爬蟲 GET()
library(rvest)
library(stringr) 
library(dplyr)
library(data.table)
library(DT)
library(RCurl)
library(tidyverse)
library(parallel)
library(plyr)
library(progress)

char_encode=URLencode(iconv('Maintenance_mask',to= 'UTF-8' ))

######################## 1. Request: GET - 請求臉部防曬產品面 ##############################]

# 臉部防曬product的資料庫
product_sunscreen_data<-data.frame()

#寫個迴圈，對固定網頁結構重複抓取
product_numbers <- c(85434, 85531, 57764,85617,79069,79852,85937,68153,21546,86219)

for (i in product_numbers) {
  
  # 這裡分各項名稱抓取資料--------
  num_product <- i
  url_product=sprintf("https://www.urcosme.com/products/%s",num_product)
  doc_product<- GET(url_product) %>% content(encoding = "utf8")
  
  #extract data wanted
  # 1.1 選定之產品資訊 ####
  
  # 產品名稱
  product_name = doc_product %>% 
    html_nodes("div.headline-title.product-name") %>% 
    html_text()
  
  # 品牌名稱
  brand = doc_product %>% 
    html_nodes("div.brand-name > a.uc-main-link") %>% 
    html_text()
  
  # 系列
  series_unm_temp = doc_product %>% 
    html_nodes('div.detail-label')%>% 
    html_text()
  
  series = doc_product %>% 
    html_nodes('div.detail-text > a.uc-main-link')%>% 
    html_text() %>% .[grep('系列', series_unm_temp)]
  
  
  # 屬性
  # attribute = doc_product %>% 
  #   html_nodes('div.detail-text > a.uc-main-link')%>% 
  #   html_text()[5]
  
  attribute_temp= doc_product %>% 
    html_nodes('div.detail-text > span')%>% 
    html_text()
  
  attribute_total_seq = sum(str_count(' > ', attribute_temp))
  
  attribute = doc_product %>% 
    html_nodes('div.detail-text > a.uc-main-link')%>% 
    html_text() %>%  .[grep('屬性', series_unm_temp)+attribute_total_seq]
  
  # 標籤
  # label = doc_product %>% html_nodes('div.detail-label')%>% html_node("#系列") 
  #   html_text()
  #label_total_seq =  sum(str_count('、', attribute_temp) ) - (attribute_total_seq+ 3) - 1
  label =  doc_product %>% 
    html_nodes('div.detail-text > a.uc-main-link') %>% 
    html_text() %>% .[6:length(.)]
  label = paste(label, collapse = '、')
  
  # 指數
  rate = doc_product %>%
    html_nodes( 'div.score-txt') %>% 
    html_text() %>% parse_number()
  
  # Y變數 - 分解為 人氣、升火、買過'要理解grep, str_split_fixed'
  y_all = doc_product %>%
    html_nodes('div.product-info-engagement-counts') %>% 
    html_text()
  
  y_all_split = str_split_fixed(y_all, pattern = '/',3)
  
  popularity_num = grep('人氣', y_all_split)
  fire_num = grep('升火', y_all_split)
  purchase_num = grep('買過', y_all_split)
  
  popularity =y_all_split[popularity_num] %>% parse_number()
  fire=y_all_split[fire_num] %>% parse_number()
  purchase = y_all_split[purchase_num]%>% parse_number()
  
  # 價格
  price = doc_product %>%
    html_nodes('div.other-text') %>% 
    html_text()%>%grep('NT\\$', ., value = T) %>%   parse_number()
  
  # 商品說明
  description = doc_product %>%
    html_nodes('div.product-desc-content') %>% 
    html_text()
  
  # make data frame
  product_info <- data.frame(product_name = ifelse(length(product_name)==0,NA,product_name), 
                             brand = ifelse(length(brand)==0,NA,brand), 
                             description = ifelse(length(description)==0,NA,description), 
                             series = ifelse(length(series)==0,NA,series),
                             attribute = ifelse(length(attribute)==0,NA,attribute),
                             label = ifelse(length(label)==0,NA,label),
                             rate = ifelse(length(rate)==0,NA,rate),
                             popularity = ifelse(length(popularity)==0,NA,popularity),
                             fire = ifelse(length(fire)==0,NA,fire),
                             purchase = ifelse(length(purchase)==0,NA,purchase),
                             price = ifelse(length(price)==0,NA,price))
  
  
  #把此次資料輸到新的資料庫中
  product_sunscreen_data<-rbind(product_sunscreen_data,product_info)  
}

#輸出結果  View(product_sunscreen_data)

#====================lable進行統計================

#取出特定欄位 
product_sunscreen_data$label
docs <- Corpus(VectorSource(product_sunscreen_data$label))

#library

library(NLP)
library(tm) #文字探勘
library(jiebaRD)
library(jiebaR) #中文斷詞 
library(RColorBrewer)
library(wordcloud) #產生文字雲

#移除可能有問題的符號
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
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


#===========================================================================
#library
library(xml2)
library(rvest)
library(stringr)
library(readr)
library(dplyr)

# EXAMPLE A (防曬 各label統計) (畫長條圖)
###

label_count <- product_sunscreen_data$label %>% unlist %>% count 
label_count = label_count[rev(order(label_count$freq)),]

label_count <-as.data.frame(label_count)
#只取有在tag_list中的

label_count_ggplot<-ggplot(label_count, aes(x = label) + 
  geom_bar(stat = "identity", fill='lightblue') + 
  labs(x='label',title='防曬標籤數量統計') + 
  theme(panel.background = element_blank(),
        axis.title = element_text(color = '#2d2d2d'),
        axis.text.x = element_text(hjust = 1, size=15),
        axis.text.y = element_text(hjust = 1, size=15),
        strip.text.x = element_text(color='#2d2d2d',face='bold',size=10),
        plot.title = element_text(hjust=0.5,face='bold',size=15))
label_count_ggplot

my.plot2 <- ggplot(label_count, aes(x = label))
my.plot2 <- my.plot2 + layer(
  geom = "bar",
  stat = "bin",
  position = "identity",
  params = list(
    fill = "steelblue",
    binwidth = 0.2,
    na.rm = FALSE
  )
)
my.plot2
