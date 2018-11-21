
#清空所有記憶體
rm(list=ls(all.names = TRUE))
library(NLP)
library(tm) #文字探勘
library(jiebaRD)
library(jiebaR) #中文斷詞 
library(RColorBrewer)
library(wordcloud) #產生文字雲

library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
#install.packages("extrafont");
# ggplot font size
#亂碼方框
library(extrafont)
loadfonts()
par(family="STKaiti")


#因為mac編碼問題讀入csv檔案會較麻煩，所以讀入XML

library(readxl)
臉部防曬 <- read_excel("~/Desktop/各品項爬蟲資料/臉部防曬.xlsx")
保養面膜 <- read_excel("~/Desktop/各品項爬蟲資料/保養面膜.xlsx")
篩紅爬蟲<- read_excel("~/Desktop/各品項爬蟲資料/篩紅爬蟲.xlsx")
唇膏爬蟲 <- read_excel("~/Desktop/各品項爬蟲資料/唇膏爬蟲.xlsx")
睫毛爬蟲 <- read_excel("~/Desktop/各品項爬蟲資料/睫毛爬蟲.xlsx")

#-------------------
tmp <- cbind(
  保養面膜[, -c(6,8:12)]
)
保養面膜 <- 保養面膜[,8:12] %>%
  mutate(
    rate = (rate - min(rate)) / (max(rate)-min(rate)),
    popularity = (popularity - min(popularity)) / (max(popularity)-min(popularity)),
    fire = (fire - min(fire)) / (max(fire)-min(fire)),
    purchase = (purchase - min(purchase)) / (max(purchase)-min(purchase)),
    price = (price - min(price)) / (max(price)-min(price))
  ) %>% cbind(
    保養面膜[,c(1:7)]
  )

kable(保養面膜[1:5, ])

tmp <- cbind(
  保養面膜[, -c(6,8:12)]
)


ggplot(tmp, aes(x=purchase, y=price)) + 
  geom_point(aes(colour = factor(rate), size = popularity)) + 
  guides(col = guide_legend(override.aes = list(shape = 15, size = 10)))+
  #geom_point(aes(shape = factor(price)))+
  geom_text( size=2,aes(label=name), family="Noto Sans CJK TC")

#-------------------
臉部防曬 <- 臉部防曬[,8:12] %>%
  mutate(
    rate = (rate - min(rate)) / (max(rate)-min(rate)),
    popularity = (popularity - min(popularity)) / (max(popularity)-min(popularity)),
    fire = (fire - min(fire)) / (max(fire)-min(fire)),
    purchase = (purchase - min(purchase)) / (max(purchase)-min(purchase)),
    price = (price - min(price)) / (max(price)-min(price))
  ) %>% cbind(
    臉部防曬[,c(1:7)]
  )

kable(臉部防曬[1:5, ])

tmp <- cbind(
  臉部防曬[, -c(6,8:12)]
)


ggplot(tmp, aes(x=purchase, y=price)) + 
  geom_point(aes(colour = factor(rate), size = popularity)) + 
  guides(col = guide_legend(override.aes = list(shape = 15, size = 10))) +
  #geom_point(aes(shape = factor(price)))+
  geom_text( size=2,aes(label=name), family="Noto Sans CJK TC")
