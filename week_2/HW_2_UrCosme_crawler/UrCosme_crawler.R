#安裝“rvest”包來刮取數據：install.packages（“rvest”）
#找到正確的 css 路徑
#Load the library：（rvest）

#url = "https://www.urcosme.com/tags/36/ranking"
#res <- GET(url)
#content(res, "text", encoding = "big5")
#html <- htmlParse(content(res, "text", encoding = "big5"), encoding = "utf8")
# Parser
# library(stringr)

#tables <- readHTMLTable(html)
library(httr)
library(rvest)
html_li <- "https://www.urcosme.com/tags/36/ranking"
ranking_cosmetic <- read_html(html_li)
# 分析的網頁
#ranking_cosmetic <- read_html("https://www.urcosme.com/tags/36/ranking")

# 抓產品名
product_list <- ranking_cosmetic %>% 
  html_nodes(".single-dot") %>% 
  html_text()

product_list
product_list_new <- c()
for (i in 1:length(product_list)) {
  if (i %% 2 == 1) {
    product_list_new <- c(product_list_new, product_list[i])
  }
}
length(product_list_new)

product_list_new <- paste0(".single-dot" , product_list)
View(product_list_new)

# 抓價錢
money_list <- ranking_cosmetic %>% 
  html_nodes(".uc-tag-ranking-item:nth-child(11) span , .uc-tag-ranking-item:nth-child(9) span , .uc-tag-ranking-item:nth-child(5) span , .uc-tag-ranking-item:nth-child(2) span , span~ span+ span") %>%
  html_text()

money_list

length(money_list)

##去除資料  
#money_list<-gsub("｜",  "," ,money_list[127])

#money_list <- paste0("product-market-date , span+ span" , money)
#View(money_list)


# 評價
score_list <- ranking_cosmetic %>% 
  html_nodes(".urcosme-heart-full~ .product-score-text") %>%
  html_text()
as.numeric()
length(score_list)
score_list

# 抓圖
poster_list <- ranking_cosmetic %>%
  html_nodes(".img-auto-center ") %>%
  html_attr("src")
poster_list
length(poster_list)

#news_li <- data.frame("評價"= score, "產品"= product, "價錢"=money, "圖片"=poster)
#View(news_li)
#paste(product_list, money_list, score_list,sep="")

##長度不同不能合起來
#colnames(df_cosmetic)<-c("產品","價格","分數","圖片")
#df_cosmetic = data.frame(product_list, money_list, score_list)
#df_cosmetic



#排序 評價 產品 價位 圖片 列成一列

#x <- cbind(c(product), c(money))
#y <- cbind(c(product), c(score))
#z <- cbind(c(product), c(poster))
#colnames(x) <- c("product", "money")
#colnames(y) <- c("product", "score")
#colnames(z) <- c("product", "poster")

#merge(x, y , by = "product", all = T) 


 
 