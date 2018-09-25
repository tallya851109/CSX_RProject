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
product <- ranking_cosmetic %>% 
  html_nodes(".single-dot") %>% 
  html_text()
as.numeric()
product
product <- paste0(".single-dot" , product)
View(product)

# 抓價錢、日期
money <- ranking_cosmetic %>% 
  html_nodes(".product-market-date , span+ span") %>%
  html_text()
money
money <- paste0("product-market-date , span+ span" , money)
View(money)


# 評價
score <- ranking_cosmetic %>% 
  html_nodes(".product-score-text , .brand-name .uc-minor-link") %>%
  html_text()
score

# 抓圖
poster <- ranking_cosmetic %>%
  html_nodes(".img-auto-center ") %>%
  html_attr("src")
poster

news_li <- data.frame("評價"= score, "產品"= product, "價錢、日期"=money, "圖片"=poster)
View(news_li)

#排序 評價 產品 價位 圖片 列成一列

x <- cbind(c(product), c(money))
y <- cbind(c(product), c(score))
#z <- cbind(c(product), c(poster))
colnames(x) <- c("product", "money")
colnames(y) <- c("product", "score")
#colnames(z) <- c("product", "poster")

merge(x, y , by = "product", all = T) 


 
 