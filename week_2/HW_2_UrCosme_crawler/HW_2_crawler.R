# 引入套件
library(rvest)
## Loading required package: xml2
# 把網址餵給 read_html
read_html("https://m.urcosme.com/search/product?keyword=%E5%8F%A3%E7%B4%85&page=3")
## {xml_document}
## <html>
## [1] <head>\n    <meta content="IE=edge,chrome=1" http-equiv="X-UA-Compat ...
## [2] <body><div id="fb-root"/>\n  <script><![CDATA[\n    (function(d, s,  ...
doc <- read_html("https://m.urcosme.com/search/product?keyword=%E5%8F%A3%E7%B4%85&page=3") # 把網頁先存在一個變數裡，不用每次都連線造訪
doc %>% html_nodes(".product-info") # 取得 css 路徑下的所有節點
doc %>% html_nodes("..product-info") %>% html_attr("href") # 取得 href 屬性
doc %>% html_nodes(".product-info") %>% html_text() # 取得文字
