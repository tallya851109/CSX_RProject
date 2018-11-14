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

#輸出結果 
View(product_sunscreen_data)
#取出特定欄位 
product_sunscreen_data$label

#輸出型態 str(product_sunscreen_data)
library("XML")
#write.csv(table1[[1]], file = "Desktop")
######################## 2. Request: GET - 請求保養面膜產品面 ##############################]

# 保養面膜product的資料庫
product_mask_data<-data.frame()

#寫個迴圈，對固定網頁結構重複抓取
product_numbers <- c(68098, 81075, 78839,86843,86401 ,87996, 86402,86752,83472,86842)

for (q in product_numbers) {
    
    # 這裡分各項名稱抓取資料--------
    num_product <- q
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
  product_mask_data<-rbind(product_mask_data,product_info)  
}

#輸出結果 View(product_mask_data)
#輸出型態 str(product_mask_data)

######################## 3. Request: GET - 請求腮紅產品面 ##############################]

# 保養面膜product的資料庫
product_cheek_data<-data.frame()

#寫個迴圈，對固定網頁結構重複抓取
product_numbers <- c(57442, 11965, 76427,16763,74080 ,87802, 58479,73795,86986,79395)

for (q in product_numbers) {
  
    
    # 這裡分各項名稱抓取資料--------
    num_product <- q
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
  product_cheek_data<-rbind(product_cheek_data,product_info)  
}

#輸出結果 View(product_cheek_data)
#輸出型態 str(product_cheek_data)

######################## 4. Request: GET - 請求唇膏產品面 ##############################]


# 保養面膜product的資料庫
product_Lip_data<-data.frame()

#寫個迴圈，對固定網頁結構重複抓取
product_numbers <- c(79306, 77107, 49346,83591,83597, 87852, 87769,87727,88351,42501)

for (q in product_numbers) {
  

    
    # 這裡分各項名稱抓取資料--------
    num_product <- q
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
  product_Lip_data<-rbind(product_Lip_data,product_info)  
}

#輸出結果 
#View(product_Lip_data)
#輸出型態 str(product_Lip_data)

######################## 5. Request: GET - 請求睫毛膏產品面 ##############################]


# 保養面膜product的資料庫
product_mascara_data<-data.frame()

#寫個迴圈，對固定網頁結構重複抓取
product_numbers <- c( 87624, 75010 ,75935,75009,63695, 84882, 82936,58093,84520,85215)

for (q in product_numbers) {
  
  
  
  # 這裡分各項名稱抓取資料--------
  num_product <- q
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
  product_mascara_data<-rbind(product_mascara_data,product_info)  
}

#輸出結果 
#View(product_mascara_data)
#輸出型態 str(product_mascara_data)

