library(tidyverse)
library(knitr)

#讀取資料以及需要的library

library(readr)
SalesTable <- read_csv("早餐店資料/SalesTable.csv")
ClientTable <- read_csv("早餐店資料/ClientTable.csv")
ProductTable <- read_csv("早餐店資料/ProductTable.csv")

#合併資料集後，利用str()看整組資料的結構
SalesTableNew <- SalesTable %>%
  inner_join(ClientTable, by = 'Client_ID') %>%
  inner_join(ProductTable, by = 'Product_ID')
kable(SalesTableNew[1:10,])

#各種編號都被讀成int，將所有Agency、Product_ID和Client_ID都轉為factor類別
SalesTableNew$Agency <- as.factor(SalesTableNew$Agency)
SalesTableNew$Product_ID <- as.factor(SalesTableNew$Product_ID)
SalesTableNew$Client_ID <- as.factor(SalesTableNew$Client_ID)

#銷售量、價格的關係
SalesTablePrice <- SalesTableNew %>%
  mutate( Unit_Price = Sales / Sales_Amount)

#單價和銷售量
ggplot(data = SalesTablePrice,
       aes( x = Unit_Price,
            y = Sales_Amount))+
  geom_point(color = 'red',
             alpha = 0.5) + theme_bw()

#不同客戶間的“銷售模式”有何差異
ggplot(SalesTableNew)+geom_boxplot( aes( x = factor(Client_Name),
                                         y = Sales,
                                         colour = Client_Name))+
  labs( x = 'Client',
        title = 'Sales Distribution by Client') + theme_bw()

#用bar chart看出過去一個月的總銷售量
SalesTableSum <- SalesTableNew %>%
  group_by( Client_Name) %>%
  summarise( Sales_Sum = sum(Sales)) %>%
  arrange(desc(Sales_Sum))

ggplot( data = SalesTableSum,
        aes( x = Client_Name,
             y = Sales_Sum,
             fill = Client_Name)) + 
  geom_bar( stat = 'identity') +
  
  scale_x_discrete(limits = SalesTableSum$Client_Name) +
  
  labs(title = 'Total Sales by Client',
       x = 'Client',
       y = 'Sales in total',
       fill = 'Client_Name') + theme_bw()

#整合起來看，我們就能知道每個客戶主要都購買哪些產品，也反映出前四個客戶佔了絕大部分的銷售額
SalesTableClient <- SalesTableNew %>%
  group_by(Client_Name, Product_Name) %>%
  summarise( Sales = sum(Sales))

ggplot( data = SalesTableClient) +
  geom_bar( aes( x = Product_Name,
                 y = Sales),
            stat = 'identity') +
  facet_wrap( ~ Client_Name)

