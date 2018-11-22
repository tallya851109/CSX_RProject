library(tidyverse)
library(knitr)
library(readxl)
#install.packages("extrafont");
# ggplot font size
#亂碼方框

#library(extrafont)
loadfonts()
par(family="Noto Sans CJK TC") 

保養面膜 <- read_excel("~/Desktop/各品項爬蟲資料/保養面膜.xlsx")

#保養面膜  唇膏爬蟲 睫毛爬蟲 篩紅爬蟲 臉部防曬

#讀進來的資料：

str(保養面膜)
kable(保養面膜[1:5,])

#類別變數：轉換為1和0
#數值變數：轉換至「以最大最小值為1和0的尺度中」

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

保養面膜 <- cbind(
  保養面膜[, -c(6:12)]
)


kable(保養面膜[1:5,])

library(reshape2)

CorMatrix <- 保養面膜 %>% cor() %>% melt()

kable(CorMatrix[ 1:5,])

#ggplot2套件完成熱密度圖

ggplot( data = CorMatrix) +
  
  geom_tile(aes(Var1, Var2,fill = value), colour = "white") + 
  
  scale_fill_gradient2(low = "firebrick4", high = "steelblue") +
  
  guides(fill=guide_legend(title="Correlation")) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
#階層式集群分析作為資料探索的方法，利用dist()函數計算距離後，再用hclust()函數進行分析及繪圖。

set.seed(500)
Distance <- dist(保養面膜, method = 'euclidean')

hclust(Distance, method = 'complete') %>% plot()

#建模將以這2、3群數分別討論。

#1. 分為兩群
#用kmeans()演算法，決定分群數目後、進行分群後
# Implement Kmeans Algorithm:

set.seed(500) # remove the random effect
K <- kmeans(保養面膜,3)

ClusterResult <- cbind(
  保養面膜,
  K$cluster
) %>% as.data.frame()

colnames(ClusterResult)[ncol(ClusterResult)] <- 'Cluster'

table(ClusterResult$Cluster)

#納入主成份分析
library(ggfortify)
set.seed(500)
autoplot(kmeans(tmp[1:5], 3), data  = tmp) + 
  geom_text(aes(label=name),hjust=0, vjust=0)+ 
  theme(text = element_text(family = "Noto Sans CJK TC"))

#_________________________
唇膏爬蟲 <- read_excel("~/Desktop/各品項爬蟲資料/唇膏爬蟲.xlsx")

#保養面膜  唇膏爬蟲 睫毛爬蟲 篩紅爬蟲 臉部防曬

#讀進來的資料：

str(唇膏爬蟲)
kable(唇膏爬蟲[1:5,])

#類別變數：轉換為1和0
#數值變數：轉換至「以最大最小值為1和0的尺度中」

唇膏爬蟲 <- 唇膏爬蟲[,8:12] %>%
  mutate(
    rate = (rate - min(rate)) / (max(rate)-min(rate)),
    popularity = (popularity - min(popularity)) / (max(popularity)-min(popularity)),
    fire = (fire - min(fire)) / (max(fire)-min(fire)),
    purchase = (purchase - min(purchase)) / (max(purchase)-min(purchase)),
    price = (price - min(price)) / (max(price)-min(price))
  ) %>% cbind(
    唇膏爬蟲[,c(1:7)]
  )

kable(唇膏爬蟲[1:5, ])

tmp <- cbind(
  唇膏爬蟲[, -c(6,8:12)]
)

唇膏爬蟲 <- cbind(
  唇膏爬蟲[, -c(6:12)]
)
kable(唇膏爬蟲[1:5,])

library(reshape2)

CorMatrix <- 唇膏爬蟲 %>% cor() %>% melt()

kable(CorMatrix[ 1:5,])

#ggplot2套件完成熱密度圖

ggplot( data = CorMatrix) +
  
  geom_tile(aes(Var1, Var2,fill = value), colour = "white") + 
  
  scale_fill_gradient2(low = "firebrick4", high = "steelblue") +
  
  guides(fill=guide_legend(title="Correlation")) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
#階層式集群分析作為資料探索的方法，利用dist()函數計算距離後，再用hclust()函數進行分析及繪圖。

set.seed(500)
Distance <- dist(唇膏爬蟲, method = 'euclidean')

hclust(Distance, method = 'complete') %>% plot()

#建模將以這3、4兩種群數分別討論。

#1. 分為三群
#用kmeans()演算法，決定分群數目後、進行分群後
# Implement Kmeans Algorithm:

set.seed(500) # remove the random effect
K <- kmeans(唇膏爬蟲,3)

ClusterResult <- cbind(
  唇膏爬蟲,
  K$cluster
) %>% as.data.frame()

colnames(ClusterResult)[ncol(ClusterResult)] <- 'Cluster'

table(ClusterResult$Cluster)

#納入主成份分析
library(ggfortify)
set.seed(500)
autoplot(kmeans(唇膏爬蟲[,1:5], 3), data = tmp) + 
  geom_text(aes(label=name),hjust=0, vjust=0)+ 
  theme(text = element_text(family = "Noto Sans CJK TC"))

#_________________________
睫毛爬蟲 <- read_excel("~/Desktop/各品項爬蟲資料/睫毛爬蟲.xlsx")

#保養面膜  唇膏爬蟲 睫毛爬蟲 篩紅爬蟲 臉部防曬

#讀進來的資料：

str(睫毛爬蟲)
kable(睫毛爬蟲[1:5,])

#類別變數：轉換為1和0
#數值變數：轉換至「以最大最小值為1和0的尺度中」

睫毛爬蟲 <- 睫毛爬蟲[,8:12] %>%
  mutate(
    rate = (rate - min(rate)) / (max(rate)-min(rate)),
    popularity = (popularity - min(popularity)) / (max(popularity)-min(popularity)),
    fire = (fire - min(fire)) / (max(fire)-min(fire)),
    purchase = (purchase - min(purchase)) / (max(purchase)-min(purchase)),
    price = (price - min(price)) / (max(price)-min(price))
  ) %>% cbind(
    睫毛爬蟲[,c(1:7)]
  )

kable(睫毛爬蟲[1:5, ])

tmp <- cbind(
  睫毛爬蟲[, -c(6,8:12)]
)

睫毛爬蟲 <- cbind(
  睫毛爬蟲[, -c(6:12)]
)
kable(睫毛爬蟲[1:5,])

library(reshape2)

CorMatrix <- 睫毛爬蟲 %>% cor() %>% melt()

kable(CorMatrix[ 1:5,])

#ggplot2套件完成熱密度圖

ggplot( data = CorMatrix) +
  
  geom_tile(aes(Var1, Var2,fill = value), colour = "white") + 
  
  scale_fill_gradient2(low = "firebrick4", high = "steelblue") +
  
  guides(fill=guide_legend(title="Correlation")) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
#階層式集群分析作為資料探索的方法，利用dist()函數計算距離後，再用hclust()函數進行分析及繪圖。

set.seed(500)
Distance <- dist(睫毛爬蟲, method = 'euclidean')

hclust(Distance, method = 'complete') %>% plot()

#建模將以這3、4兩種群數分別討論。

#1. 分為4群
#用kmeans()演算法，決定分群數目後、進行分群後
# Implement Kmeans Algorithm:

set.seed(500) # remove the random effect
K <- kmeans(睫毛爬蟲,4)

ClusterResult <- cbind(
  睫毛爬蟲,
  K$cluster
) %>% as.data.frame()

colnames(ClusterResult)[ncol(ClusterResult)] <- 'Cluster'

table(ClusterResult$Cluster)

#納入主成份分析
library(ggfortify)
set.seed(500)
autoplot(kmeans(睫毛爬蟲[,1:5], 4), data = tmp) + 
  geom_text(aes(label=name),hjust=0, vjust=0)+ 
  theme(text = element_text(family = "Noto Sans CJK TC"))


#_________________________

篩紅爬蟲 <- read_excel("~/Desktop/各品項爬蟲資料/篩紅爬蟲.xlsx")

#保養面膜  唇膏爬蟲 睫毛爬蟲 篩紅爬蟲 臉部防曬

#讀進來的資料：

str(篩紅爬蟲)
kable(篩紅爬蟲[1:5,])

#類別變數：轉換為1和0
#數值變數：轉換至「以最大最小值為1和0的尺度中」

篩紅爬蟲 <- 篩紅爬蟲[,8:12] %>%
  mutate(
    rate = (rate - min(rate)) / (max(rate)-min(rate)),
    popularity = (popularity - min(popularity)) / (max(popularity)-min(popularity)),
    fire = (fire - min(fire)) / (max(fire)-min(fire)),
    purchase = (purchase - min(purchase)) / (max(purchase)-min(purchase)),
    price = (price - min(price)) / (max(price)-min(price))
  ) %>% cbind(
    篩紅爬蟲[,c(1:7)]
  )

kable(篩紅爬蟲[1:5, ])

tmp <- cbind(
  篩紅爬蟲[, -c(6,8:12)]
)
篩紅爬蟲 <- cbind(
  篩紅爬蟲[, -c(6:12)]
)

library(reshape2)

CorMatrix <- 篩紅爬蟲 %>% cor() %>% melt()

kable(CorMatrix[ 1:5,])

#ggplot2套件完成熱密度圖

ggplot( data = CorMatrix) +
  
  geom_tile(aes(Var1, Var2,fill = value), colour = "white") + 
  
  scale_fill_gradient2(low = "firebrick4", high = "steelblue") +
  
  guides(fill=guide_legend(title="Correlation")) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
#階層式集群分析作為資料探索的方法，利用dist()函數計算距離後，再用hclust()函數進行分析及繪圖。

set.seed(500)
Distance <- dist(篩紅爬蟲, method = 'euclidean')

hclust(Distance, method = 'complete') %>% plot()

#建模將以這3、4兩種群數分別討論。

#1. 分為三群
#用kmeans()演算法，決定分群數目後、進行分群後
# Implement Kmeans Algorithm:

set.seed(500) # remove the random effect
K <- kmeans(篩紅爬蟲,3)

ClusterResult <- cbind(
  篩紅爬蟲,
  K$cluster
) %>% as.data.frame()

colnames(ClusterResult)[ncol(ClusterResult)] <- 'Cluster'

table(ClusterResult$Cluster)

#納入主成份分析
library(ggfortify)
set.seed(500)
autoplot(kmeans(篩紅爬蟲[,1:5], 3), data  = tmp) + 
  geom_text(aes(label=name),hjust=0, vjust=0)+ 
  theme(text = element_text(family = "Noto Sans CJK TC"))

#_________________________

臉部防曬 <- read_excel("~/Desktop/各品項爬蟲資料/臉部防曬.xlsx")

#保養面膜  唇膏爬蟲 睫毛爬蟲 篩紅爬蟲 臉部防曬

#讀進來的資料：

str(臉部防曬)
kable(臉部防曬[1:5,])

#類別變數：轉換為1和0
#數值變數：轉換至「以最大最小值為1和0的尺度中」

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

臉部防曬 <- cbind(
  臉部防曬[, -c(6:12)]
)
kable(臉部防曬[1:5,])

library(reshape2)

CorMatrix <- 臉部防曬 %>% cor() %>% melt()

kable(CorMatrix[ 1:5,])

#ggplot2套件完成熱密度圖

ggplot( data = CorMatrix) +
  
  geom_tile(aes(Var1, Var2,fill = value), colour = "white") + 
  
  scale_fill_gradient2(low = "firebrick4", high = "steelblue") +
  
  guides(fill=guide_legend(title="Correlation")) +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())
#階層式集群分析作為資料探索的方法，利用dist()函數計算距離後，再用hclust()函數進行分析及繪圖。

set.seed(500)
Distance <- dist(臉部防曬, method = 'euclidean')

hclust(Distance, method = 'complete') %>% plot()

#建模將以這3、4兩種群數分別討論。

#1. 分為三群
#用kmeans()演算法，決定分群數目後、進行分群後
# Implement Kmeans Algorithm:

set.seed(500) # remove the random effect
K <- kmeans(臉部防曬,4)

ClusterResult <- cbind(
  臉部防曬,
  K$cluster
) %>% as.data.frame()

colnames(ClusterResult)[ncol(ClusterResult)] <- 'Cluster'

table(ClusterResult$Cluster)

#納入主成份分析
library(ggfortify)
set.seed(500)
autoplot(kmeans(臉部防曬[,1:5], 4), data  = tmp) + 
  geom_text(aes(label=name),hjust=0, vjust=0)+ 
  theme(text = element_text(family = "Noto Sans CJK TC"))



