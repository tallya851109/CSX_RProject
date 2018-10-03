library(ggplot2)
#匯入資料
iris
#理解資料
View(iris)
i=iris
#型態轉換
temp=matrix(unlist(iris),nrow=150,byrow=T)
summary(temp)
#畫圖
ggplot(data=i,aes( x=Species))+
  geom_bar(fill="lightblue",color="black")


#-----------Choropleth map面量圖

library(choroplethr)
data(df_pop_state) #記載各州人口數的資料
state_choropleth(df_pop_state) #把各州人口畫在地圖上
data(continental_us_states)
state_choropleth(df_pop_state,reference_map = TRUE,
                 zoom= continental_us_states) #把各州人口畫在地圖上

#-----------地圖、點圖
#install.packages("ggmap", type = "source")

library(ggmap)
twmap <- get_map(location = 'Taiwan', zoom = 7,language = "zh-TW")
ggmap(twmap)+ #ggmap
  geom_polygon(data = final.plot,  #面量圖
               aes(x = long, y = lat, group = group, fill = prevalence), 
               color = "grey80", size = 0.1,alpha = 0.5) + 
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))
##資料載入
library(jsonlite)
library(RCurl)
WaterData<-fromJSON(getURL("http://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=190796c8-7c56-42e0-8068-39242b8ec927"))
WaterDataFrame<-WaterData$result$results
WaterDataFrame$longitude<-as.numeric(WaterDataFrame$longitude)
WaterDataFrame$latitude<-as.numeric(WaterDataFrame$latitude)
WaterDataFrame$qua_cntu<-as.numeric(WaterDataFrame$qua_cntu)
##結合ggmap
library(ggmap)
TaipeiMap = get_map(location = c(121.43,24.93,121.62,25.19), 
                    zoom = 11, maptype = 'roadmap')
TaipeiMapO = ggmap(TaipeiMap)+ 
  geom_point(data=WaterDataFrame[WaterDataFrame$qua_cntu>=0,], 
             aes(x=longitude, y=latitude,color=qua_cntu,size=3.5))+ 
  scale_color_continuous(low = "yellow",high = "red")+ 
  guides(size=FALSE)
TaipeiMapO

#-----------直方圖
my.plot <- ggplot(diamonds, aes(carat, price, colour = cut))
my.plot <- my.plot + layer(
  geom = "point",
  stat = "identity",
  position = "identity",
  params = list(na.rm = FALSE)
)

my.plot2 <- ggplot(diamonds, aes(x = carat))
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

