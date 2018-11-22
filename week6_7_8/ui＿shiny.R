# LISTS

attribute_list = c('保養面膜', '臉部防曬', '唇膏', '睫毛', '篩紅')


##UI =======================================================================
ui <- navbarPage(
  
  theme = shinythemes::shinytheme("flatly"),
  
  # Application title
  "美妝品tag分析",
  
  tabPanel(
    "簡介",
    tags$h2("簡介"),br(),
    tags$h3("作者： 廖宣啊"),br(),br(),
    tags$h4("市面上美妝品百百種，每個產品也都有各自的特色；"),br(),
    tags$h4("本次專題透過urcosme最大的美妝評論網站，進行熱門產品特點的資料分析："),br(),
    tags$h4("找出熱門產品的【關鍵字】，並進行分群"),br(),
    tags$h4("【價錢與熱門度】、【評價分數與熱門度】、【評價分數與價錢】的分佈，"),br(),
    tags$h4("及【品牌與產品關鍵字的關係】。"),br()
  ),
  tabPanel(
    "文字雲",
    "各品項熱門商品標籤的文字雲",
    tags$h2("熱門商品標籤統計"),br(),
    sidebarPanel(
      selectInput("attribute_1", "品項:", 
                  choices=attribute_list),
      hr(),
      helpText("列出這品項熱門商品標籤文字雲")
      
    ),
    mainPanel(
      plotOutput("WordCloud_1")
    )
  ),
  
  #_______________________________________---
  tabPanel(
    "熱門商品標籤統計",
    tags$h2("熱門商品標籤統計"),br(),
    sidebarPanel(
      selectInput("attribute_1", "品項:", 
                  choices=attribute_list),
      hr(),
      helpText("列出這品項熱門商品的標籤統計")
      
    ),
    mainPanel(
      plotOutput("Plot_1")
    )
  ),
  
  tabPanel(
    "商品市場屬性的相關性",
    tags$h1("商品市場屬性的相關性"),
    sidebarLayout(
      sidebarPanel(
        tags$h4("使用PCA及K-means分群，我們可以將市場屬性類似的商品分群"),
        radioButtons("radio", label = "Choices", choices = list("保養面膜" = 1, "臉部防曬" = 2, "唇膏" = 3,"睫毛" = 4, "篩紅" = 5)),
        numericInput("k1","Number of k:", min = 1, max = 5,value = 3)
      ),
    # Show a plot of the generated distribution

      mainPanel(
      plotlyOutput("Plotly_KM1")
    )
  )),
  
  
  tabPanel (
    "商品標籤特色的相關性",
    tags$h1("商品標籤特色的相關性"),
    sidebarLayout(
      sidebarPanel(
        tags$h4("使用PCA及K-means分群，我們可以將標籤特色類似的商品分群"),
        radioButtons("radio", label = "Choices", choices = list("保養面膜" = 1, "臉部防曬" = 2, "唇膏" = 3,"睫毛" = 4, "篩紅" = 5)),
        numericInput("k2","Number of k:", min = 1, max = 5,value = 3)
      ),
      # Show a plot of the generated distribution
      
      mainPanel(
        plotlyOutput("Plotly_KM2")
      )
    )))
  
##SERVER =====================================================================
server <- function(input, output) {
  output$Plot_1 <- renderPlot({
    input.attribute <- input$attribute_1
    words_count_attribute = TDM_attribute[,c('d', input.attribute)]
    colnames(words_count_attribute) = c('word', 'count')
    words_count_attribute = words_count_attribute[rev(order(words_count_attribute$count)),]
    
    rownames(words_count_attribute)=NULL

      ggplot(words_count_attribute[1:20,], aes(x = reorder(word, count), y =count)) + 
      geom_bar(stat = "identity", fill='lightblue') + 
      coord_flip() +
      labs(x='word', y='count', title=paste('Author: ', input.author)) +
      theme(panel.background = element_blank(),
            axis.title = element_text(color = '#2d2d2d'),
            axis.text.x = element_text(hjust = 1, size=15),
            axis.text.y = element_text(hjust = 1, size=15),
            strip.text.x = element_text(color='#2d2d2d',face='bold',size=10),
            plot.title = element_text(hjust=0.5,face='bold',size=15))
  })
  output$WordCloud_1 <- renderPlot({
    wordcloud(docs.df_author$word, docs.df_author$freq, scale=c(5,0.8),max.words=input$wc_max,
              random.order=FALSE, random.color=TRUE, 
              rot.per=.1, colors=brewer.pal(8,"Dark2"),
              ordered.colors=FALSE,use.r.layout=FALSE,
              fixed.asp=TRUE)
  })
 #___________________________________________________
  
   output$Plot_2 <- renderPlot({
    input.tag = input$tag_1
    
    words_count_tag = TDM_tag[,c('d', input.tag)]
    colnames(words_count_tag) = c('word', 'count')
    words_count_tag = words_count_tag[rev(order(words_count_tag$count)),]
    rownames(words_count_tag)=NULL
    
    ggplot(words_count_tag[1:20,], aes(x = reorder(word, count), y =count)) + 
      geom_bar(stat = "identity", fill='lightblue') + 
      coord_flip()+
      labs(x='word', y='count', title=paste('此類別的常用字 Tag: ', input.tag)) +
      theme(panel.background = element_blank(),
            axis.title = element_text(color = '#2d2d2d'),
            axis.text.x = element_text(hjust = 1, size=15),
            axis.text.y = element_text(hjust = 1, size=15),
            strip.text.x = element_text(color='#2d2d2d',face='bold',size=10),
            plot.title = element_text(hjust=0.5,face='bold',size=15))
  })
  output$Plot_3 <- renderPlot({
    input.tag = input$tag_1
    tag_author_count <- dta %>% filter(str_detect(tag, input.tag)) %>% select(author) %>% 
      unlist() %>% count
    
    ggplot(tag_author_count, aes(x = reorder(x, freq), y = freq)) + 
      geom_bar(stat = "identity", fill='lightblue') + 
      coord_flip()+
      labs(x='作者',title=paste('此類別的詩人及詩作數量 Tag: ',input.tag)) + 
      theme(panel.background = element_blank(),
            axis.title = element_text(color = '#2d2d2d'),
            axis.text.x = element_text(hjust = 1, size=15),
            axis.text.y = element_text(hjust = 1, size=15),
            strip.text.x = element_text(color='#2d2d2d',face='bold',size=10),
            plot.title = element_text(hjust=0.5,face='bold',size=15))
  })
  output$Plot_4 <- renderPlot({
    ggplot(tag_count, aes(x = reorder(x, freq), y = freq)) + 
      geom_bar(stat = "identity", fill='lightblue') + 
      coord_flip() + 
      labs(x='tag',title='各類別唐詩數量統計') + 
      theme(panel.background = element_blank(),
            axis.title = element_text(color = '#2d2d2d'),
            axis.text.x = element_text(hjust = 1, size=15),
            axis.text.y = element_text(hjust = 1, size=15),
            strip.text.x = element_text(color='#2d2d2d',face='bold',size=10),
            plot.title = element_text(hjust=0.5,face='bold',size=15))
  })
  output$Plotly_KM1 <- renderPlotly({
    k_author = input$k1
    
    cl_author <- kmeans(kmeansData_author, k_author)
    kmeansData_author <- as.data.frame(kmeansData_author) 
    kmeansData_author$cl <- as.factor(cl_author$cluster)
    
    plot_ly(kmeansData_author, x= ~PC1, y=~PC2, type='scatter',
            mode='text', text=paste0("<b>",rownames(kmeansData_author),"</b>"), 
            color = ~cl, colors="Set1", textfont = list(size = 14) )
  })
  output$Plotly_KM2 <- renderPlotly({
    k_tag = input$k2
    
    kmeansData_tag = pcat_tag$x[,1:2]
    # kmeansData = kmeansData[kmeansData[,1] > -0.05, ]
    
    cl_tag <- kmeans(kmeansData_tag, k_tag)
    kmeansData_tag <- as.data.frame(kmeansData_tag) 
    kmeansData_tag$cl <- as.factor(cl_tag$cluster)
    
    plot_ly(kmeansData_tag, x= ~PC1, y=~PC2, type='scatter',
            mode='text', text=paste0("<b>",rownames(kmeansData_tag),"</b>"), 
            color = ~cl, colors="Set1", textfont = list(size = 14) )
  })
  
}
fviz_eig(pcat_author)
fviz_pca_ind(pcat_author, geom= c("point","text","arrow"), col.ind = "cos2")
fviz_pca_var(pcat_author, col.var = "contrib")
# Run the application 
shinyApp(ui = ui, server = server)
