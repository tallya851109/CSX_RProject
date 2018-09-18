<<筆記>>

<- 稱為賦值(按 alt 與 - 幫我們生成 <- 符號)

邏輯判斷：>、<、==、!=、%in%（包含於）、&、|、!

數學運算：+ ,-	 ,*	,/  ,^ 或 **	次方 ,%%	回傳餘數

條件指令：if、else、ifelse、switch

迴圈指令：for、while、repeat、break、next

#語法格式
  if('條件'){
   '做A'
  }else{
 '做B'
  }
  
  ifelse('條件', '條件若成立：做A', '條件若不成立：做B')

使用c()所指定物件，為一個向量(vector)，即為一維的陣列(array)

length() 計算一個向量的長度（元素個數）

sample() 隨機抽樣函式

a <- matrix(c(1:6), nrow=3, ncol=2) #建立一個3x2的矩陣，依照column分別填入1~6的值

資料框(data frame)

tmp <- data.frame(Student_ID=c(1,2,3,4,5),
                  name=c("Helen", "Lun", "Leon", "Kevin", "Tommy"),
                  score=c(80,36, 88.9, 97.5, 60))
tmp[4,3]
