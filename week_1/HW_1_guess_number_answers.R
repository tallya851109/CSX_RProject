# 猜數字遊戲
# 基本功能
# 1. 請寫一個由"電腦隨機產生"不同數字的四位數(1A2B遊戲)
# 2. 玩家可"重覆"猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

# 額外功能：每次玩家輸入完四個數字後，檢查玩家的輸入是否正確(錯誤檢查)

#電腦隨機生出四個不重複數字的四位數
ans <- sample(x = c(1:9, size = 4))
#讓玩家輸入數字
guess1=readline('請輸入四位數：')
#轉換成數字
guess <- as.numeric(guess1)
#紀錄猜次數的變數
guess.count <- 0
#如果有位置對、數字對的，就是回覆A（guess[i]是跑電腦給的數字向量，ans[i]是跑玩家給的數字向量）
#剩下數字如果有數字一樣的，就是回覆B（guess[i]是跑電腦給的數字，ans[j]是跑玩家給的數字）
a <- b <- 0

for(i in 1:4){
  if(guess[i] == ans[i]){
    a <- a + 1
  }else
    for(j in 1:4){
      if(guess[i] == ans[j]){
        b <- b + 1
      }
    }
}

cat(a,"A",b,"B","\n")
if(a == 4){print("猜對")}
break

{
  num <- readline('請輸入四位數：')
  guess.count <- guess.count + 1
  a <- 0
  b <- 0
  i <- 1
  for(n in num){
    if(n == answer[i]){
      a <- a + 1
      i <- i + 1
    } else if(n == answer){
      b <- b + 1
    }
  }
  return(a)
  return(b)
} 

#若不是在給玩家輸入，紀錄猜次數+1
  guess.count <- guess.count + 1
#不斷循環，當猜中（4A）就停止，並顯示恭喜答對，猜的次數
  if（g[c(1)]==nums[c(1)]）

print(paste("第", time, "猜數字, ", a, "A", b, "B" )
      
} else{print(paste("恭喜答對，答案是: ", answer, "共猜了 ", guess.count))
  )
}
break
}
'
