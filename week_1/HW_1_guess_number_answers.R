# 猜數字遊戲
# 基本功能
# 1. 請寫一個由"電腦隨機產生"不同數字的四位數(1A2B遊戲)
# 2. 玩家可"重覆"猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

# 額外功能：每次玩家輸入完四個數字後，檢查玩家的輸入是否正確(錯誤檢查)
nums <- sample(x = c(1:9, size = 4)
guess=readline('請輸入四位數：')
g <- as.numeric(guess)


#create a guessCount to store the total counts
guess.count <- 0

#use repeat to right the method
repeat{
  cat("請輸入一組四位數字","\n")
  guess <- scan(nmax = 4)
  
  a <- b <- 0
  
  if(!any(duplicated(guess))){
    #add one time of guess
    guess.count <- guess.count + 1
    
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
  }
  cat("Your guess :", guess, ", Match : ", a, "A", b, "B\n")
  
  if(a == 4){
    cat("CORRECT! You guess for", guess.count, "times")
    break
  }else
    cat("Input Error: Please input 4 <non-repetitive> numbers.\n")
}    




'
answer <- sample(1000:9999, 1)
time <- 0
repeat{
guess <- cat("請輸入一組四位數字","\n")
enter <- scan()
a <- 0
b <- 0
for(i in 1:4){
if(enter[i] == answer[i]){
a <- a + 1
}else
for(j in 1:4){
if(enter[j] == answer[i] ){
b <- b + 1
}
}
}
cat(a,"A",b,"B","\n")
if(a == 4){print("猜對")}
break
}'
'
check <- function(num)
{
  num <- cat("請輸入一組四位數","\n")
  time <- time + 1
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
  answer <- sample(1000:9999, 1)
  while(TRUE){
  guess <- cat("請輸入一組四位數","\n")
  a = check(guess)
  b = check(guess)
  if(a == 4){
  print(paste("猜對了，答案是: ", answer, "共猜了 ", time))
  } else{
  print(paste("第", time, "猜數字, ", a, "A", b, "B" ))
  }
  break
  }
  '