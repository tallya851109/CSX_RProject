########################################################### 
# 加分作業：
# 當要完成的目標變複雜後，學習如何將複雜的問題拆解成一個一個小問題來解決
# 練習 R function 的使用

# OOXX 遊戲練習
# 1. 設計一個兩人的OOXX遊戲。
# 2. 遊戲玩家分為A、B。A 先手，使用的符號為'O'; B 後手，使用的符號為'X'
# 3. 遊戲一開始，請輸出以下遊戲提示，並且停留等待玩家A輸入

#    Round 0
#    Now is player A's term!
#    Player A input(1~9) : 

# 4. 玩家們可以輸入的數字範圍為 1~9，依序對應九宮格的九格位置。
#    如果輸入錯誤，請抓錯！輸出以下遊戲提示。

#    Invalid input! Please re-enter! 
#    Round 0
#    Now is player A's term!
#    Player A input(1~9) : 

# 5. 待玩家正確輸入完後，請輸出以下遊戲提示(當時的遊戲圖形狀況)，並且等待切換到另外一位玩家等待輸入。
#    * 提醒，記得增加'Round'次數，以及切換使用者

#    O| | 
#    _____
#     | | 
#    _____
#     | | 
#    **************
#    Round 1
#    Now is player B's term!
#    Player B input(1~9) : 

# 6. 當玩家輸入的位置之前已經有'O'或'X'時，請輸出以下遊戲提示。

#    This position is already occupied!
#    Round 1
#    Now is player B's term!
#    Player B input(1~9) : 

# 7. 當使用者輸入'exit'時，結束遊戲並印出以下遊戲提示 

#    Bye-Bye!!

# 8. 判斷遊戲結束！當三個直排、橫排、或者斜排時，請輸出以下遊戲提示(當時的遊戲圖形狀況)，並且輸出勝利的玩家。

#    O|X|O
#    _____
#    X|O|X
#    _____
#    O| | 
#    **************
#    Player A wins!!! 
#

# 9. 當空格皆被填滿且無玩家獲勝時，請輸出以下遊戲提示(當時的遊戲圖形狀況)以及和局遊戲提示。

#   O|O|X
#   _____
#   X|X|O
#   _____
#   O|X|O
#   **************
#   End in a draw!!! 

player<- function(x){
  if(x%%2==0)return("A")
  else return("B")
}
trans <- function(x){
  if(x=="A"||x=="O") return("O")
  else if (x=="B"||x=="X") return("X")
  else return(" ")
}

judge <- function(x){
  if(x=="O") return(1)
  else if (x=="X") return(-1)
  else return(0)
}

i <- 0
com <-""
TTT <- as.character(c(1:9))

while(i != 10){
  error<- 0
  #check input
  while(error!= 1){
    cat("Round",i,"\n")
    
    cat("Now is player ",player(i),"'s turn!\n",sep="")
    
    com <- readline(paste("Player",player(i),"input(1~9) :"))
    
    if (com == "exit"){
      print("Bye-Bye!!")
      break
    } 
    com <- as.integer(com)
    if(com>=1&&com<=9)  {
      error<- 1
      if(TTT[com]=="O"||TTT[com]=="X"){
        error<- 0
        cat("This position is already occupied!\n")
      }
      
    }
    else {
      error<- 0
      cat("Invalid input! Please re-enter!\n")
    }
  }
  
  #exiting
  if (com == "exit"){
    break
  } 
  
  #implement O/X
  TTT[com] <- player(i)
  meow <- 0
  for(j in c(1:9)) {
    TTT[j] <- trans(TTT[j])
  }
  
  cat(TTT[1],"|",TTT[2],"|",TTT[3],"\n_____\n",TTT[4],"|",TTT[5],"|",TTT[6],"\n_____\n",TTT[7],"|",TTT[8],"|",TTT[9],"\n**************\n",sep="")
  
  #judge win
  for(a in c(0:2)){
    p <- 0
    q <- 0
    for (b in c(1:3)) {
      p<- p+judge(TTT[3*a+b])
      q<- q+judge(TTT[3*b-a])
    }
  }
  if(p==3||q==3){
    cat("Player",player(i),"wins!\n")
    break
  }
  else if(p==-3||q==-3){
    cat("Player",player(i),"wins!\n")
    break
  }
  else if(TTT[5]==TTT[1]&&TTT[5]==TTT[9]&&TTT[5]!=" "){
    cat("Player",player(i),"wins!\n")
    break
  }
  else if(TTT[5]==TTT[3]&&TTT[5]==TTT[7]&&TTT[5]!=" "){
    cat("Player",player(i),"wins!\n")
    break
  }  
  else if (i==8){
    cat("End in a draw!!!\n")
    break
  }
  i<-i+1
}