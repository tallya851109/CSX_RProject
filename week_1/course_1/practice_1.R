### BMI

# Craete variable called my.height.cm with your actual height in cm 
my.height.cm <-156
  
  # Craete variable called my.weight.cm with your actual weight in kg
  my.weight.kg <-47
  
  # Create my.height.m transfered by my.height.cm  
  my.height.m <- my.height.cm/100
  
  # Create my.bmi with BMI(Body Mass Index) formula
  my.bmi <- my.weight.kg/(my.height.m)^2
  
  # Use if-else to print matched information
  # Reference: http://www.tpech.gov.taipei/ct.asp?xItem=1794336&CtNode=30678&mp=109171

  if (my.bmi >= 35) {
    print(paste("Your bmi: ", my.bmi))
    print("重度肥胖!放下你手上食物!!")
  } else if (my.bmi >= 30) {
    print(paste("Your bmi: ", my.bmi))
    print("中度肥胖!小心變成豬!")
  }else if (my.bmi >= 27) {
    print(paste("Your bmi: ", my.bmi))
    print("輕度肥胖!不要掉以輕心，開始少吃了")
  }else if (my.bmi >= 24) {
    print(paste("Your bmi: ", my.bmi))
    print("過重，有點肉肉的")
  } else if (my.bmi >= 18.5) {
    print(paste("Your bmi: ", my.bmi))
    print("恭喜！正常喔～")
    } else{
    print(paste("Your bmi: ", my.bmi))
    print("過輕，可以盡情享受美食")
  }


  
  