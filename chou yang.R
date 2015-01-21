
time <- choose(N, n)#N总体数，n抽样数
sampleExp <- function(x) sample(x, n, replace = TRUE) #有放回的抽样,x样本 
sampleExpR <- function(x) sample(x, n, replace = FALSE)#无放回的抽样，x样本
sim.choose <- function(x, time, replace){
  sum <- c()
  for(i in 1:time)
  {
    result <- c()
    if(replace == TRUE)
      result <- sampleExp(x)
    else
      result <- sampleExpR(x)
    result
    sum[(4*i - 1): (2*i)] <- result
  }
 sum <- matrix(sum, 4)
 print(sum)
 sum
}


