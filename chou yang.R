
time <- choose(N, n)#N��������n������
sampleExp <- function(x) sample(x, n, replace = TRUE) #�зŻصĳ���,x���� 
sampleExpR <- function(x) sample(x, n, replace = FALSE)#�޷Żصĳ�����x����
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

