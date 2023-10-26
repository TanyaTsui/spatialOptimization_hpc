library(parallel)
nCores <- detectCores()
print(nCores)

# define function to test whether an number is prime
is_prime <- function(num, var2) {
  num <- num + var2
  # if input equals 2 or 3, then we know it's prime
  if(num == 2 | num == 3) 
    return(TRUE)
  # if input equals 1, then we know it's not prime
  if(num == 1) 
    return(FALSE)
  
  # else if num is greater than 2
  # and divisible by 2, then can't be even
  if(num %% 2 == 0) 
    return(FALSE)
  
  # else use algorithm to figure out
  # what factors, if any, input has
  
  # get square root of num, rounded down
  root <- floor(sqrt(num))
  
  # try to divide each odd number up to root
  # into num; if any leave a remainder of zero,
  # then we know num is not prime
  for(elt in seq(5,root))
  {
    if (num %% elt == 0)
      return(FALSE)
    
  }
  # otherwise, num has no divisors except 1 and itself
  # thus, num must be prime
  return(TRUE)
  
}

# get random sample of 1 million integers from integers between 1 and 
# 10 million
# set seed so the random sample will be the same every time
set.seed(2)
sample_numbers <- sample(1000000, 1000000)
var2 <- 2

system.time(results <- lapply(sample_numbers, is_prime))

# create cluster object
cl <- makeCluster(8)
system.time(results_p <- parSapply(cl, sample_numbers, is_prime, var2=var2))
print(results_p)
stopCluster(cl)
