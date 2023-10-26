
# -------------- calculate initial temperature -----------

calcInitialTemp <- function(nHubs) {
  library(parallel)
  library(spsann)
  library(dplyr)
  library(reticulate)
  
  # ------ Read data (pre-processed in Python) ------
  # candidate sites
  print('read data')
  candi <- read.csv('data/candiHubs_ams.csv')
  candi <- candi[,c("x","y")]
  
  # first guesses for optimization algorithm 
  firstGuesses <- read.csv('data/firstGuesses_ams.csv')
  strToVector <- function(x) {return(as.numeric(strsplit(x,',')[[1]]))}
  firstGuesses$candiIndexes <- lapply(firstGuesses$candiIndexes, strToVector)
  
  use_condaenv('C:/Users/tpytsui/Miniconda/envs/geo_env')
  source_python('costEffectiveness_v2.py')
  testFunction <- function(points) {
    testValue <- calcTotCostEffectiveness_r(points)
    res <- testValue
  }
  
  # select first guess
  candiIndex <- unlist(firstGuesses[nHubs, "candiIndexes"])
  startingPoints <- candi[candiIndex, ]
  startingPoints$id <- as.numeric(rownames(startingPoints))
  startingPoints <- relocate(startingPoints, id)
  
  # make annealing schedule (determine initial temp)
  schedule <- scheduleSPSANN(
    initial.temperature = 0.0001, chains = 1
  )

  # execute the simulated annealing algorithm 
  res <- optimUSER(
    points = nHubs, fun = testFunction, 
    schedule = schedule, candi = candi, 
    plotit = FALSE
  ) 
  
  # record results
  resEnergy <- res$objective$energy
  startEnergy <- resEnergy[1,1]
  nRes <- nrow(resEnergy)
  finalEnergy <- resEnergy[nRes, 1]
  
  # m1 - count number of results that are better than the starting result
  betterRes <- resEnergy[resEnergy$obj < startEnergy,]
  m1 <- length(betterRes)
  
  # m2 - number of other proposed transitions 
  m2 <- nRes - m1
  
  # deltaf(+) - av cost difference over improving iterations
  costDiffs <- list() 
  count = 0
  for (val in betterRes) {
    count = count + 1
    if (count == 1) costDiff = startEnergy - val
    if (count > 1) costDiff = betterRes[count-1] - val
    costDiffs <- append(costDiffs, costDiff)
  }
  deltaf <- Reduce('+', costDiffs) / length(betterRes)
  
  # acceptance rate - rate of accepted transitions 
  acceptanceRate <- 0.95 # default rate for spsann 
  
  # initial temperature
  initialTemp <- deltaf / log(m2/(m2*acceptanceRate - m1*(1-acceptanceRate)))
  # note: log() is natural log, NOT base 10 log. 
  # Why? because R likes being confusing.
  
  return(initialTemp)
}


cl <- makeCluster(3)
system.time(test_p <- parSapply(cl, 2:4, calcInitialTemp))
print(test_p)
stopCluster(cl)

error <- try(prrint('hello'))

# test_p <- mclapply(2:4, calcInitialTemp) # does not work for windows, just serial 
# print(test_p)
