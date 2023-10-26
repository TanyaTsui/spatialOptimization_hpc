library(spsann)
library(dplyr)
library(reticulate)

# load cost effectiveness script
use_python('/apps/arch/2022r2/software/linux-rhel8-skylake_avx512/gcc-8.5.0/python-3.8.12-p6aunbmaoqlflowbsjqkzzm7n62qyrch/bin/python')
source_python('costEffectiveness_v2.py')
costEffectiveness <- function(points, reverse) {
  testValue <- calcTotCostEffectiveness_r(points, reverse)
  return(testValue)
}

# read data for candidate hubs 
candi <- read.csv('data/candiHubs_ams.csv')
candi <- candi[,c("x", "y")]

# test for a few hub numbers  
for (nHubs in c(25, 75, 90, 138)) {
  
  # adjust nHubs 
  if (nHubs < 80) {
    print(paste0('calculating results for ', nHubs, ' hubs...'))
    nHubs_adjusted = nHubs # no adjustment because nHubs is low 
    reverse <- FALSE
  } else {
    print(paste0('calculating results for ', nHubs, ' hubs...'))
    print('nHubs > 100, running reverse cost effectiveness calculations')
    nHubs_adjusted = 139 - nHubs 
    reverse <- TRUE
  }
  
  # calculate schedule parameters
  calcParams <- function(candi) {
    xCoords <- sort(candi$x)
    xDists <- sort(diff(xCoords))
    xMin <- xDists[1]
    xMax <- tail(xCoords, 1) - xCoords[1]
    yCoords <- sort(candi$y)
    yDists <- sort(diff(yCoords))
    yMin <- yDists[1]
    yMax <- tail(yCoords, 1) - yCoords[1]
    return(c(xMin, xMax, yMin, yMax))
  }
  
  # run ssaa 
  paramsList <- calcParams(candi)
  schedule <- scheduleSPSANN(initial.temperature = 0.00001, cellsize = 0, 
                             chains = 1, temperature.decrease = 0.1, initial.acceptance = 0.1, 
                             x.max = paramsList[2], y.max = paramsList[4])
  res <- optimUSER(
    points = nHubs_adjusted, fun = costEffectiveness, reverse = reverse, 
    schedule = schedule, candi = candi, 
    plotit = FALSE, track = TRUE
  )
  
  # write results 
  resPoints <- res$points
  resPoints <- resPoints[order(resPoints$id), ]
  resPoints$nHubs <- nHubs
  write.table(
    resPoints, file='results/resPoints_spJitterTest_230331.csv', append=TRUE, sep=',', 
    col.names = FALSE
  )
}


