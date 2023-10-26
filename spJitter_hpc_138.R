library(spsann)
library(dplyr)
library(reticulate)


use_condaenv('C:/Users/tpytsui/Miniconda/envs/geo_env')
source_python('costEffectiveness_v2.py')
costEffectiveness <- function(points, reverse) {
  testValue <- calcTotCostEffectiveness_r(points, reverse)
  return(testValue)
}

candi <- read.csv('data/candiHubs_ams.csv')
candi <- candi[,c("x", "y")]

calcParams <- function(candi) {
  # calculate params
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

paramsList <- calcParams(candi)
schedule <- scheduleSPSANN(initial.temperature = 0.0001, cellsize = 0,
			   chains = 1, initial.acceptance = 0.1, temperature.decrease = 0.1,  
                           x.max = paramsList[2], y.max = paramsList[4])
res <- optimUSER(
  points = 2, fun = costEffectiveness, reverse = TRUE, 
  schedule = schedule, candi = candi, 
  plotit = FALSE, track = TRUE
)
resPoints <- res$points
resPoints <- resPoints[order(resPoints$id), ]
print(resPoints)



