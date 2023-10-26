library(spsann)
library(dplyr)
library(reticulate)

# load cost effectiveness script
use_condaenv('C:/Users/tpytsui/Miniconda/envs/geo_env')
source_python('costEffectiveness_v2.py')
costEffectiveness <- function(points, reverse) {
  testValue <- calcTotCostEffectiveness_r(points, reverse)
  return(testValue)
}

# read data for candidate hubs 
candi <- read.csv('data/candiHubs_ams.csv')
candi <- candi[,c("x", "y")]

# define nHubs 
nHubs <- 2

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
  points = 1, fun = costEffectiveness, reverse = reverse, 
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


# testing results 
resAll <- read.csv('results/resPoints_230331_2-3.csv', header = FALSE, 
                   col.names = c('index', 'hubName', 'x', 'y', 'nHubs', 'startTime', 'endTime'))
resAll <- resAll[,c('hubName', 'x', 'y', 'nHubs')]
for (nHubs in unique(resAll$nHubs)) {
  print(paste0('testing for ', nHubs, ' hubs'))
  temp <- resAll[resAll$nHubs == nHubs, ]
  print(length(unique(temp$hubName)))
}






# # backup
# # try on my own data
# candi <- read.csv('data/candiHubs_ams.csv')
# candi <- candi[,c("x", "y")]
# 
# # calculate params
# calcParams <- function (candi) {
#   xCoords <- sort(candi[, 2])
#   xDists <- sort(diff(xCoords))
#   xMin <- xDists[1]
#   xMax <- tail(xDists, 1) 
#   yCoords <- sort(candi[, 3])
#   yDists <- sort(diff(yCoords))
#   yMin <- yDists[1]
#   yMax <- tail(yDists, 1) 
#   return(c(xMin, xMax, yMin, yMax))
# }
# 
# # jitter 
# paramsList <- calcParams(candi)
# pts3 <- spJitter(points = pts2, candi = candi, which.point = 1, cellsize = 0, 
#                  x.min = paramsList[1], x.max = paramsList[2], 
#                  y.min = paramsList[1], y.max = paramsList[2])
# plot(candi[, 2:3], asp = 1, pch = 16, col = "gray")
# points(pts2[, 2:3], col = "red", cex = 1)
# points(pts3[, 2:3], pch = 19, col = "blue", cex = 1)


