library(parallel)
library(spsann)
library(dplyr)
library(reticulate)

# ---------- import costEffectiveness.py ----------
use_condaenv('C:/Users/tpytsui/Miniconda/envs/geo_env')
source_python('costEffectiveness_v2.py')
costEffectiveness <- function(points, reverse) {
  testValue <- calcTotCostEffectiveness_r(points, reverse)
  return(testValue)
}

# ---------- Read data ----------
candi <- read.csv('data/candiHubs_ams.csv')
candi <- candi[,c("x", "y")]

initialTemps <- read.csv('data/initialTemps.csv', sep = ";")
initialTemps <- initialTemps[1:2]
colnames(initialTemps) <- c('nHubs', 'trendline')

# ---------- execute the simulated annealing algorithm ----------
nHubs <- 130

if (nHubs < 80) {
  print(paste0('calculating results for ', nHubs, ' hubs...'))
  nHubs_adjusted = nHubs # no adjustment because nHubs is low 
  reverse <- FALSE
} else {
  print(paste0('calculating results for ', nHubs, ' hubs...'))
  print('nHubs > 100, running reverse cost effectiveness calculations')
  nHubs_adjusted = 138 - nHubs 
  reverse <- TRUE
}

tempRow <- initialTemps[initialTemps$nHubs == nHubs, ]
initialTemp <- tempRow$trendline
schedule <- scheduleSPSANN(initial.temperature = initialTemp)

res <- optimUSER(
points = nHubs_adjusted, fun = costEffectiveness, reverse = reverse, 
schedule = schedule, candi = candi, plotit = FALSE, track = TRUE
)

# ---------- save results ----------
resPoints <- res$points
resPoints$nHubs <- nHubs
resPoints$time <- Sys.time()
resPoints <- resPoints[, c('id', 'x', 'y', 'nHubs', 'time')]
write.table(
resPoints, file='results/resPoints_parallel_2-3.csv', append=TRUE, sep=',', 
col.names = FALSE
)

resEnergy <- res$objective$energy
resEnergy$nHubs <- nHubs
resEnergy$time <- Sys.time()
write.table(
resEnergy, file='results/resEnergy_parallel_2-3.csv', append=TRUE, sep=',', 
col.names = FALSE
)