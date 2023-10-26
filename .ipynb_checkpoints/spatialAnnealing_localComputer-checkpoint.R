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
nHubs <- 2
# tempRow <- initialTemps[initialTemps$nHubs == nHubs, ]
initialTemp <- 0.00001 # tempRow$trendline
schedule <- scheduleSPSANN(initial.temperature = initialTemp)

print(paste0('calculating results for ', nHubs, ' hubs...'))
res <- optimUSER(
points = nHubs, fun = costEffectiveness, reverse = TRUE, 
schedule = schedule, candi = candi, plotit = TRUE
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