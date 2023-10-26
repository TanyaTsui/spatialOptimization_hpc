library(spsann)
library(dplyr)
library(reticulate)

# ---------- import costEffectiveness.py ----------
print('setup python with reticulate')
# for supercomputer
# use_python('/apps/arch/2022r2/software/linux-rhel8-skylake_avx512/gcc-8.5.0/python-3.8.12-p6aunbmaoqlflowbsjqkzzm7n62qyrch/bin/python')
# for laptop
use_condaenv('C:/Users/tpytsui/Miniconda/envs/geo_env')
source_python('costEffectiveness_v2.py')
testFunction <- function(points) {
  testValue <- calcTotCostEffectiveness_r(points)
  return(testValue)
}

# ---------- Read data ----------
candi <- read.csv('data/candiHubs_ams.csv')
candi <- candi[,c("x", "y")]

initialTemps <- read.csv('data/initialTemps.csv', sep = ";")
initialTemps <- initialTemps[1:2]
colnames(initialTemps) <- c('nHubs', 'trendline')


# ---------- run spatial annealing ----------
nHubs <- 2
print(paste0('calculating results for ', nHubs, ' hubs...'))
tempRow <- initialTemps[initialTemps$nHubs == nHubs, ]
initialTemp <- tempRow$trendline # trendline, slow, or extraSlow

# make annealing schedule (determine initial temp)
# initialTemp <- 0.0001
schedule <- scheduleSPSANN(initial.temperature = initialTemp) 

# execute the simulated annealing algorithm 
res <- optimUSER(
  points = nHubs, fun = testFunction, 
  schedule = schedule, candi = candi, plotit = TRUE
) 

# save resEnergy
resEnergy <- res$objective$energy
print(resEnergy)
# resEnergy$nHubs <- nHubs
# resEnergy$time <- Sys.time()
# write.table(
#   resEnergy, file='results/resEnergy_test.csv', append=TRUE, sep=',', 
#   col.names = FALSE
# )
# 
# # save resPoints
# resPoints <- res$points
# resPoints$nHubs <- nHubs
# resPoints$time <- Sys.time()
# resPoints <- resPoints[, c('id', 'x', 'y', 'nHubs', 'time')]
# write.table(
#   resPoints, file='results/resPoints.csv', append=TRUE, sep=',', 
#   col.names = FALSE
# )

