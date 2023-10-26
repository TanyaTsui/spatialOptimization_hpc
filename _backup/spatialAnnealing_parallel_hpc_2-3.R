library(parallel)

sa_parallel <- function(nHubs) {
    library(spsann)
    library(dplyr)
    library(reticulate)

    # ---------- import costEffectiveness.py ----------
    use_python('/apps/arch/2022r2/software/linux-rhel8-skylake_avx512/gcc-8.5.0/python-3.8.12-p6aunbmaoqlflowbsjqkzzm7n62qyrch/bin/python')
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
    
    # ---------- execute the simulated annealing algorithm ----------
    tempRow <- initialTemps[initialTemps$nHubs == nHubs, ]
    initialTemp <- tempRow$trendline 
    schedule <- scheduleSPSANN(initial.temperature = initialTemp) 
    
    res <- optimUSER(
    points = nHubs, fun = testFunction, 
    schedule = schedule, candi = candi, plotit = FALSE
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
    
    # ---------- return messages ----------
    print(paste0('calculating results for ', nHubs, ' hubs...'))
    return(messages)
}

nCores <- detectCores()
cl <- makeCluster(2)
print(paste0('made cluster with ', nCores, ' cores'))
messages <- parSapply(cl, 2:3, sa_parallel)
print(messages)
stopCluster(cl)