library(parallel)

sa_parallel <- function(nHubs) {
    library(spsann)
    library(dplyr)
    library(reticulate)

    # ---------- import costEffectiveness.py ----------
    use_python('/apps/arch/2022r2/software/linux-rhel8-skylake_avx512/gcc-8.5.0/python-3.8.12-p6aunbmaoqlflowbsjqkzzm7n62qyrch/bin/python')
    source_python('costEffectiveness_v2.py')
    testFunction <- function(points, reverse) {
      testValue <- calcTotCostEffectiveness_r(points, reverse)
      return(testValue)
    }
    
    # ---------- Read data ----------
    candi <- read.csv('data/candiHubs_ams.csv')
    candi <- candi[,c("x", "y")]

    initialTemps <- read.csv('data/initialTemps.csv', sep = ";")
    initialTemps <- initialTemps[1:2]
    colnames(initialTemps) <- c('nHubs', 'trendline')
    
    # ---------- Adjust nHubs (initial temp, reverse) ----------
    if (nHubs < 80) {
      nHubs_adjusted = nHubs # no adjustment because nHubs is low 
      reverse <- FALSE
    } else {
      nHubs_adjusted = 138 - nHubs 
      reverse <- TRUE
    }
    
    # ---------- execute the simulated annealing algorithm ----------
    startTime <- Sys.time()
    
    tempRow <- initialTemps[initialTemps$nHubs == nHubs, ]
    initialTemp <- tempRow$trendline 
    schedule <- scheduleSPSANN(initial.temperature = initialTemp) 

    res <- optimUSER(
    points = nHubs_adjusted, fun = testFunction, reverse = reverse, 
    schedule = schedule, candi = candi, plotit = FALSE
    ) 
    
    endTime <- Sys.time()

    # ---------- save results ----------
    resPoints <- res$points
    resPoints$nHubs <- nHubs
    resPoints$startTime <- startTime
    resPoints$endTime <- endTime 
    resPoints <- resPoints[, c('id', 'x', 'y', 'nHubs', 'startTime', 'endTime')]
    write.table(
    resPoints, file='results/resPoints_230328_111-138.csv', append=TRUE, sep=',', 
    col.names = FALSE
    )

    resEnergy <- res$objective$energy
    resEnergy$nHubs <- nHubs
    resEnergy$startTime <- startTime
    resEnergy$endTime <- endTime 
    write.table(
    resEnergy, file='results/resEnergy_230328_111-138.csv', append=TRUE, sep=',', 
    col.names = FALSE
    )

}

nCores <- detectCores()
cl <- makeCluster(nCores)
system.time(test_p <- parSapply(cl, 111:138, sa_parallel))
stopCluster(cl)