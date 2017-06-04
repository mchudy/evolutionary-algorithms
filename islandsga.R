library("foreach")
source("classicga.R")

# TODO: might consider parallel execution

islandsga <- function(fitness,
                      min,
                      max,
                      populationSize = 50,
                      islandsCount = 5,
                      migrationRate = 0.05,
                      migrationInterval = 10,
                      crossoverProb = 0.8,
                      mutationProb = 0.1,
                      elitismPercentage = 0.1,
                      iterations = 1000,
                      verbose = FALSE
) {
  suggestions <- matrix(nrow = 0, ncol = length(min))
  
  islSize <- max(1, floor(populationSize / islandsCount))
  migPop <- max(1, floor(migrationRate * islSize))
  numiter <- max(1, floor(iterations / migrationInterval))
  
  GAs <- vector(mode = "list", length = islandsCount)
  POPs <- rep(list(suggestions), times = islandsCount)
  for(iter in seq_len(numiter)) {
    GAs <- foreach(i. = seq_len(islandsCount)) %do% 
      { 
      classicga(
         fitness = fitness,
         min = min, 
         max = max,
         populationSize = islSize,
         crossoverProb = crossoverProb,
         mutationProb = mutationProb,
         iterations = migrationInterval,
         convergenceIters = migrationInterval,
         initialPopulation = POPs[[i.]]
         )
      }

    for(island in seq_len(islandsCount)) {
      # get summary of GAs evolution
      j <- seq((iter-1)*migrationInterval+1, iter*migrationInterval)
      #sumryStat[[i]][j,] <- GAs[[i]]@summary
      # migration step
      from <- island
      to <- (island %% islandsCount) + 1
      ## select top individuals to migrationPop
      j <- order(GAs[[from]]$fitness, decreasing = TRUE)[seq(migPop)]
      migrationPop <- GAs[[from]]$population[j,,drop=FALSE]
      ## substitute the worst individuals
      # j <- order(GAs[[to]]@fitness, decreasing = FALSE)[seq(migPop)]
      ## substitute random individuals
      # j <- sample(GAs[[to]]@popSize, size = migPop)
      ## substitute random individuals but the elitist ones
      j <- sample(setdiff(seq(islSize),
                          order(GAs[[to]]$fitness, decreasing = TRUE)[seq(elitismPercentage)]),
                  size = migPop)
      newpop <- rbind(GAs[[to]]$population[-j,,drop=FALSE], migrationPop)
      POPs[[to]] <- newpop
    }
  }
  
  fitnessValues <- lapply(GAs, function(x) max(x$fitness, na.rm = TRUE))
  # get islands' solutions
  
  solutions <- lapply(GAs, function(x) x$solution)
  # colnames(solution) <- parNames(GAs[[1]])
  fitnessValue <- max(unlist(fitnessValues), na.rm = TRUE)
  solution <- solutions[[which(fitnessValue == unlist(fitnessValues))[1]]]
  
  return(list(fitnessValues = fitnessValues, solutions = solutions, solution = solution))
  # 
  # # islands fitness & solution
  # object@fitnessValues <- fitnessValues
  # object@solutions <- solutions
  # # overall fitness & solution
  # 
  # 
  # # return an object of class 'gaisl'
  # return(object)
}