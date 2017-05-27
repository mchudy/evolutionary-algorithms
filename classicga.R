classicga <- function(fitness,
                      min,
                      max,
                      populationSize = 5,
                      crossoverProb = 0.8,
                      mutationProb = 0.1,
                      maxIterations = 2,
                      verbose = FALSE
) {
  dimension <- length(min)
  fitnessVec <- rep(NA, populationSize)
  eps = sqrt(.Machine$double.eps)
  
  # generate random initial population
  population <- matrix(as.double(NA), nrow = populationSize, ncol = dimension)
  for(j in 1:dimension) { 
    population[,j] <- runif(populationSize, min[j], max[j])
  }
  
  if(verbose) print(population)
  
  for (iter in seq_len(maxIterations)) {
    
    # evaluate fitness
    for(i in seq_len(populationSize)) {
      if(is.na(fitnessVec[i])){ 
        fitnessVec[i] <- fitness(population[i,])
      }
    }
    
    # check stop condition
    
    
    # selection
    selected <- rouletteSelection(fitnessVec, population, populationSize)
    population <- selected$population
    fitnessVec <- selected$fitness
    if(verbose) print(selected)
    
    # crossover
    if(crossoverProb > 0) {
      nmating = floor(populationSize / 2)
      # shouldn't it be populationSize instead of 2*nmating when taking sample?
      mating <- matrix(sample(1:(2 * nmating), size = (2 * nmating)), ncol = 2)
      for(i in seq_len(nmating)) { 
        if(crossoverProb > runif(1)) { 
          parents <- mating[i,]
          crossoverResult <- singlePointCrossover(fitnessVec, population, parents)
          population[parents,] <- crossoverResult$children
          fitnessVec[parents] <- crossoverResult$fitness
        }
      }
    }
    if(verbose) print(population)
    
    # mutation
    
    # elitism
  }
}

rouletteSelection <- function(fitness, population, popSize) {
  prob <- abs(fitness)/sum(abs(fitness))
  sel <- sample(1:popSize, size = popSize, 
                prob = pmin(pmax(0, prob), 1, na.rm = TRUE),
                replace = TRUE)
  result <- list(population = population[sel,,drop=FALSE],
              fitness = fitness[sel])
  return(result)
}

singlePointCrossover <- function(fitnessVec, population, parents) {
  fitness <- fitnessVec[parents]
  parents <- population[parents,,drop = FALSE]
  n <- ncol(parents)
  
  children <- matrix(as.double(NA), nrow = 2, ncol = n)
  fitnessChildren <- rep(NA, 2)
  crossOverPoint <- sample(0:n, size = 1)
  
  if (crossOverPoint == 0) { 
    children[,] <- parents[2:1,]
    fitnessChildren <- rev(fitness)
  } else if (crossOverPoint == n) { 
    children <- parents
    fitnessChildren <- fitness 
  } else { 
    children[1,] <- c(parents[1,1:crossOverPoint],
                      parents[2,(crossOverPoint + 1):n])
    children[2,] <- c(parents[2,1:crossOverPoint],
                    parents[1,(crossOverPoint + 1):n])
  }
  result <- list(children = children, fitness = fitnessChildren)
  return(result)
}

mutation <- function() {
  
}



# Example
set.seed(2)
Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}
GA <- classicga(fitness =  function(x) -Rastrigin(x[1], x[2]),
                min = c(-5.12, -5.12), max = c(5.12, 5.12), verbose = TRUE)

#GA <- ga(type = "real-valued", fitness =  function(x) -Rastrigin(x[1], x[2]),
#         min = c(-5.12, -5.12), max = c(5.12, 5.12), 
#         popSize = 50, maxiter = 100)
