classicga <- function(fitness,
                      min,
                      max,
                      populationSize = 5,
                      crossoverProb = 0.8,
                      mutationProb = 0.1,
                      maxIterations = 100,
                      verbose = FALSE) 
{
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
    
    if(sum(rev(fitnessVec) >= (max(fitnessVec, na.rm = TRUE) - eps))) {
      
    }
    
    # selection (roulette wheel)
    prob <- abs(fitnessVec)/sum(abs(fitnessVec))
    sel <- sample(1:object@popSize, size = object@popSize, 
                  prob = pmin(pmax(0, prob), 1, na.rm = TRUE),
                  replace = TRUE)
    out <- list(population = object@population[sel,,drop=FALSE],
                fitness = object@fitness[sel])
    
    
    # crossover
    
    # mutation
    
    # elitism
  }
}

Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}

x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
f <- outer(x1, x2, Rastrigin)

GA <- classicga(fitness =  function(x) -Rastrigin(x[1], x[2]),
         min = c(-5.12, -5.12), max = c(5.12, 5.12), verbose = TRUE)

#GA <- ga(type = "real-valued", fitness =  function(x) -Rastrigin(x[1], x[2]),
#         min = c(-5.12, -5.12), max = c(5.12, 5.12), 
#         popSize = 50, maxiter = 100)

rouletteSelection <- function(object, ...)
{
  prob <- abs(object@fitness)/sum(abs(object@fitness))
  sel <- sample(1:object@popSize, size = object@popSize, 
                prob = pmin(pmax(0, prob), 1, na.rm = TRUE),
                replace = TRUE)
  out <- list(population = object@population[sel,,drop=FALSE],
              fitness = object@fitness[sel])
  return(out)
}