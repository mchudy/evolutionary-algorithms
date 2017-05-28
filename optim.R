#install.packages("GA")

library("bbob")
library("GA")
source("classicga.R")

my_optimizer <- function(par, fun, lower, upper, max_eval) {
  classicga(fitness=fun, min=lower, max=upper,
            populationSize = 50, mutationProb = 0.1,
            crossoverProb = 0.2, iterations = max_eval,
            elitismPercentage = 0.05)
  
  #ga(type = "real-valued", fitness = fun, 
  #   min = lower, max = upper, popSize = 50)
  
  #optim(par, fun, method="L-BFGS-B", 
  #    lower=lower, upper=upper, control=list(maxit=max_eval))
}

budget <- 1000
bbo_benchmark(my_optimizer, "l-bfgs-b", "results_data", budget = 1000)
#bbo_timing(my_optimizer)