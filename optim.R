#install.packages("GA")

library("bbob")
library("GA")

my_optimizer <- function(par, fun, lower, upper, max_eval) {
  ga(type = "real-valued", fitness = fun, 
     min = lower, max = upper, popSize = 50)
  
  #optim(par, fun, method="L-BFGS-B", 
  #  lower=lower, upper=upper, control=list(maxit=max_eval))
}

budget <- 10000
bbo_benchmark(my_optimizer, "l-bfgs-b", "results_data", budget = 10000)
#bbo_timing(my_optimizer)