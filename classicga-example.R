source("classicga.R")

# Example 1
f <- function(x)  (x^2+x)*cos(x) # -10 < x < 10
curve(f, -10, 10)
GA <- classicga(fitness =  f, min = -10, max = 10, populationSize = 50, verbose = FALSE)
print(GA)

# Example 2 (difficult function)
Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}
GA <- classicga(fitness =  function(x) -Rastrigin(x[1], x[2]),
                min = c(-5.12, -5.12), max = c(5.12, 5.12), verbose = FALSE)
print(GA)

GA <- GA::ga(type = "real-valued", fitness =  function(x) -Rastrigin(x[1], x[2]),
             min = c(-5.12, -5.12), max = c(5.12, 5.12), 
             popSize = 50, maxiter = 1000, crossover = ga_spCrossover)
print(GA)