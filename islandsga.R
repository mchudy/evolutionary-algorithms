source("classicga.R")

# TODO: might consider parallel execution

islandsga <- function(fitness,
                      min,
                      max,
                      migrationDirection, # a vector which length is equal to the number of islands, and its values represents 
                                          # islands to which migrations should be performed
                      populationSize = 50,
                      islandsCount = 5,
                      migrationRate = 0.05,
                      migrationInterval = 10,
                      crossoverProb = 0.8,
                      mutationProb = 0.1,
                      elitismPercentage = 0.1,
                      verbose = FALSE
) {
  for(iter in seq_len(iterations)) {
    # run classicga for every island for migrationInterval iterations (need to adjust it - add population and fitnessVec arguments)
  }
}