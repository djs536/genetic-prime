### This script contains a function intended to find as high a prime number as possible
### via genetic algorithm given a certain number of generations.

genPrime <- function(max_generations = 10, population = 100, starting_range = c(1, 100), wait_generations = 5) {
  
  ## Define fitness function
  fit <- function(x) { # each candidate = x
    if(x <= 1) {
      return(-1) # ensures x > 1
    } else if(x == 2) {
      return(x) # handles special case of prime number 2
    }
    for(j in 2:floor(sqrt(x))) {
      if(x %% j == 0) {
        return(-1) # returns -1 if dividing x by any numbers between 2 and sqrt(x) yields remainder of 0
      }
    }
    return(x)
  }
  
  ## Define mutation function
  mutate <- function(x) { # takes entire vector of previous generation
    y <- unique(sort(x[x > 0], decreasing = T)) # sort vector so highest-value candidate are first
    
    survival_prob <- 1 / c(1:length(y)) # NEW: assigns survival probability for each candidate based on ranking (1 has probability of 1)
    
    survival <- c() # NEW: creates survival status storage vector
    
    for(j in 1:length(y)) {
      survival[j] <- sample(c(T, F), 1, prob = c(survival_prob[j], 1 - survival_prob[j])) # NEW: uses probability to determine survival
    }
    
    y <- y[survival] # NEW: kills off non-survivor candidates
    
    n_candidates <- population - length(y) # IDs number of new candidates necessary
    multiples <- runif(n_candidates, 1, 1.1) # randomly seeds multipliers for new candidates
    children <- unique(round(y[1] * multiples)) # rounds child candidates to nearest integer; takes unique candidates
    if(length(children) < n_candidates) { # checks if offspring solutions are sufficient to fill next generation
      children <- c(children, sample(c(max(y):(max(y) + population)), n_candidates - length(children))) # adds random integers as necessary
    }
    return(c(y, children)) # returns new generation
  }
  
  ## Initialize prime storage list with length = generations
  primes <- list(candidates = list(),
                 fitness = list())
  
  ## Initialize generation 1 with n = population using starting_range
  primes$candidates[[1]] <- sample(c(starting_range[1]:starting_range[2]), replace = T, size = population)
  primes$fitness[[1]] <- integer(population)
  
  ## Initialize counter for generations
  counter <- 0
  
  ## For-loop that populates each subsequent generation
  for(i in 2:max_generations) {
    primes$fitness[[i - 1]] <- sapply(primes$candidates[[i - 1]], fit)
    
    if(i > 2) {
      counter <- ifelse(max(primes$fitness[[i - 1]]) == max(primes$fitness[[i - 2]]), counter + 1, 0)
    }
    
    if(counter == wait_generations) {
      break
    }
    
    primes$candidates[[i]] <- mutate(primes$fitness[[i - 1]]) # run mutation and save as new generation
  }
  
  ## Repeat evaluation process for final generation
  
  last_gen <- length(primes$candidates) # IDs location of last candidates list
  
  primes$fitness[[last_gen]] <- sapply(primes$candidates[[last_gen]], fit)
  
  best_solution <- max(primes$fitness[[length(primes$candidates)]]) # ID best solution as highest prime number in final generation
  
  ## Add solution to primes list and return solution
  
  primes$solution <- best_solution
  primes$by_generation <- sapply(primes$fitness, max)
  
  cat("Highest prime found:", best_solution, "\n", "Generations taken:", length(primes$by_generation), "\n")
  return(primes)
  
}




test <- genPrime(100, 100, c(1, 5), 3)

plot(test$by_generation)


