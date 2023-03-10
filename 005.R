rm(list = ls())

# Problem 5: Smallest multiple

# 2520 is the smallest number that can be divided by each of the numbers 
# from 1 to 10 without any remainder. What is the smallest positive number 
# that is evenly divisible by all of the numbers from 1 to 20?

n_max <- 20

source("functions/primes.R")

find_smallest_common_multiple <- function(numbers) {
  individual_factors <- sapply(numbers, find_prime_factors)
  shared_factors <- c()
  for (i in individual_factors) {
    temp <- shared_factors
    for (j in i) {
      if (j %in% temp) {
        temp <- temp[-which(temp == j)[1]]
      } else {
        shared_factors <- c(shared_factors,j)
      }
    }
  }
  return(prod(shared_factors))
}

find_smallest_common_multiple(1:10)
find_smallest_common_multiple(1:20)
