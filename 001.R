# Challenge 1: Multiples of 3 and 5

# If we list all the natural numbers below 10 that are multiples of 3 or 5, 
# we get 3, 5, 6 and 9. The sum of these multiples is 23.
# Find the sum of all the multiples of 3 or 5 below 1000.

sum_of_multiples <- function(n_max, multiples = c(3,5)) {
  x <- 0
  for (i in 1:(n_max-1)) {
    if (any(i %% multiples == 0)) {
      x <- x + i
    }
  }
  return(x)
}

sum_of_multiples(1000)
