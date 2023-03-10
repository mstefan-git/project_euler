# Problem 7: 10001st prime

# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
# we can see that the 6th prime is 13. What is the 10 001st prime number?

find_factors <- function(n) {
  d <- 2
  factors <- c()
  q <- n
  while (TRUE) {
    if (q %% d == 0) {
      q <- q / d
      factors <- c(factors, d)
    } else {
      d <- d+1
    }
    if (q == 1) {
      break
    }
  }  
  return(factors)
}

is_prime <- function(n) {
  if (length(find_factors(n)) == 1) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

find_nth_prime <- function(n) {
  i <- 0
  n_primes <- 0
  while (n_primes < n) {
    i <- i+1
    if (is_prime(i)) {
      n_primes <- n_primes + 1
    }
  }
  return(i)
}

find_nth_prime(10001)
