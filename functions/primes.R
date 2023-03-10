find_prime_numbers <- function(n = 1e6) {
  
  # Find prime numbers using the "Sieve of Eratosthenes"
  #
  # Args:
  # - n: number up to which prime numbers are to be found 
  #      (i.e. size of the sieve)
  # 
  # Returns:
  # - vector of prime numbers
  
  # error handling
  if (!is.numeric(n) || length(n) != 1 || n != round(n) || n < 2) {
    stop("Argument 'n' must be a natural number > 1.")
  }
  if (n == 2) {
    return(2)
  } 
  if (n == 3) {
    return(c(2,3))
  }
  
  # create sieve of TRUE values from 1 to n
  primes <- rep(TRUE, n)
  
  # 1 is not a prime number
  primes[1] <- FALSE
  
  # go through the sieve and eliminate multiples of primes
  for (i in 2:as.integer(sqrt(n))) {
    if (!primes[i]) next
    primes[seq(i^2, n, i)] <- FALSE
  }
  
  # return the primes
  return(which(primes))

}


find_prime_factors <- function(n) {
  
  # Find a number's prime factors using a while loop
  #
  # Args:
  # - n: the number for which to find the prime factors
  # 
  # Returns:
  # - vector of prime factors
  
  # error handling
  if (!is.numeric(n) || length(n) != 1 || n != round(n) || n < 2) {
    stop("Argument 'n' must be a natural number > 1.")
  }

  # initialize factors as empty vector
  factors <- c()
  
  # "2" is the first divisor to be considered
  d <- 2
  
  # loop through ever greater divisors 
  while (n >= d) {
    
    # even division is possible
    if (n %% d == 0) {
      
      # add "d" to prime factors
      factors <- c(factors, d)
      
      # divide n by d
      n <- n / d
      
      # in the next iteration we want to check whether 
      # we can divide n/d one more time by the same d
      # therefore we must first decrease d,
      # because the next iteration of the loop increases it
      d <- d - 1
    }
    
    # move to next d
    d <- d + 1
  }
  
  # return the prime factors
  return(factors)
}
