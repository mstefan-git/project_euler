# Challenge 3: Largest prime factor

# The prime factors of 13195 are 5, 7, 13 and 29.
# What is the largest prime factor of the number 600851475143 ?

find_prime_factors <- function(n) {
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
  return(unique(factors))
}

find_prime_factors(10)
find_prime_factors(13195)
find_prime_factors(600851475143)
