# Challenge 3: Largest prime factor

# The prime factors of 13195 are 5, 7, 13 and 29.
# What is the largest prime factor of the number 600851475143 ?

find_prime_factors <- function(n) {
  prime_factors <- c()
  for (p in 2:floor(sqrt(n))) {
    # check if p divides n without a remainder
    if (n %% p == 0) {
      # check that p is not a multiple of previously found prime factors
      if (all(p %% prime_factors != 0)) {
        prime_factors <- c(prime_factors, p)
      }
    }
  }
  return(prime_factors)
}

find_prime_factors(10)
find_prime_factors(13195)
find_prime_factors(600851475143)
