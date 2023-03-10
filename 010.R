rm(list = ls())

# Problem10: Summation of primes

# The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
# Find the sum of all the primes below two million.

source("functions/primes.R")

sum(find_prime_numbers(10))
sum(find_prime_numbers(2e6))
