rm(list = ls())

# Problem 5: Smallest multiple

# 2520 is the smallest number that can be divided by each of the numbers 
# from 1 to 10 without any remainder. What is the smallest positive number 
# that is evenly divisible by all of the numbers from 1 to 20?

source("functions/gcd_lcm.R")

lcm_vec(1:10)
lcm_vec(1:20)
