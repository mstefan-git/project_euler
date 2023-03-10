rm(list = ls())

# Problem 9: Special Pythagorean triplet

# Pythagorean triplet is a set of three natural numbers, 
# a < b < c, for which, a2 + b2 = c2 .
# For example, 32 + 42 = 9 + 16 = 25 = 52.
# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc.

n <- 500
a <- rep(1:n, n^2)
b <- rep(1:n, each = n^2)
c <- rep(rep(1:n, each = n), n)
sum_abc <- a + b + c
pythagoras <- a^2 + b^2 - c^2
idx <- which(sum_abc == 1000 & pythagoras == 0 & a < b & b < c)

a[idx] * b[idx] * c[idx]
