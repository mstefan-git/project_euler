rm(list = ls())

# Problem 4: Largest palindrome product

# A palindromic number reads the same both ways. The largest palindrome 
# made from the product of two 2-digit numbers is 9009 = 91 × 99.
# Find the largest palindrome made from the product of two 3-digit numbers.

is_palindrome <- function(n) {
  n <- as.character(n)
  n_rev <- paste0(rev((strsplit(n, ""))[[1]]), collapse = "")
  if (n == n_rev) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

largest_palindrome <- function(n_digits) {
  n <- 0
  numbers <- 1:(10^n_digits-1)
  for (n1 in numbers) {
    for (n2 in numbers) {
      p <- n1*n2
      if (is_palindrome(p) & p > n) {
        n <- p
      }
    }
  }
  return(n)
}

largest_palindrome(1)
largest_palindrome(2)
largest_palindrome(3)
