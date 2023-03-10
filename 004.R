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
  x <- 0
  numbers <- 1:(10^n_digits-1)
  for (i in numbers) {
    for (j in numbers) {
      y <- i*j
      if (is_palindrome(y) & y > x) {
        x <- y
      }
    }
  }
  return(x)
}

largest_palindrome(1)
largest_palindrome(2)
largest_palindrome(3)
