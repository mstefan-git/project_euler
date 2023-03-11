rm(list = ls())

collatz <- function(n) {
  numbers <- c(n)
  while (n > 1) {
    if (n %% 2 == 0) {
      n  <- n/2
    } else {
      n <- 3*n+1
    }
    numbers <- c(numbers,n)
  }
  return(numbers)
}

search_collatz <- function(n) {
  
  c_max <- 0
  n_max <- 0  
  searched <- c()
  
  for (i in n:1) {
    if (i %in% searched) {
      next
    }
    c <- collatz(i)  
    if (length(c) > c_max) {
      c_max <- length(c)
      n_max <- i
    }
    searched <- unique(c,searched)
    searched <- searched[(which(searched < n))]
  }
  
  return(n_max)
  
}

search_collatz(1e6)
