fibonacci <- function(n) {
  
  # Generate n Fibonacci numbers using a while loop
  #
  # Args:
  # - n: number of Fibonacci to be generated 
  # 
  # Returns:
  # - vector of Fibonacci numbers
  
  # error handling
  if (!is.numeric(n) || length(n) != 1 || n != round(n) || n < 1) {
    stop("Argument 'n' must be a natural number.")
  }

  # start of the sequence
  f <- c(0, 1)
  if (n < 3) return(f[1:n])
  
  # iterative loop to generate next values
  for (i in 1:(n-2)) {
    new_f <- f[length(f)] + f[length(f) - 1]
    f <- c(f, new_f)
  }
  
  return(f)
  
}


fast_fibonacci <- function(n) {
  
  # Generate n Fibonacci numbers using Binet's formula
  #
  # Args:
  # - n: number up to which Fibonacci numbers are to be found 
  # 
  # Returns:
  # - vector of Fibonacci numbers
  
  # error handling
  if (!is.numeric(n) || length(n) != 1 || n != round(n) || n < 1) {
    stop("Argument 'n' must be a natural number.")
  }
  
  # start of the sequence
  if (n < 3) return(c(0,1)[1:n])
  
  # Binet's formula for the nth value of the Fibonacci sequence
  binets_formula <- function(n) {
    phi <- 1 + sqrt(5)
    psi <- 1 - sqrt(5)
    f <- ((phi/2)^n - (psi/2)^n) / sqrt(5)
    return(f)
  }
  
  # generate values using Binet's fomula
  return(sapply(0:(n-1), binets_formula))
  
  
  # Comparison to iterative Fibonacci number generation 
  #
  # magnitudes <- 1:15
  # slow <- sapply(magnitudes, function(x) system.time(fibonacci(2^x)))
  # fast <- sapply(magnitudes, function(x) system.time(fast_fibonacci(2^x)))
  # 
  # plot(slow[1,] ~ magnitudes, 
  #      type = "l", 
  #      ylab = "Time in s", 
  #      xlab = "2^x many Fibonacci numbers generated",
  #      main = "Solution with loop vs. Binet's formula")
  # lines(fast[1,] ~ magnitudes, col = "red")
  # legend("topleft",
  #        legend = c("Loop", "Binet"),
  #        lty = c(1,1),
  #        col = c(1,2))
  
}
