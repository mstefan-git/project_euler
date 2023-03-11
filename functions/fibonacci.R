fibonacci <- function(n) {
  
  # Generate Fibonacci numbers up to n
  #
  # Args:
  # - n: number up to which Fibonacci numbers are to be found 
  # 
  # Returns:
  # - vector of Fibonacci numbers
  
  # error handling
  if (!is.numeric(n) || length(n) != 1 || n != round(n) || n < 1) {
    stop("Argument 'n' must be a natural number (> 0).")
  }

  # start of the sequence
  f <- c(0, 1)
  
  # iterative loop to generate next values
  while (f[length(f)] < n) {
    new_f <- f[length(f)] + f[length(f) - 1]
    f <- c(f, new_f)
  }
  
  return(f)
  
}
