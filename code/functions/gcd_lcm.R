gcd <- function(a,b) {
  
  # Compute greatest common divisor of two integers
  #
  # Args:
  # - a: first number
  # - b: second number
  # 
  # Returns:
  # - greatest common divisor of a and b
  
  # error handling
  if (!is.numeric(a) || length(a) != 1 || a != round(a) ) {
    stop("Argument 'a' must be an integer.")
  }
  if (!is.numeric(b) || length(b) != 1 || b != round(b) ) {
    stop("Argument 'b' must be an integer.")
  }
  
  # Euclidean algorithm
  while (TRUE) {
    if (a == 0) return(b)
    if (b == 0) return(a)
    b_new <- a %% b
    a <- b
    b <- b_new
  }
  
}



gcd_vec <- function(x) {
  
  # Compute greatest common divisor of multiple integers
  #
  # Args:
  # - x: vector of integers
  # 
  # Returns:
  # - greatest common divisor of the integers contained in x
  
  # error handling
  if (!is.numeric(x) || any(x != round(x)) ) {
    stop("Argument 'x' must be a vector of integers.")
  }
  
  # binary tree (around Euclidean algorithm)
  gcd_end <- x[1]
  for (i in 2:length(x)) {
    gcd_end <- gcd(gcd_end, x[i])
  }
  return(gcd_end)
  
}



lcm <- function(a,b) {
  
  # Compute least common multiple of multiple integers
  #
  # Args:
  # - a: first number
  # - b: second number
  # 
  # Returns:
  # - greatest common divisor of a and b
  
  # error handling
  if (!is.numeric(a) || length(a) != 1 || a != round(a) ) {
    stop("Argument 'a' must be an integer.")
  }
  if (!is.numeric(b) || length(b) != 1 || b != round(b) ) {
    stop("Argument 'b' must be an integer.")
  }
  
  return(abs(a*b) / gcd(a,b)) 
  
}



lcm_vec <- function(x) {
  
  # Compute least common multiple of two integers
  #
  # Args:
  # - x: vector of integers
  # 
  # Returns:
  # - least common multiple of the integers contained in x
  
  # error handling
  if (!is.numeric(x) || any(x != round(x)) ) {
    stop("Argument 'x' must be a vector of integers.")
  }
  
  # assume that the first integer is the least common multiple
  lcm_all <- x[1]
  
  # loop through all other integers and update lcm_all if needed
  for (i in x[-1]) {
    lcm_all <- lcm(lcm_all, i)
  }
  
  # return
  return(lcm_all)
  
}