# Problem 6: Sum square difference

# The sum of the squares of the first ten natural numbers is 385.
# The square of the sum of the first ten natural numbers is 3025.
# Hence the difference between the sum of the squares of the first ten 
# natural numbers and the square of the sum is 2640.
# Find the difference between the sum of the squares of the first 
# the hundred natural numbers and the square of the sum.

myfunction <- function(n) {
  numbers <- 1:n
  sum_of_squares <- sum(numbers^2)
  square_of_sum <- (sum(numbers))^2
  return(square_of_sum - sum_of_squares)
}

myfunction(10)
myfunction(100)
