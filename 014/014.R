rm(list = ls())

collatz <- function(n) {
  if (n %% 2 == 0) {
    return(n/2)
  } else {
    return(3*n+1)
  }
}

n <- 1e4
lengths <- rep(0,n)
for (i in 2:n) {
  x <- i
  count <- 0
  while (x != 1 & x >= i) {
    x <- collatz(x)
    count <- count + 1
  }
  count <- count + lengths[x]
  lengths[i] <- count
}
which.max(lengths)


collatz.length <- vector(length=1e4)
collatz.length[1] <- 0
for (n in 2:1e4) {
  x <- n
  count <- 0 
  while (x != 1 & x >= n) {
    if (x %% 2 == 0) {
      x <- x / 2
      count <- count + 1
    }
    else {
      x <- (3 * x + 1) / 2
      count <- count + 2
    } 
  }
  count <- count + collatz.length[x]
  collatz.length[n] <- count
}
answer <- which.max(collatz.length)
print(answer)