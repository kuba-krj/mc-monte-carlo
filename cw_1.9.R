library(mvtnorm)

n <- 10^5

rmult.rnorm <- function(n, d, V) {
  Z <- matrix(rnorm(d*n), n, d)
  A <- chol(V)
  return(Z %*% A)
}

M <- matrix(c(3, 1, 1, 1), 2, 2)

res <- rmult.rnorm(n, 2, M)

# warto sprawdzić:
#cov(res)

plot(res, col="blue", pch="+")

points(rmvnorm(n, c(0, 0), M), col="red", pch="+")
