library(mvtnorm)
n <- 5

# Dwuwymiarowy rozkład Cauchy'ego
Z <- rmult.rnorm(n, 2, diag(2))
R <- matrix(sqrt(rchisq(n, 1)), n, 2)
Y <- Z/R
plot(Y, pch="+")

