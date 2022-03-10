library(dplyr)

# Obliczenia prostą metodą
R_calc <- function(x, u, b){
  return(min(which(x > u)[1], which(x < -b)[1], na.rm = TRUE))
  }
theta_prymit <- function(M, u, b){
  return(mean(apply(M, 2, R, u = u, b = b)))
}

# Podpunkt a)
mu <- 0; len <- 10^3; n <- 10^5
M <- matrix(rnorm(n*m, mu, 1), len, n)
M <- apply(M, 2, cumsum)
R <- apply(M, 2, R_calc, u = 10, b = 10)
sd(R)
mean(R)
theta_prymit(M, 10, 10)