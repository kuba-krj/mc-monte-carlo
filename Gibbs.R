# Funkcje losujące krok i trajektorię
step <- function(mu, kappa){
  mu_n <- rnorm(1, 1/(5*kappa+0.2), 1/sqrt((5*kappa+0.2)))
  kappa_n <- rgamma(1, shape = 2.5, rate = 2.5+2.5*mu_n^2)
  return(c(mu_n, kappa_n))
}

pg <- function(n){
  m <- matrix(nrow = n, ncol = 2)
  m[1,] <- c(0, 0)
  for (i in 2:n) {
    m[i,] <- step(mu = m[i-1, 1], kappa = m[i-1, 2])
  }
  return(m)
}

# Losowanie konkretnej trajektorii i jej wykres
gibbs <- pg(10^5)
plot(gibbs, pch = '.', xlim = c(-2, 4), ylim = c(0, 3), col = "orange",
     xlab = "mu", ylab = "kappa", main = "Przykład - próbnik Gibbsa")

# Rysowanie teoretycznej gęstości
dens <- function(x, y){
  return(exp(-y*(2.5+2.5*x^2)-0.1*(x-5)^2)*y^(1.5))
}

x <- seq(-1, 4, 0.01)
y <- seq(0, 2.5, 0.01)
contour(x, y, outer(x, y, FUN = dens), add=TRUE, col="blue")

# Wariancja asymptotyczna
n <- 10^3

sumy_mu <- numeric(n)
sumy_kappa <- numeric(n)
for (i in 1:n) {
  gibbs <- pg(1000)
  mu <- gibbs[,1]
  kappa <- gibbs[,2]
  sumy_mu[i] <- sum(mu)
  sumy_kappa[i] <- sum(kappa)
}

print(1/n * var(sumy_mu))
print(1/n * var(sumy_kappa))

var(pg(10^4))





