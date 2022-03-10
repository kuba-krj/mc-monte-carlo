library(MCMCpack)
library(MASS)
n <- 10^5

# generujemy rozkład statystyk pozycyjnych
U <- rdirichlet(n, c(2, 2, 2))
#U <- rdirich_b(n, 3, c(2, 2, 2))
M <- matrix(c(U[,1], U[,1] + U[,2]), n, 2)

plot(M, pch=".", col="yellow")
contour(kde2d(M[,1], M[,2]), add = TRUE, col = "red")

# poziomice teoretyczne
# gęstość statystyk pozycyjnych (2:5, 4:5)
dorder <- function(x, y) {
  x[x>y] <- 0
  return(120*(y-x)*(1-y)*x)
}
# przygotowanie i narysowanie punktów
x <- seq(0, 1, by=0.01)
y <- seq(0, 1, by=0.01)
contour(x, y, outer(x, y, FUN = dorder), add=TRUE, col="blue")

# sprawdzenie kowariancji
cov(M)