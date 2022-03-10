# Zadanie 1.7
n <- 10^6

Y <- runif(n)
X <- runif(n, 0, Y)

plot(density(X))
curve(-log(x), add=TRUE, col="red")
