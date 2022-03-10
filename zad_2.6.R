# Generowanie następnego kroku
step <- function(X, alpha, v){
  return(alpha*X + rnorm(1, 0, v))
}

# Generowanie ciągu stanów
rmarkov <- function(n, p0, alpha, v){
  traj <- numeric(n+1)
  traj[1] <- p0
  for(i in 2:(n+1)){
    traj[i] <- step(traj[i-1], alpha, v)
  }
  return(traj)
}

# Rysowanie
alpha <- 0.5
v <- 1
t1 <- rmarkov(10^2, 1, alpha, v)
t2 <- rmarkov(10^2, 1, alpha, v)

matplot(matrix(c(t1, t2), length(t1), 2), type="s", main="Losowania")
legend("top", c("losowanie 1", "losowanie 2"), col=seq_len(2), fill=1:2)
grid()

# Rozkład stacjonarny
# plot(density(t1), col="red", main="Rozkład stacjonarny")
# curve(dnorm(x, sd = v/sqrt(1-alpha^2)), add = TRUE, col="blue")
# grid()

# Generowanie rozkładu stacjonarnego
# X <- rnorm(10^3, sd = v/sqrt(1-alpha^2))
# plot(X, type='s')

