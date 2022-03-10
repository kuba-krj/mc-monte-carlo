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
t1 <- rmarkov(10^4, 1, 0.5, 1)

plot(t1, type = 's', col="red")

# Rozkład stacjonarny
plot(density(t1), col="red")
curve(dnorm(x, sd = 1.18), add = TRUE, col="blue")

# Generowanie rozkładu stacjonarnego
# X <- rnorm(10^4, 0, 1.18)
# plot(X, type='s')

