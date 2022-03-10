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

# Wariancja asymptotyczna
sigma_as <- function(m, k, p0, alpha, v){
  x <- numeric(k)
  for (i in 1:k) {
    x[k] <- sum(rmarkov(m, p0, alpha, v))
  }
  return(var(x)/m)
}

sigma_as(10^2, 10^3, 1, 0.9, 1)

