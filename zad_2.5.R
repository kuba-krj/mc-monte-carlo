n <- 40
alpha <- (1/2)
beta <- (1/2)

# Generowanie następnego kroku
step <- function(x, alpha, beta){
  X <- runif(1)
  if(x == 1){
    res = if(X < alpha) 2 else 1
  }
  if(x == 2){
    res = if(X < beta) 1 else 2
  }
  return(res)
}

# Generowanie ciągu stanów
rmarkov <- function(n, p0, alpha, beta){
  traj <- numeric(n+1)
  traj[1] <- p0
  for(i in 2:(n+1)){
    traj[i] <- step(traj[i-1], alpha, beta)
  }
  return(traj)
}

# Obliczanie rozkładu stacjonarnego (argument - trajektoria)
stat <- function(t){
  res <- numeric(2)
  res[1] <- length(t[t == 1])/length(t)
  res[2] <- length(t[t == 2])/length(t)
  return(res)
}

# Rysowanie
t1 <- rmarkov(40, 1, 0.1, 0.1)
t2 <- rmarkov(40, 1, 0.9, 0.9)

# plot(t1, type = 's', col="red")
plot(t2, type='s', col="blue")

# Obliczanie rozkładu stacjonarnego
# alpha = 0.1, beta = 0.9
t <- rmarkov(10^4, 1, 0.9, 0.1)
s <- stat(t)
print(s)
# generowanie trajektorii łańcucha stacjonarnego
X <- runif(50)
X[X < s[2]] <- 2
X[X >= s[2] & X < 2] <- 1
plot(X, type = 's')
