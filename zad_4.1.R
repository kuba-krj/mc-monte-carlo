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

# Wariancja asymptotyczna
sigma_as <- function(m, k, p0, alpha, beta){
  x <- numeric(k)
  for (i in 1:k) {
    x[k] <- sum(rmarkov(m, p0, alpha, beta))/m
  }
  return(var(x))
}

sigma_as(10^3, 10^3, 1, 0.1, 0.1)



