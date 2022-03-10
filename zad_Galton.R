# Generowanie następnego kroku
step <- function(X, lambda){
  return(rpois(1, lambda*X))
}

# Generowanie ciągu stanów
rgalton <- function(n, lambda){
  traj <- numeric(n+1)
  traj[1] <- 1
  for(i in 2:(n+1)){
    traj[i] <- step(traj[i-1], lambda)
  }
  return(traj)
}

# Rysowanie
t1 <- rgalton(10, 1.5)
plot(t1, type = 's', col="red")

# Prawdopodobieństwo wymarcia
p_wym <- function(lambda){
  n <- 10^5
  t <- numeric(n)
  for(i in 1:n){
    t[i] <- rgalton(12, lambda)[12]
  }
  return(1 - length(t[t>0])/n)
}

# Obliczenia numeryczne
p_wym_teor <- function(lambda){
  if(lambda <= 1)
    return(1)
  u <- uniroot(function(x, lambda) exp(lambda*(x-1)) - x, 
    c(0, 1-.Machine$double.eps^0.25), lambda = lambda)
  return(unlist(u)[[1]])
}

p_wym(1.5)
p_wym_teor(1.5)
