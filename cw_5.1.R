# Interesujące nas parametry: S, N i H
S <- function(X){
  return(sum(X))
}

N <- function(X){
  N <- 0
  for (i in 2:dim(X)[1]-1) {
    for(j in 2:dim(X)[2]-1){
      N <- N + X[i, j] * X[i+1, j] + X[i, j] * X[i, j+1]
    }
  }
  return(N)
}

H <- function(X, alpha0, alpha1){
  return( alpha0 * S(X) + alpha1 * N(X) )
}

# Funkcje losujące krok i trajektorię
step <- function(X, d, alpha0, alpha1, beta){
  # Zakładam tu że X ma dodane odpowiednie zerowe wiersze i kolumny
  for (i in 2:(d+1)) {
    for(j in 2:(d+1)){
      # Prawdopodobieństwo, że X[i, j] to 1
      prob_1 <- exp(-beta*alpha0 - beta*alpha1*
                      (X[i-1, j] + X[i, j-1] + X[i+1, j] + X[i, j+1]))
      # losowanie
      X[i, j] <- sample(x = c(0, 1), size = 1, replace = TRUE, 
                        prob = c(1, prob_1))
    }
  }
  return(X)
}

pg <- function(n, d, alpha0, alpha1, beta){
  X <- matrix(rep(0, (d+2)^2), nrow = d+2, ncol = d+2)
  S <- numeric(n - 10^3)
  N <- numeric(n - 10^3)
  H <- numeric(n - 10^3)
  for(i in 1:n){
    X <- step(X, d, alpha0, alpha1, beta)
    if(i > 10^3){
      S[i - 10^3] <- S(X)
      N[i - 10^3] <- N(X)
      H[i - 10^3] <- H(X, alpha0, alpha1)
    }
  }
  image(X)
  return(list("E_S" = sum(S)/(n - 10^3), "E_N" = sum(N)/(n - 10^3), "H" = H))
}

# Uruchomienie próbnika
res <- pg(10^4, 20, 4, -2, 0.5)

# Wypisanie E_s i E_N
print(res[["E_S"]])
print(res[["E_N"]])

# Histogram H
#hist(res[["H"]], main = "Symulacja rozkładu Boltzmanna", xlab = "x")
