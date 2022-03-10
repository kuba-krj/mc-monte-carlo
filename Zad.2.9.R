library(ggplot2)
library(dplyr)
# Funkcja zadająca następny czas
X <- function(x) {if(x == 1) 2 else 1}
# Funkcja zadająca następny element
Q <- function(x, alpha, beta) {if(x == 1) alpha else beta}
# Algorytm Gillespy'ego
rGilesp <- function(alpha, beta, t_stop){
  t <- X <- NULL
  t[1] <- 0
  X[1] <- 1
  i <- 1
  while(t[i] <= t_stop) {
    i <- i+1
    W <- rexp(1, Q(X[i-1], alpha, beta))
    t[i] <- t[i-1] + W
    X[i] <- ifelse(X[i-1] == 1, 2, 1)
  } 
  return(matrix(c(t, X), i, 2))
}

# wykres
n <- 10
proc <- rGilesp(0.7, 0.6, n)
qplot(proc[,1], proc[,2], geom = "step", xlab = "", ylab = "") 

# symulacja rozkładu stacjonarnego (średni czas przebywania w 1)
t <- proc[,1]
diff <- (t - lag(t))[-1]
prob_1 <- sum(diff[seq(1, length(diff), 2)])/n
print(prob_1)



