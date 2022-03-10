# -------------------------------------------------------
# Zadanie 1
n <- 10^6

X_1 <- runif(n)
X_2 <- runif(n)
U <- runif(n)

X <- numeric(n)
X[U < X_1/(X_1+X_2)] <- X_1[U < X_1/(X_1+X_2)]
X[U >= X_1/(X_1+X_2)] <- X_2[U >= X_1/(X_1+X_2)]

hist(X, prob=TRUE)
curve(2*x*log(1+x)-2*x*log(x), add=TRUE)


# -------------------------------------------------------
# Zadanie 2
n <- 10^7

# Żeby było co najmniej 1000 przypadków, które rozpatrujemy
repeat{
  X <- sample(c(-0.5, 0.5), size=n, replace=TRUE)
  S <- cumsum(X)
  ind <- which(S == 0)
  ind <- ind[ind >= 200]
  if(length(ind) >= 1000)
    break;
}

# Funkcja zwraca S na 200 indeksach poprzedzających i
R <- function(i){
  if(i < 200)
    return(0)
  else
    return(S[(i-199):i])
}

m <- length(ind)

W <- numeric(m)
j <- 1
for(i in ind){
  W[j] <- max(R(i)) - min(R(i))
  j <- j + 1
}

res <- sum(W)/m

# Wynik
print(res)

# Przedział ufności
dokl <- qnorm(0.95) * sd(W) / sqrt(m)
print(c(res - dokl, res + dokl))

# Szerokość przedziału ufności
# Niestety moja metoda nie pozwala 
# uzyskać żądanego przedziału ufności
print(2*dokl)

# -------------------------------------------------------
# Zadanie 3
n <- 10^4

step <- function(X, Y){
  Y_new <- runif(1, X, 1)
  X_new <- runif(1, 0, Y_new)
  return(c(X_new, Y_new))
}

rchain <- function(n){
  X <- numeric(n)
  Y <- numeric(n)
  X[1] <- 0.5
  Y[1] <- 0.5
  
  for (i in 2:n) {
    result <- step(X[i-1], Y[i-1])
    X[i] <- result[1]
    Y[i] <- result[2]
  }
  return(matrix(c(X, Y), n, 2))
}

chain <- rchain(n)
plot(chain)


# -------------------------------------------------------
# Zadanie 4
n <- 10^7

X <- runif(n)
U <- runif(n)

p <- function(x) {return(0.1*(1+(1/(2*sqrt(x)))))}

X <- X[U <= p(X)]
hist(X, prob=TRUE, breaks = 100)
curve(0.5*(1+(1/(2*sqrt(x)))), add=TRUE)

