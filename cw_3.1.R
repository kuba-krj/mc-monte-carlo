n <- 10^6

# Prymitywnie
X <- rnorm(n)
res_prymit <- length(X[X > 4]) / length(X)

# Mądrze
# p <- function(x){
#   res <- dnorm(x)
#   res[x < 4] <- 0
#   return(res)
# }
# q <- function(x){
#   x <- x - 4
#   return(dexp(x))
# }

X <- rexp(n)
q <- dexp(X)
X <- X + 4
W <- dnorm(X) / q
res_madr <- sum(W) / n

# Teoretycznie
res_teor <- 1 - pnorm(4)

# Wypisanie
# Prymitywnie
print(res_prymit)
# Teoretycznie
print(res_teor)
# Mądrze 
print(res_madr)