library(ggplot2)

step <- function(x, a){
  X_next <- runif(1, x-a, x+a) 
  U <- runif(1)
  a <- dnorm(X_next)/dnorm(x)
  if(U > a){
      X_next <- x
  }
  return(X_next)
}

rmarkov <- function(n, a){
  x <- numeric(n)
  x[1] <- 0
  for (i in 2:n) {
    x[i] <- step(x[i-1], a)
  }
  return(x)
}

x <- rmarkov(10^4, 5)
hist(x, prob = TRUE)
curve(dnorm(x), add = TRUE)




