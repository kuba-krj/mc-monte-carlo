library(ggplot2)

n <- 10^4
alpha <- c(30, 1)

# używając gamma
rdirich_g <- function(n, d, alpha){
  M <- matrix(nrow = n, ncol = d)
  for (i in 1:d) {
    M[,i] <- rgamma(n, alpha[i])
  }
  M <- t(apply(M, 1, function(x) x/sum(x)))
  return(M)
}

# używając beta
rdirich_b <- function(n, d, alpha){
  B <- matrix(nrow = n, ncol = d-1)
  M <- matrix(nrow = n, ncol = d)
  # konstrukcja macierzy B
  for (i in 1:(d-1)) {
    B[,i] <- rbeta(n, alpha[i], sum(alpha[(i+1):d]))
  }
  # ten wektor będzie przechowywał iloczyny
  product <- 1-B[,1]
  # konstrukcja macierzy wynikowej
  M[,1] <- B[,1]
  if(d > 2){
    for(i in 2:(d-1)){
      M[,i] <- product*B[,i]
      product <- product*(1-B[,i])
    }
  }
  M[,d] <- product
  return(M)
}

# wywołanie funkcji i wykres
Mg <- rdirich_g(n, 2, alpha)
Mb <- rdirich_b(n, 2, alpha)
Md <- rdirichlet(n, alpha)
# plot(Mg, pch="x", col="blue", ylim = c(0, 1), xlim=c(0, 1))
# points(Mb, pch="+", col="yellow")
# points(Md, col="green")


# wykres z ggplot
p <- ggplot() +
  xlim(c(0, 1)) + ylim(c(0, 1)) +
  labs(title = "Dwuwymiarowy rozkład Dirichleta") +
  theme(plot.title = element_text(hjust = 0.5, size=22)) +
  geom_point(data=as.data.frame(Md), aes(x=Md[,1], y=Md[,2], label="Z pakietu MCMCPack"),
             shape=3) +
  geom_point(data=as.data.frame(Mg), aes(x=Mg[,1], y=Mg[,2]),
             shape=3, label="Za pomocą Gamma")

print(p)

# M <- rdirich_g(1, 10, rep((1/1000), 10))
# M <- as.vector(M)
# M <- cumsum(M)
# plot(ecdf(M))
# rug(M)

# Obejrzyjmy rozkłady brzegowe
# X1 <- Mg[,1]
# X2 <- Mg[,2]
# Y1 <- Mb[,1]
# Y2 <- Mb[,2]
# plot(ecdf(X1))
# plot(ecdf(Y1), add=TRUE, col="green")
# plot(ecdf(X2), col="red")
# plot(ecdf(Y2), add=TRUE, col="blue")
