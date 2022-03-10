library(ggplot2)
library(dplyr)

# Algorytm Gillespy'ego
rEpidem <- function(alpha, beta, t_stop){
  t <- X <- NULL
  t[1] <- 0
  X[1] <- 1
  i <- 1
  while(t[i] <= t_stop & X[i] != 0) {
    i <- i+1
    # Generowanie czasu skoku
    W <- rexp(1, (alpha + beta) * X[i-1])
    t[i] <- t[i-1] + W
    # Wybór miejsca skoku
    X[i] <- sample( c( X[i-1] + 1, X[i-1] - 1 ), size = 1, replace = TRUE, 
                    prob = c(alpha/(alpha+beta), beta/(alpha+beta)) )
  } 
  return(matrix(c(t, X), length(t), 2))
}

# Funkcja do rysowania kilku wykresów
draw <- function(alpha, beta, t_stop, n_lines, log_scale){
  g <- ggplot(NULL)
  for(i in 1:n_lines){
    p <- rEpidem(alpha, beta, t_stop)
    d <- data.frame(t=p[,1], I=p[,2], Próba = rep(paste("Próba",i), dim(p)[1]))
    g <- g + geom_step(data = d, aes(x = t, y = I, color = Próba), 
                       alpha = 0.4, size = 1.3)
  }
  if(log_scale) g <- g + scale_y_continuous(trans='log2')
  g <- g + stat_function(fun = function(t) exp((alpha - beta) * t), 
              aes(color = "Teoretyczne"), size = 1.3, linetype = "dashed")
  print(g)
}

# Rysowanie kilku wykresów
draw(alpha = 0.4, beta = 0.1, t_stop = 25, n_lines = 7, log_scale = FALSE)

# Funkcja do rysowania wartości średniej 
exp_val <- function(alpha, beta, t_stop){
  t <- seq(0, t_stop, 0.01)
  values <- NULL
  for(i in 1:1000){
    M <- rEpidem(alpha, beta, t_stop)
    values <- cbind(values, sapply(t, function(x) tail(M[M[,1] <= x], 1)))
  }
  M <- matrix(values, length(t), 100)
  data.frame(t = t, I = rowMeans(M)) %>%
    ggplot(aes(x = t, y = I)) +
    geom_step(alpha = 0.4, size = 1.3, aes(col = "Doświadczalnie")) +
    stat_function(fun = function(t) exp((alpha - beta) * t), 
                  aes(color = "Teoretyczne"), size = 1.3, linetype = "dashed")
}

exp_val(0.5, 0.1, 5)








