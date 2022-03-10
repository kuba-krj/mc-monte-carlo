n <- 100
par <- 0.2
len <- 200

plot_pois <- function(n, len, par, plot_color, plot_add){
  X <- c(0, rexp(n, rate=par))
  Y <- c(0, cumsum(X))
  Y <- Y[Y < len]
  print(Y)
  plot(stepfun(1:(length(Y)-1), Y), col = plot_color, 
       add=plot_add, xlim = c(0, 50))
}

plot_pois(n, len, par, "red", FALSE)
plot_pois(n, len, par, "green", TRUE)
plot_pois(n, len, par, "blue", TRUE)

# jeszcze spojrzeć na print screen: nie jestem pewien czy to działa
