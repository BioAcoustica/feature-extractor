times.diff <- function(x, y) {
  z <- c();
  for (i in 1:length(y)) {
    z[i] <- y[i] - x[i];
  }
  return(z);
}

diff <- function(x) {
  y <- c();
  for(i in 1:length(x)-1) {
    y[i] <- x[i+1] - x[i];
  }
  return(y)
}