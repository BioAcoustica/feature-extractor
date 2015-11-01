frequency <- function(wave, min=0, max=200) {
  output <- c( NA, NA, NA, NA, NA)
  fspectrum <- spec(wave, f=f)
  peaks <- localpeaks(fspectrum)
  peaksT <- data.frame(peaks)
  peaksT <- peaksT[peaksT$freq > min,]
  peaksT <- peaksT[peaksT$freq < max,]
  peaksT <- peaksT[order(peaksT$amp, decreasing=TRUE),]
  output[1] <- peaksT[1,1]
  
  #Harmonic1
  h1 <- output[1]*2
  h1_margin <- 0.01
  h1_range <- c(h1*(1-h1_margin), h1*(1+h1_margin))
  hT <- subset(peaksT, peaksT$freq > h1_range[1])
  hT <- subset(hT, hT$freq < h1_range[2])
  if (length(hT[,1]) == 1) {
    h1_found <- hT[1,]
    output[2] <- h1_found$freq
    output[3] <- h1_found$amp
  }
  
  #Harmonic2
  h2 <- output[1]*3
  h2_margin <- 0.01
  h2_range <- c(h2*(1-h2_margin), h2*(1+h2_margin))
  hT <- subset(peaksT, peaksT$freq > h2_range[1])
  hT <- subset(hT, hT$freq < h2_range[2])
  if (length(hT[,1]) == 1) {
    h2_found <- hT[1,]
    output[4] <- h2_found$freq
    output[5] <- h2_found$amp
  }
  return(output)
}

time <- function(wave, f) {
  #type #chirpL # chirpI
  output <- c("continuous", NA, NA, NA)
  times <- timer(wave, f=f, msmooth=c(50,0), threshold=10)
  
  #Chirp interval
  #Sort pause periods into order of size
  sorted <- times$p[order(times$p)]
  #find transition from small to large
  tolerance <- 1
  running_mean <- c()
  transition <- c()
  for (i in 1:length(sorted)) {
    running_mean[i] <- sorted[i] 
    if (sorted[i] > mean(running_mean) * (1 + tolerance) ) {
      transition <- c(transition, sorted[i])
      
    }
  }
  if (length(transition) > 0) {
    output[1] <- "chirp"
    longer <- times$p[times$p > transition[1]]
    output[2] <- min(longer)
    output[3] <- max(longer)
    #Chirp length
    start.diffs <- diff(times$s.start)
    chirps <- c()
    chirp <- c()
    for (i in 1:length(start.diffs)) {
      if (length(chirp) == 0) {
        #this is the start of a chirp
        chirp[1] <- times$s.start[i]
      }
      if (start.diffs[i] >= transition[1]) {
        #This is the end of the chirp
        chirp[2] <- times$s.end[i]
        chirps <- c(chirps, c(chirp))
        chirp <- c()
      }
    }
    chirp.d <- c()
    for (i in 1:(length(chirps)/2)) {
      chirp.d[i] <- chirps[2*i] - chirps[(2*i)-1]
    }
    output[4] <- mean(chirp.d)
  }
  return(output)
}
time <- function(wave, f) {
  #type #chirpL # chirpI
  output <- c("continuous", NA, NA, NA)
  times <- timer(wave, f=f, msmooth=c(50,0), threshold=10)
  
  #Chirp interval
  #Sort pause periods into order of size
  sorted <- times$p[order(times$p)]
  #find transition from small to large
  tolerance <- 5
  running_mean <- c()
  transition <- c()
  for (i in 1:length(sorted)) {
    running_mean[i] <- sorted[i] 
    if (sorted[i] > mean(running_mean) * (1 + tolerance) ) {
      transition <- c(transition, sorted[i])
      
    }
  }
  if (length(transition) > 0) {
    output[1] <- "chirp"
    longer <- times$p[times$p > transition[1]]
    output[2] <- min(longer)
    output[3] <- max(longer)
    #Chirp length
    start.diffs <- diff(times$s.start)
    chirps <- c()
    chirp <- c()
    for (i in 1:length(start.diffs)) {
      if (length(chirp) == 0) {
        #this is the start of a chirp
        chirp[1] <- times$s.start[i]
      }
      if (start.diffs[i] >= transition[1]) {
        #This is the end of the chirp
        chirp[2] <- times$s.end[i]
        chirps <- c(chirps, c(chirp))
        chirp <- c()
      }
    }
    chirp.d <- c()
    for (i in 1:(length(chirps)/2)) {
      chirp.d[i] <- chirps[2*i] - chirps[(2*i)-1]
    }
    output[4] <- mean(chirp.d)
  }
  return(output)
}
