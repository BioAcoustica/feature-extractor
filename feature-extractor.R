library(tuneR);
library(seewave);
source("functions.R")
source("features.R")

recordings <- c(
  "data/gryllus_campestris.wav"
#  "data/gryllotalpa_gryllotalpa.wav"
#  "data/metrioptera_roeselii.wav",
#  "data/chorthippus_albomarginatus.wav"
)

#Read the waveform

time <- function(wave, f) {
  #type #chirpL # chirpI
  output <- c("continuous", NA, NA, NA)
  times <- timer(wave, f=f, msmooth=c(50,0), threshold=15)
  
  #Chirp interval
  #Sort pause periods into order of size
  sorted <- times$p[order(times$p)]
  #find transition from small to large
  tolerance <- 20
  running_mean <- c(sorted[1])
  transition <- c()
  for (i in 1:(length(sorted)-1)) {
    if (sorted[i] > mean(running_mean) * (1 + tolerance) ) {
      print(c(sorted[i], (mean(running_mean) * (1 + tolerance))))
      transition <- c(transition, sorted[i])
    }
    running_mean[i+1] <- sorted[i+1] 
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


data <- matrix(ncol = 9)
for (i in 1:length(recordings)) {
  wave <- readWave(recordings[i]);
  f <- wave@samp.rate;
  #wave <- cutw(wave, f, from=0, to=5);
  data <- rbind(data, c(frequency(wave, min=1, max=40), time(wave, f)))

}

