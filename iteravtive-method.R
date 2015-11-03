
t <- 0.0;
increment <- 0.00005;
i <- 0;

amplitude <- c();
times <- c();
d <- duration(wave, f);

while (t < d-t-t) {
  t <- t + increment; #Initial matching would be highest, so increment straight away
  dwave <- addsilw(wave, f=f, at="start", d=t);
  iwave <- addsilw(wave, f=f, at="end", d=t);
  
  cross <- iwave + dwave;
  cross <- cutw(cross, f=f, from=t, to=d);
  
  amplitude[i] <- rms(cross);
  #plot(cross);
  #amplitude[i] <- rms(iwave+dwave);
  i <- i +1;
}

plot(amplitude);

#Location of local maxima
library(zoo)
amplitudez <- zoo(amplitude);
maximaz <- rollapply(amplitudez, 5, function(amplitude) which.max(amplitude)==2)
maxima <- index(maximaz)[coredata(maximaz)]

maximat <- maxima * increment;

diff <- c();
for (i in 1:length(maximat)-1) {
  diff[i] <- maximat[i+1] - maximat[i];
}

average <- sum(diff)/length(diff);
print(average);