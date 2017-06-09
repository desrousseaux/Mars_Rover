library(readr)
basalt <- read.table("~/Documents/Imperial/Project/Dropbox/Mars Rover MSc/basalt.dat", sep="")
for(i in names(basalt)){
  X <- basalt[[i]]/max(basalt[[i]])*1000
  plot(X, type='l', col="red", main=i)
  spl <- smooth.spline(X, spar=0.8)
  lines(X-spl$y, col="blue")
  legend(6000, 6e21, c("Original", "Corrected"), lty=c(1,1), lwd=c(2.5,2.5), col=c("red", "blue"))
  grid()
}

i <- "V12"
X <- basalt[[i]]/max(basalt[[i]])*1000
plot(X, type='l', col="red", main=i, ylim = c(-100, 1100))
spl <- smooth.spline(X,spar=0.6)
lines(X-spl$y, col="blue")
legend(6000, 800, c("Original", "Corrected"), lty=c(1,1), lwd=c(2.5,2.5), col=c("red", "blue"))
grid()

library(wavelets)
wtData <- NULL


