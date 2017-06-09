library(readr)
dt <- read_csv("~/Documents/Imperial/Project/Dropbox/Mars Rover MSc/2010_06_20_164450.csv", 
                col_names = FALSE, skip = 29)
for(i in names(dt)){
  X <- dt[[i]]
  plot(X, type='l', col="red", ylim = c(-1000, 4000), xlim=c(-100, 6600), main=i)
  spl <- smooth.spline(X, spar = 0.8)
  lines(X-spl$y, col="blue")
  legend(4000, 3000, c("Original", "Corrected"), lty=c(1,1), lwd=c(2.5,2.5), col=c("red", "blue"))
}