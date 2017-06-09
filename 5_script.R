#install.packages("readr")
#install.packages("hyperSpec")
library(readr)
library(hyperSpec)
library(serrsBayes)
setwd("~/Desktop/Project")

dt <- read_csv("./2010_06_20_164450.csv", 
               col_names = FALSE, skip = 29)
X <- dt[[1]]
#plot(1:1000, X[1:1000], col=2)


hX <- new("hyperSpec", wavelength=1:6444, spc=t(X))

peaks <- function(series, span=3, ties.method = "first") {
  if((span <- as.integer(span)) %% 2 != 1) stop("'span' must be odd")
  z <- embed(series, span)
  s <- span%/%2
  v <- max.col(z, ties.method=ties.method) == 1 + s
  pad <- rep(FALSE, s)
  result <- c(pad, v, pad)
  result
}
pk <- peaks(X)
sum(pk)
peakLoc <- wl(hX)[pk]
#points(peakLoc, X[peakLoc], col=4, lwd=2)

# peak detection
pk <- c(FALSE,abs(diff(X))>20) & c(FALSE, diff(sign(diff(X)))==-2, FALSE)
peakLoc <- wl(hX)[pk]
#plot(X, type='l', col=2)
#points(peakLoc, X[peakLoc], col=4, lwd=2)

wavenumbers <- c(1:6444)
spectra <- matrix(X, nrow=1, ncol = length(X))
peakLocations <- peakLoc
peakAmplitude <- X[peakLocations]

# fit the model using SMC
lPriors <- list(scale.mu=log(11.6) - (0.4^2)/2, scale.sd=0.4, bl.smooth=10, bl.knots=50,
                beta.mu=200, beta.sd=200, noise.sd=200, noise.nu=4)
## Not run:
## takes approx. 1 minute for 100 SMC iterations with 10,000 particles
result <- fitSpectraSMC(wavenumbers, spectra, peakLocations, lPriors)
plot.ts(result$ess, xlab="SMC iterations", ylab="ESS")
# sample 200 particles from the posterior distribution
samp.idx <- sample.int(length(result$weights), 200, prob=result$weights)
plot(wavenumbers, spectra[1,], type='l', xlab="Raman offset", ylab="intensity")
for (pt in samp.idx) {
  bl.est <- result$basis %*% result$alpha[,1,pt]
  lines(wavenumbers, bl.est, col="#C3000009")
  lines(wavenumbers, bl.est + result$expFn[pt,], col="#0000C309")
}

# test 2
# lPriors <- list(scale.mu=log(11.6) - (0.4^2)/2, scale.sd=0.4, bl.smooth=10^3, bl.knots=50,
#                 beta.mu=500, beta.sd=500, noise.sd=200, noise.nu=4)

#test 3
# lPriors <- list(scale.mu=log(11.6) - (0.4^2)/2, scale.sd=0.4, bl.smooth=10^11, bl.knots=50,
#                 +                 beta.mu=5000, beta.sd=5000, noise.sd=200, noise.nu=4)

#test 5
# lPriors <- list(scale.mu=log(11.6) - (0.4^2)/2, scale.sd=0.4, bl.smooth=10, bl.knots=50,
#                 beta.mu=200, beta.sd=200, noise.sd=200, noise.nu=4)

