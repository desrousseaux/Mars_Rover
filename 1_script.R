setwd("~/Documents/Imperial/Project/Dropbox/Mars Rover MSc")
basDAT <- read.table("basalt.dat")
basLAB <- c("Si", "Ti", "Al", "Mg", "Ca", "Na", "K", "P", "O", "C", "H", "Fe", "Mn")
library(hyperSpec)
library("RColorBrewer")
basSPC <- new("hyperSpec", wavelength=basDAT$V1, spc=t(basDAT[,3:15]))
normSPC <- sweep(basSPC, 2, max, '/')
plot(normSPC, col=c("#000000",brewer.pal(12, 'Paired')), stacked=TRUE,
     axis.args=list(y=list(labels=basLAB)))

peaks <- function(series, span=3, ties.method = "first") {
    if((span <- as.integer(span)) %% 2 != 1) stop("'span' must be odd")
    z <- embed(series, span)
    s <- span%/%2
    v <- max.col(z, ties.method=ties.method) == 1 + s
    pad <- rep(FALSE, s)
    result <- c(pad, v, pad)
    result
}

peaksNa <- peaks(t(basSPC[[6,]]))
sum(peaksNa)


peakLoc <- wl(basSPC)[peaksNa]
plot(basSPC[6,,1.512~1.515])
points(peakLoc, basSPC[[6,,peakLoc]], col=4, lwd=2)
#plot(basSPC[6,,1.58~1.95])
#points(peakLoc, basSPC[[6,,peakLoc]], col=4, lwd=2)

lPriors <- list(scale.mu=5e-04, scale.sd=3e-4, bl.smooth=10^13, bl.knots=50,
                  amp.mu=1e19, amp.sd=2e19, noise.sd=200, noise.nu=4)
sd_mh <- c(rep(1e14, length(peakLoc)), rep(1e-6, length(peakLoc)))

library("serrsBayes")
system.time(
  mhResult <- fitSpectraMCMC(wl(basSPC), basSPC[[6,]], peakLoc, lPriors, sd_mh, niter = 5000)
)

mhResult$n_acc/(5000*4)
plot.ts(mhResult$sigma)
