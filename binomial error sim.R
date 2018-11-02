nsim <- 10000
nsamp <- 2:50
bernoulli <- 1
cointoss <- c(0.5, 0.25)

out <- list()
for(i in 1:length(cointoss)) {
	outtmp <- matrix(NA, nsim, length(nsamp))

	pb <- txtProgressBar(style = 3)
	for(p in 1:length(nsamp)) {
	setTxtProgressBar(pb, p / length(nsamp))
		for(q in 1:nsim) {
			outtmp[q, p] <- mean(rbinom(nsamp[p], bernoulli, cointoss[i]))
		}
	}
	
	out[[i]] <- outtmp
}
close(pb)


### visualize
library(colorspace)

x <- nsamp
y <- lapply(out, apply, 2, sd)

plot(rep(x, length(y)), unlist(y), type = 'n', las = 1, xlab = "n samples", ylab = "sd")
for(i in 1:length(y))
	lines(x, y[[i]], col = rainbow_hcl(length(y))[i], lwd = 2)

abline(v = 10, lty = 2, lwd = 2, col = "grey")

legend("topright", legend = paste0("p = ", format(cointoss, digits = 2)), lty = rep(1, length(y)), col = rainbow_hcl(length(y)), bty = 'n')

x2 <- x[2:length(x)]
y2 <- diff(y[[1]])

dev.new(); plot(x2, y2)
abline(v = 10)