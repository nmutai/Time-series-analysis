### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch11-TimeSeriesRegression.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
library("tseries")
library("forecast")
library("dynlm")
library("strucchange")
set.seed(1071)


###################################################
### code chunk number 2: arima-xreg (eval = FALSE)
###################################################
## arima(y, order = c(p, d, q), seasonal = c(P, D, Q), xreg = x)


###################################################
### code chunk number 3: Nile
###################################################
plot(Nile)


###################################################
### code chunk number 4: Nile-stationary
###################################################
data("Nile", package = "datasets")
kpss.test(Nile)
adf.test(Nile)


###################################################
### code chunk number 5: Nile-acf
###################################################
par(mfrow = c(1, 2))
acf(Nile)
pacf(Nile)


###################################################
### code chunk number 6: Nile-ar1
###################################################
ar1 <- arima(Nile, order = c(1, 0, 0))
ar1


###################################################
### code chunk number 7: Nile-auto (eval = FALSE)
###################################################
## auto.arima(Nile, ic = "bic", approx = FALSE, stepwise = FALSE,
##   stationary = TRUE)


###################################################
### code chunk number 8: Nile2
###################################################
plot(Nile)
abline(h = coef(ar1)[2], col = 2, lwd = 2)


###################################################
### code chunk number 9: Nile3
###################################################
plot(Nile)
abline(h = coef(ar1)[2], col = 2, lwd = 2)
x <- as.numeric(time(Nile) > 1898)
ar0x <- arima(Nile, order = c(0, 0, 0), xreg = x)
lines(ts(coef(ar0x)[1] + x * coef(ar0x)[2], start = start(Nile)), col = 4, lwd = 2)


###################################################
### code chunk number 10: Nile-ar1x
###################################################
x <- as.numeric(time(Nile) > 1898)
ar1x <- arima(Nile, order = c(1, 0, 0), xreg = x)
coeftest(ar1x)


###################################################
### code chunk number 11: Nile-ar1x-print
###################################################
ar1x


###################################################
### code chunk number 12: Nile-ar0x
###################################################
ar0x <- arima(Nile, order = c(0, 0, 0), xreg = x)


###################################################
### code chunk number 13: Nile-ar1x-print
###################################################
ar0x


###################################################
### code chunk number 14: Nile-auto (eval = FALSE)
###################################################
## auto.arima(Nile, xreg = x, ic = "bic", approx = FALSE,
##   stepwise = FALSE, stationary = TRUE)


###################################################
### code chunk number 15: intervention0 (eval = FALSE)
###################################################
## plot(0, 0, xlim = c(1, 9), ylim = c(0, 1),
##   axes = FALSE, xlab = "", ylab = "")
## lines(c(1, 6), c(0, 0))
## points(1:5, rep(0, 5))
## axis(1, at = 1:9, labels = FALSE)
## axis(1, at = 5, labels = "T")
## axis(2, at = 0, labels = "0")
## box()


###################################################
### code chunk number 16: intervention1
###################################################
plot(0, 0, xlim = c(1, 9), ylim = c(0, 1),
  axes = FALSE, xlab = "", ylab = "")
lines(c(1, 6), c(0, 0))
points(1:5, rep(0, 5))
axis(1, at = 1:9, labels = FALSE)
axis(1, at = 5, labels = "T")
axis(2, at = 0, labels = "0")
box()
lines(c(6, 9), c(1, 1))
points(6:9, rep(1, 4))
axis(2, at = 1, labels = expression(omega))


###################################################
### code chunk number 17: intervention3
###################################################
plot(0, 0, xlim = c(1, 9), ylim = c(0, 1),
  axes = FALSE, xlab = "", ylab = "")
lines(c(1, 6), c(0, 0))
points(1:5, rep(0, 5))
axis(1, at = 1:9, labels = FALSE)
axis(1, at = 5, labels = "T")
axis(2, at = 0, labels = "0")
box()
points(6:9, 0.6 * (1 - 0.4^(6:9 - 5)) / (1 - 0.4), type = "o")
axis(2, at = 0.6, labels = expression(omega))
axis(2, at = 1, labels = expression(omega/(1 - delta)))


###################################################
### code chunk number 18: intervention2
###################################################
plot(0, 0, xlim = c(1, 9), ylim = c(0, 1),
  axes = FALSE, xlab = "", ylab = "")
lines(c(1, 6), c(0, 0))
points(1:5, rep(0, 5))
axis(1, at = 1:9, labels = FALSE)
axis(1, at = 5, labels = "T")
axis(2, at = 0, labels = "0")
box()
points(6:9, 0.25 * (6:9 - 5), type = "o")
axis(2, at = 0.25, labels = expression(omega))


###################################################
### code chunk number 19: intervention4
###################################################
plot(0, 0, xlim = c(1, 9), ylim = c(0, 1),
  axes = FALSE, xlab = "", ylab = "")
lines(c(1, 6), c(0, 0))
points(1:5, rep(0, 5))
axis(1, at = 1:9, labels = FALSE)
axis(1, at = 5, labels = "T")
axis(2, at = 0, labels = "0")
box()
points(6:9,  1.55 - (1 - 0.45^(6:9 - 5)), type = "o")
axis(2, at = 0.55, labels = expression(omega[2]))
axis(2, at = 1, labels = expression(omega[1] + omega[2]))


###################################################
### code chunk number 20: Nile-resid
###################################################
plot(merge(
    Nile = as.zoo(Nile),
    zoo(mean(Nile), time(Nile)),
    CUSUM = cumsum(Nile - mean(Nile)),
    zoo(0, time(Nile)),
    MOSUM = rollapply(Nile - mean(Nile), 15, sum),
    zoo(0, time(Nile))
  ), screen = c(1, 1, 2, 2, 3, 3), main = "", xlab = "Time",
  col = c(1, 4, 1, 4, 1, 4)
)


###################################################
### code chunk number 21: Nile-ocus-plot
###################################################
plot(efp(Nile ~ 1, type = "OLS-CUSUM"), ylim = c(-1.5, 3))


###################################################
### code chunk number 22: Nile-ocus1
###################################################
ocus <- efp(Nile ~ 1, type = "OLS-CUSUM")


###################################################
### code chunk number 23: Nile-ocus2 (eval = FALSE)
###################################################
## plot(ocus)


###################################################
### code chunk number 24: Nile-ocus3
###################################################
sctest(ocus)


###################################################
### code chunk number 25: Nile-Fstats (eval = FALSE)
###################################################
## fs <- Fstats(Nile ~ 1, from = 0.1)
## plot(fs)


###################################################
### code chunk number 26: Nile-Fstats-plot
###################################################
fs <- Fstats(Nile ~ 1, from = 0.1)
plot(fs)


###################################################
### code chunk number 27: Nile-breakpoints
###################################################
bp <- breakpoints(Nile ~ 1)
plot(bp)
summary(bp)


###################################################
### code chunk number 28: Nile-breakpoints-output1
###################################################
bp_out <- capture.output(summary(bp))
writeLines(bp_out[1:14])


###################################################
### code chunk number 29: Nile-breakpoints-output2
###################################################
writeLines(bp_out[-(1:14)])


###################################################
### code chunk number 30: Nile-breakpoints-coef
###################################################
coef(bp)


###################################################
### code chunk number 31: Nile-breakpoints-plot
###################################################
plot(bp)


###################################################
### code chunk number 32: Nile-confint0 (eval = FALSE)
###################################################
## plot(Nile)
## lines(fitted(bp), col = 4, lwd = 2)
## lines(confint(bp))


###################################################
### code chunk number 33: Nile-confint-plot
###################################################
plot(Nile)
lines(fitted(bp), col = 4, lwd = 2)
lines(confint(bp))


###################################################
### code chunk number 34: seat-decompose
###################################################
dd <- log(UKDriverDeaths)
plot(decompose(dd))


###################################################
### code chunk number 35: seat-decompose-plot
###################################################
plot(decompose(dd))


###################################################
### code chunk number 36: seat-dynlm
###################################################
dynlm(dd ~ L(dd) + L(dd, 12))


###################################################
### code chunk number 37: seat-dynlm-by-hand
###################################################
dd_dat <- ts.intersect(dd, dd1 = lag(dd, k = -1),
  dd12 = lag(dd, k = -12))
lm(dd ~ dd1 + dd12, data = dd_dat)


###################################################
### code chunk number 38: seat-efp
###################################################
dd_ocus <- efp(dd ~ dd1 + dd12, data = dd_dat,
  type = "OLS-CUSUM")
sctest(dd_ocus)


###################################################
### code chunk number 39: seat-efp-plot
###################################################
plot(dd_ocus)


###################################################
### code chunk number 40: seat-Fstats
###################################################
dd_fs <- Fstats(dd ~ dd1 + dd12, data = dd_dat, from = 0.1)
sctest(dd_fs)


###################################################
### code chunk number 41: seat-supF (eval = FALSE)
###################################################
## plot(dd_fs, main = "supF test")


###################################################
### code chunk number 42: seat-supF-plot
###################################################
plot(dd_fs, main = "supF test")


###################################################
### code chunk number 43: seat-dating (eval = FALSE)
###################################################
## dd_bp <- breakpoints(dd ~ dd1 + dd12, data = dd_dat, h = 0.1)


###################################################
### code chunk number 44: seat-dating1
###################################################
if(file.exists("Ch11-TimeSeriesRegression-breakpoints-UKDriverDeaths.rda")) {
load("Ch11-TimeSeriesRegression-breakpoints-UKDriverDeaths.rda")
} else {
dd_bp <- breakpoints(dd ~ dd1 + dd12, data = dd_dat, h = 0.1)
save(dd_bp, file = "Ch11-TimeSeriesRegression-breakpoints-UKDriverDeaths.rda")
}


###################################################
### code chunk number 45: seat-dating-coef
###################################################
coef(dd_bp, breaks = 2)


###################################################
### code chunk number 46: seat-breakpoints (eval = FALSE)
###################################################
## plot(dd_bp, legend = FALSE, main = "")


###################################################
### code chunk number 47: seat-fitted (eval = FALSE)
###################################################
## plot(dd)
## lines(fitted(dd_bp, breaks = 2), col = 4)
## lines(confint(dd_bp, breaks = 2))


###################################################
### code chunk number 48: seat-breakpoints-plot
###################################################
plot(dd_bp, legend = FALSE, main = "")


###################################################
### code chunk number 49: seat-fitted-plot
###################################################
plot(dd)
lines(fitted(dd_bp, breaks = 2), col = 4)
lines(confint(dd_bp, breaks = 2))


