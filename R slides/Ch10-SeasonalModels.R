### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch10-SeasonalModels.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
library("tseries")
library("dynlm")
set.seed(1071)


###################################################
### code chunk number 2: AirPassengers-data
###################################################
data("AirPassengers", package = "datasets")
ap <- log(AirPassengers)
ap_lm <- dynlm(ap ~ trend(ap) + season(ap))


###################################################
### code chunk number 3: AirPassengers-plot
###################################################
plot(merge(as.zoo(ap), fitted(ap_lm), residuals(ap_lm), zoo(0, index(ap))),
  screen = c(1, 1, 2, 2), col = c(1, 4, 1, 4),
  xlab = "Time", ylab = c("observed/fitted", "residuals"), main = "")


###################################################
### code chunk number 4: AirPassengers-acf
###################################################
par(mfrow = c(1, 2))
acf(residuals(ap_lm))
pacf(residuals(ap_lm))


###################################################
### code chunk number 5: AirPassengers-noise
###################################################
Box.test(residuals(ap_lm), type = "Ljung-Box")
kpss.test(residuals(ap_lm))


###################################################
### code chunk number 6: acf-SARIMA001001
###################################################
par(mfrow = c(1, 2))
acf_sma <- function(ma = 0, sma = 0, ...) {
  acf <- c(ma / (1 + ma^2), rep(0, 10), sma / (1 + sma^2), 0)
  acf[c(11, 13)] <- acf[1] * acf[12]
  plot(acf, type = "h", xlab = "Lag k", ylab = expression(rho[k]), ...)
  points(acf, pch = 19)
  abline(h = 0)
  invisible(acf)
}
acf_sma(0.5, 0.8, ylim = c(-0.5, 0.5))
legend("topleft", expression(list(theta == -0.5, Theta == -0.8)), bty = "n")
acf_sma(-0.5, 0.8, ylim = c(-0.5, 0.5))
legend("topleft", expression(list(theta == +0.5, Theta == -0.8)), bty = "n")


###################################################
### code chunk number 7: acf-SARIMA001100
###################################################
par(mfrow = c(1, 2))
acf_sar <- function(ma = 0, sar = 0, ...) {
  acf <- rep(0, 61)
  acf[0:4 * 12 + 12] <- sar^(1:5)
  acf[0:4 * 12 + 11] <- sar^(1:5) * (ma / (1 + ma^2))
  acf[0:5 * 12 + 1] <- sar^(0:5) * (ma / (1 + ma^2))
  plot(acf, type = "h", xlab = "Lag k", ylab = expression(rho[k]), ...)
  points(acf, pch = 19)
  abline(h = 0)
  invisible(acf)
}
acf_sar(0.4, 0.75, ylim = c(-0.4, 0.8))
legend("topright", expression(list(Phi == 0.75, theta == -0.4)), bty = "n")
acf_sar(-0.4, 0.75, ylim = c(-0.4, 0.8))
legend("topright", expression(list(Phi == 0.75, theta == 0.4)), bty = "n")


###################################################
### code chunk number 8: AirPassengers-plot4
###################################################
plot(ts.union(ap, diff(ap), diff(ap, 12), diff(diff(ap, 12))), main = "")


###################################################
### code chunk number 9: AirPassengers-orig-acf
###################################################
par(mfrow = c(1, 2))
acf(ap, lag = 50)
pacf(ap, lag = 50)


###################################################
### code chunk number 10: AirPassengers-diff-acf
###################################################
par(mfrow = c(1, 2))
acf(diff(ap), lag = 50)
pacf(diff(ap), lag = 50)


###################################################
### code chunk number 11: AirPassengers-diff12-acf
###################################################
par(mfrow = c(1, 2))
acf(diff(ap, 12), lag = 50)
pacf(diff(ap, 12), lag = 50)


###################################################
### code chunk number 12: AirPassengers-diffdiff12-acf
###################################################
par(mfrow = c(1, 2))
acf(diff(diff(ap, 12)), lag = 50)
pacf(diff(diff(ap, 12)), lag = 50)


###################################################
### code chunk number 13: AirPassengers-arima
###################################################
fit <- arima(ap, c(0, 1, 1), seasonal = list(order = c(0, 1 ,1)))
fit


###################################################
### code chunk number 14: forecast
###################################################
library("forecast")


###################################################
### code chunk number 15: AirPassengers-arima2
###################################################
library("forecast")
fit <- Arima(ap, c(0, 1, 1), seasonal = list(order = c(0, 1 ,1)))
fit


###################################################
### code chunk number 16: AirPassengers-resid
###################################################
par(mfrow = c(1, 2))
fit_res <- residuals(fit)/sqrt(fit$sigma)
hist(fit_res)
qqnorm(fit_res)
qqline(fit_res)


###################################################
### code chunk number 17: AirPassengers-tsdiag
###################################################
tsdiag(fit)


###################################################
### code chunk number 18: AirPassengers-forecast
###################################################
plot(forecast(fit, h = 24), shadecols = gray(c(0.8, 0.6)))


