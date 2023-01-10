### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch04-StationarySeries.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
set.seed(1071)


###################################################
### code chunk number 2: ma1-ac1
###################################################
theta <- -10:10/10
plot(theta, -theta / (1 + theta^2), type = "l",
  xlab = expression(theta), ylab = expression(-theta / (1 + theta^2)), main = expression(rho[1]))


###################################################
### code chunk number 3: arma-plot-functions
###################################################
plot_proc <- function(ar = numeric(0), ma = numeric(0), n = 120,
  ylab = expression(Y[t]), ylim = c(-4.7, 4.7), ...)
{
  x <- arima.sim(model = list(ar = ar, ma = ma), n = n)
  plot(x, ylab = ylab, ylim = ylim, ...)
  abline(h = 0, col = "lightgray")
  invisible(x)
}

plot_acf <- function(ar = numeric(0), ma = numeric(0), lag.max = 12, pacf = FALSE,
  ylim = NULL, pch = 19, xlab = "Lag", ylab = expression(rho[k]), ...)
{
  acf_sim <- ARMAacf(ar = ar, ma = ma, lag.max = lag.max, pacf = pacf)
  if(is.null(ylim)) ylim <- if(all(acf_sim >= 0)) c(0, 1) else c(-1, 1)
  plot(0:lag.max, acf_sim, ylim = ylim, pch = pch, xlab = xlab, ylab = ylab, ...)
  lines(0:lag.max, acf_sim, type = "h")
  abline(h = 0)
  invisible(acf_sim)
}

plot_lag <- function(y, lag = 1, xlab = expression(Y[t]), ylab = NULL, ...)
{
  if(is.null(ylab)) ylab <- parse(text = paste("Y[t-", lag, "]", sep = ""))
  plot(y, lag(y, -lag), xy.labels = FALSE, xy.lines = FALSE,
    xlab = xlab, ylab = ylab, ...)
}


###################################################
### code chunk number 4: ma1-sim
###################################################
par(mfrow = c(1, 2))
set.seed(1)
y <- plot_proc(ma = 0.9)
plot_acf(ma = 0.9)
legend("topright", expression(theta == -0.9), bty = "n")


###################################################
### code chunk number 5: ma1-sim1
###################################################
par(mfrow = c(1, 2))
plot_lag(y, 1)
plot_lag(y, 2)


###################################################
### code chunk number 6: ma1b-sim
###################################################
par(mfrow = c(1, 2))
set.seed(1)
y <- plot_proc(ma = -0.9)
plot_acf(ma = -0.9)
legend("topright", expression(theta == 0.9), bty = "n")


###################################################
### code chunk number 7: ma1b-sim1
###################################################
par(mfrow = c(1, 2))
plot_lag(y, 1)
plot_lag(y, 2)


###################################################
### code chunk number 8: ma2-sim
###################################################
par(mfrow = c(1, 2))
set.seed(2)
y <- plot_proc(ma = -c(1, -0.6))
plot_acf(ma = -c(1, -0.6))
legend("topright", expression(theta[1] == 1, theta[2] == -0.6), bty = "n")


###################################################
### code chunk number 9: ma2-sim1
###################################################
par(mfrow = c(1, 2))
plot_lag(y, 1)
plot_lag(y, 2)


###################################################
### code chunk number 10: ar1a-sim1
###################################################
par(mfrow = c(1, 2))
set.seed(0)
y <- plot_proc(ar = 0.9)
plot_acf(ar = 0.9)
legend("topright", expression(phi == 0.9), bty = "n")


###################################################
### code chunk number 11: ar1a-sim2
###################################################
par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))
plot_lag(y, 1)
plot_lag(y, 2)
plot_lag(y, 3)
plot_lag(y, 4)


###################################################
### code chunk number 12: ar1b-sim1
###################################################
par(mfrow = c(1, 2))
set.seed(2)
y <- plot_proc(ar = 0.7)
plot_acf(ar = 0.7)
legend("topright", expression(phi == 0.7), bty = "n")


###################################################
### code chunk number 13: ar1c-sim1
###################################################
par(mfrow = c(1, 2))
set.seed(2)
y <- plot_proc(ar = 0.4)
plot_acf(ar = 0.4)
legend("topright", expression(phi == 0.4), bty = "n")


###################################################
### code chunk number 14: ar1d-sim1
###################################################
par(mfrow = c(1, 2))
set.seed(2)
y <- plot_proc(ar = -0.5)
plot_acf(ar = -0.5)
legend("topright", expression(phi == -0.5), bty = "n")


###################################################
### code chunk number 15: ar1e-sim1
###################################################
par(mfrow = c(1, 2))
set.seed(2)
y <- plot_proc(ar = -0.8)
plot_acf(ar = -0.8)
legend("topright", expression(phi == -0.8), bty = "n")


###################################################
### code chunk number 16: ar2-roots
###################################################
plot(0, 0, xlab = expression(phi[1]), ylab = expression(phi[2]), xlim = c(-2, 2), ylim = c(-1, 1), type = "n")
phi1 <- -20:20/10
polygon(c(-2, 0, 2, 0), c(-1, 1, -1, -1), col = gray(0.85))
polygon(phi1, -phi1^2/4, col = gray(0.72))
lines(phi1, -phi1^2/4)
lines(phi1, ifelse(phi1 < 0, 1 + phi1, 1 - phi1))
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
text(0, 0.4, "real roots")
text(0, -0.5, "complex roots")
text(1.5, 0.7, expression(phi[1]^2 + 4 * phi[2] == 0))
arrows(1.5, 0.65, 0.8, -0.8^2 /4 + 0.05, length = 0.1)


###################################################
### code chunk number 17: ar2a-sim1
###################################################
par(mfrow = c(1, 2))
set.seed(1)
y <- plot_proc(ar = c(0.5, 0.25))
plot_acf(ar = c(0.5, 0.25))
legend("topright", expression(phi[1] == 0.5, phi[2] == 0.25), bty = "n")


###################################################
### code chunk number 18: ar2b-sim1
###################################################
par(mfrow = c(1, 2))
set.seed(1)
y <- plot_proc(ar = c(1, -0.25))
plot_acf(ar = c(1, -0.25))
legend("topright", expression(phi[1] == 1.0, phi[2] == -0.25), bty = "n")


###################################################
### code chunk number 19: ar2c-sim1
###################################################
par(mfrow = c(1, 2))
set.seed(1070)
y <- plot_proc(ar = c(1.5, -0.75))
plot_acf(ar = c(1.5, -0.75))
legend("topright", expression(phi[1] == 1.5, phi[2] == -0.75), bty = "n")


###################################################
### code chunk number 20: ar2d-sim1
###################################################
par(mfrow = c(1, 2))
set.seed(1)
y <- plot_proc(ar = c(1, -0.6))
plot_acf(ar = c(1, -0.6))
legend("topright", expression(phi[1] == 1.0, phi[2] == -0.6), bty = "n")


