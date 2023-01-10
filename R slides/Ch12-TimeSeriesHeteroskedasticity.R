### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch12-TimeSeriesHeteroskedasticity.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
library("tseries")
library("fGarch")
library("e1071")
set.seed(1071)


###################################################
### code chunk number 2: CREF1
###################################################
data("CREF", package = "TSA")
tsp(CREF)
CREF[1:5]


###################################################
### code chunk number 3: CREF2
###################################################
load("CREF2.rda")
CREF2[1:5]


###################################################
### code chunk number 4: CREF1-plot
###################################################
plot(CREF)


###################################################
### code chunk number 5: CREF2-plot
###################################################
plot(CREF2)


###################################################
### code chunk number 6: CREF3
###################################################
identical(CREF, ts(coredata(CREF2)))


###################################################
### code chunk number 7: CREF4
###################################################
start(CREF2)
as.numeric(start(CREF2))
as.POSIXct(start(CREF2))
as.POSIXlt(start(CREF2))
as.POSIXlt(start(CREF2))$wday


###################################################
### code chunk number 8: CREF5
###################################################
CREF2a <- as.ts(CREF2)
tsp(CREF2a)
CREF2a[1:20]
as.Date(12656)
as.Date(13375)


###################################################
### code chunk number 9: CREF6
###################################################
rcref  <- 100 * diff(log(CREF))
tsp(rcref)
rcref[1:5]
rcref2 <- 100 * diff(log(CREF2))
rcref2[1:5]


###################################################
### code chunk number 10: rcref1-plot
###################################################
plot(rcref)


###################################################
### code chunk number 11: rcref2-plot
###################################################
plot(rcref2)


###################################################
### code chunk number 12: rcref-acf
###################################################
par(mfrow = c(1, 2))
acf(rcref)
pacf(rcref, ylim = c(-0.09, 0.19))


###################################################
### code chunk number 13: rcref-acf-abs
###################################################
par(mfrow = c(1, 2))
acf(abs(rcref))
pacf(abs(rcref), ylim = c(-0.09, 0.19))


###################################################
### code chunk number 14: rcref-acf-sq
###################################################
par(mfrow = c(1, 2))
acf(rcref^2)
pacf(rcref^2, ylim = c(-0.09, 0.19))


###################################################
### code chunk number 15: mcleod-li (eval = FALSE)
###################################################
## plot(1:26, sapply(1:26, function(i)
##   Box.test(rcref^2, lag = i, type = "Ljung-Box")$p.value),
##   xlab = "Lag", ylab = "P-value", ylim = c(0, 1))
## abline(h = 0.05, lty = 2)


###################################################
### code chunk number 16: mcleod-li-plot
###################################################
plot(1:26, sapply(1:26, function(i)
  Box.test(rcref^2, lag = i, type = "Ljung-Box")$p.value),
  xlab = "Lag", ylab = "P-value", ylim = c(0, 1))
abline(h = 0.05, lty = 2)


###################################################
### code chunk number 17: rcref-skewness
###################################################
library("e1071")
skewness(rcref, type = 1)
kurtosis(rcref, type = 1)


###################################################
### code chunk number 18: rcref-jb
###################################################
length(rcref) * c(skewness(rcref, type = 1)^2/6,
  kurtosis(rcref, type = 1)^2/24)
jarque.bera.test(rcref)


###################################################
### code chunk number 19: qq-hist
###################################################
par(mfrow = c(1, 2))
hist(rcref)
qqnorm(rcref)
qqline(rcref)


###################################################
### code chunk number 20: rdax
###################################################
data("EuStockMarkets", package = "datasets")
rdax <- 100 * diff(log(EuStockMarkets[, "DAX"]))
coeftest(lm(rdax ~ 1))
Box.test(rdax^2)
length(rdax) * c(skewness(rdax, type = 1)^2/6,
  kurtosis(rdax, type = 1)^2/24)


###################################################
### code chunk number 21: DAX
###################################################
png("Ch12-TimeSeriesHeteroskedasticity-DAX.png", bg = "transparent", height = 6 * 150, width = 8 * 150, res = 144)
plot(merge(as.zoo(EuStockMarkets[, "DAX"]), 100 * diff(log(EuStockMarkets[, "DAX"]))),
  main = "DAX", xlab = "Time", ylab = c("prices", "returns"))
dev.off()


###################################################
### code chunk number 22: rdax-acf
###################################################
par(mfrow = c(1, 2))
acf(rdax)
acf(rdax^2)


###################################################
### code chunk number 23: rdax-qq-hist
###################################################
par(mfrow = c(1, 2))
hist(rdax)
qqnorm(rdax)
qqline(rdax)


###################################################
### code chunk number 24: ARCH1-sim1 (eval = FALSE)
###################################################
## load("garch-sim.rda")
## plot(g01)


###################################################
### code chunk number 25: ARCH1-plot1
###################################################
load("garch-sim.rda")
plot(g01)


###################################################
### code chunk number 26: ARCH1-acf1
###################################################
par(mfrow = c(1, 2))
acf(g01)
pacf(g01, ylim = c(-0.12, 0.52))


###################################################
### code chunk number 27: ARCH1-acf2
###################################################
par(mfrow = c(1, 2))
acf(g01^2)
pacf(g01^2, ylim = c(-0.12, 0.52))


###################################################
### code chunk number 28: ARCH1-sim2
###################################################
set.seed(1)
x <- garchSpec(model = list(omega = 0.01, alpha = 0.9, beta = 0))
x
x <- garchSim(x, n = 500)
x <- ts(as.vector(coredata(as.zoo(x))))


###################################################
### code chunk number 29: ARCH1-plot2
###################################################
plot(x)


###################################################
### code chunk number 30: GARCH11-plot1
###################################################
plot(g11)


###################################################
### code chunk number 31: GARCH11-acf1
###################################################
par(mfrow = c(1, 2))
acf(g11)
pacf(g11, ylim = c(-0.11, 0.15))


###################################################
### code chunk number 32: GARCH11-acf2
###################################################
par(mfrow = c(1, 2))
acf(g11^2)
pacf(g11^2, ylim = c(-0.11, 0.15))


###################################################
### code chunk number 33: ARIMA-GARCH-fit
###################################################
arma11 <- arima(g11^2, order = c(1, 0, 1))


###################################################
### code chunk number 34: ARIMA-GARCH-fit
###################################################
arma11
b <- - coef(arma11)[2]
a <- coef(arma11)[1] - b
o <- coef(arma11)[3] * (1 - a - b)
par <- c(o, a, b)
names(par) <- c("omega", "alpha", "beta")
par


###################################################
### code chunk number 35: g11-garch1
###################################################
g11_g1 <- garch(g11, order = c(1, 1), trace = FALSE)
g11_g2 <- garch(g11, order = c(2, 2), trace = FALSE)
g11_gf1 <- garchFit(~ garch(1,1), data = g11, trace = FALSE,
  include.mean = FALSE)
g11_gf2 <- garchFit(~ garch(2,2), data = g11, trace = FALSE,
  include.mean = FALSE)
sapply(list(g11_gf1, g11_g1), coef)
sapply(list(g11_gf2, g11_g2), coef)


###################################################
### code chunk number 36: g11-garch2a (eval = FALSE)
###################################################
## summary(g11_g1)


###################################################
### code chunk number 37: g11-garch2b
###################################################
out <- capture.output(summary(g11_g1))
writeLines(out[1:18])


###################################################
### code chunk number 38: g11-garch2c
###################################################
writeLines(out[-(1:18)])


###################################################
### code chunk number 39: g11-garch3a (eval = FALSE)
###################################################
## summary(g11_g2)


###################################################
### code chunk number 40: g11-garch3b
###################################################
out <- capture.output(summary(g11_g2))
writeLines(out[1:10])


###################################################
### code chunk number 41: g11-garch3c
###################################################
writeLines(out[-(1:10)])


###################################################
### code chunk number 42: g11-garch4
###################################################
logLik(g11_g1)
logLik(g11_g2)
AIC(g11_g1, g11_g2)


###################################################
### code chunk number 43: g11-garch5a (eval = FALSE)
###################################################
## summary(g11_gf1)


###################################################
### code chunk number 44: g11-garch5b
###################################################
out <- capture.output(summary(g11_gf1))
writeLines(out[1:8])


###################################################
### code chunk number 45: g11-garch5c
###################################################
writeLines(out[9:28])


###################################################
### code chunk number 46: g11-garch5c
###################################################
writeLines(out[-(c(1:28, 35))])


###################################################
### code chunk number 47: garch11-diag
###################################################
g11_res <- na.omit(residuals(g11_g1))
c(skewness(g11_res), kurtosis(g11_res))
jarque.bera.test(g11_res)
Box.test(g11_res^2, type = "Ljung-Box")


###################################################
### code chunk number 48: cref-garch1a
###################################################
par <- expand.grid(p = 0:2, q = 1:2)


###################################################
### code chunk number 49: cref-garch1b
###################################################
fm <- lapply(1:nrow(par), function(i)
  garch(rcref, order = c(par$p[i], par$q[i]), trace = FALSE))


###################################################
### code chunk number 50: cref-garch1c
###################################################
par <- cbind(par,
  logLik = sapply(fm, logLik),
  AIC = sapply(fm, AIC),
  BIC = sapply(fm, AIC, k = log(length(rcref)))
)


###################################################
### code chunk number 51: cref-garch2a
###################################################
garch_formula <- function(p, q)
  as.formula(sprintf("~ garch(%f, %f)", q, p))


###################################################
### code chunk number 52: cref-garch2b
###################################################
logLik.fGARCH <- function(object, ...)
  structure(-object@fit$llh, df = length(coef(object)),
    class = "logLik")


###################################################
### code chunk number 53: cref-garch2d
###################################################
fm2 <- lapply(1:nrow(par), function(i)
  garchFit(garch_formula(par$p[i], par$q[i]),
    data = rcref, trace = FALSE))


###################################################
### code chunk number 54: cref-garch2e
###################################################
par <- cbind(par,
  logLik2 = sapply(fm2, logLik),
  AIC2 = sapply(fm2, AIC),
  BIC2 = sapply(fm2, AIC, k = log(length(rcref)))
)
round(par, digits = 1)


###################################################
### code chunk number 55: cref-garch3a (eval = FALSE)
###################################################
## cref_g11 <- fm2[[2]]
## summary(cref_g11)


###################################################
### code chunk number 56: cref-garch3b
###################################################
cref_g11 <- fm2[[2]]
out <- capture.output(summary(cref_g11))
writeLines(out[1:16])


###################################################
### code chunk number 57: cref-garch3c
###################################################
writeLines(out[17:37])


###################################################
### code chunk number 58: cref-garch3d
###################################################
writeLines(out[-(1:37)])


###################################################
### code chunk number 59: cref-garch4
###################################################
coef(cref_g11)["omega"] /
  (1 - coef(cref_g11)["alpha1"] - coef(cref_g11)["beta1"])
var(rcref)


###################################################
### code chunk number 60: cref-garch5
###################################################
cref_res <- residuals(cref_g11, standardize = TRUE)


###################################################
### code chunk number 61: rcref-resid1
###################################################
par(mfrow = c(1, 2))
hist(cref_res)
qqnorm(cref_res)
qqline(cref_res)


###################################################
### code chunk number 62: rcref-resid2
###################################################
par(mfrow = c(1, 2))
acf(cref_res)
acf(cref_res^2)


###################################################
### code chunk number 63: rcref-ci
###################################################
plot(rcref)
abline(h = coef(cref_g11)[1])
lines(coef(cref_g11)[1] + qnorm(0.025) * volatility(cref_g11), col = 2)
lines(coef(cref_g11)[1] + qnorm(0.975) * volatility(cref_g11), col = 2)


