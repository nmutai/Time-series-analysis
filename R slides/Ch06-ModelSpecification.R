### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch06-ModelSpecification.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
set.seed(1071)


###################################################
### code chunk number 2: ma1a
###################################################
set.seed(1)
ma1a <- arima.sim(model = list(ma = 0.9), n = 120)
plot(ma1a)


###################################################
### code chunk number 3: ma1a-acf
###################################################
par(mfrow = c(1, 2))
acf(ma1a, ylim = c(-0.25, 1))
pacf(ma1a)


###################################################
### code chunk number 4: ma1a-acf2
###################################################
par(mfrow = c(1, 2))
acf(ma1a, ci.type = "ma", ylim = c(-0.25, 1))
pacf(ma1a)


###################################################
### code chunk number 5: ma1b
###################################################
set.seed(1)
ma1b <- arima.sim(model = list(ma = -0.9), n = 120)
plot(ma1b)


###################################################
### code chunk number 6: ma1b-acf
###################################################
par(mfrow = c(1, 2))
acf(ma1b, ylim = c(-0.5, 1))
pacf(ma1b)


###################################################
### code chunk number 7: ma1b-acf2
###################################################
par(mfrow = c(1, 2))
acf(ma1b, ci.type = "ma", ylim = c(-0.5, 1))
pacf(ma1b)


###################################################
### code chunk number 8: ma2
###################################################
set.seed(2)
ma2 <- arima.sim(model = list(ma = -c(1, -0.6)), n = 120)
plot(ma2)


###################################################
### code chunk number 9: ma2-acf
###################################################
par(mfrow = c(1, 2))
acf(ma2, ylim = c(-0.7, 1))
pacf(ma2)


###################################################
### code chunk number 10: ma2-acf2
###################################################
par(mfrow = c(1, 2))
acf(ma2, ci.type = "ma", ylim = c(-0.7, 1))
pacf(ma2)


###################################################
### code chunk number 11: ar1a
###################################################
set.seed(0)
ar1a <- arima.sim(model = list(ar = 0.9), n = 120)
plot(ar1a)


###################################################
### code chunk number 12: ar1a-acf
###################################################
par(mfrow = c(1, 2))
acf(ar1a)
pacf(ar1a)


###################################################
### code chunk number 13: ar1b
###################################################
set.seed(2)
ar1b <- arima.sim(model = list(ar = -0.8), n = 120)
plot(ar1b)


###################################################
### code chunk number 14: ar1b-acf
###################################################
par(mfrow = c(1, 2))
acf(ar1b)
pacf(ar1b)


###################################################
### code chunk number 15: ar2
###################################################
set.seed(1)
ar2 <- arima.sim(model = list(ar = c(1, -0.6)), n = 120)
plot(ar2)


###################################################
### code chunk number 16: ar2-acf
###################################################
par(mfrow = c(1, 2))
acf(ar2)
pacf(ar2)


###################################################
### code chunk number 17: arma11
###################################################
set.seed(0)
arma11 <- arima.sim(model = list(ar = 0.6, ma = 0.3), n = 120)
plot(arma11)


###################################################
### code chunk number 18: arma11-acf
###################################################
par(mfrow = c(1, 2))
acf(arma11)
pacf(arma11)


###################################################
### code chunk number 19: oil1-acf
###################################################
data("oil.price", package = "TSA")
par(mfrow = c(1, 2))
acf(log(oil.price))
pacf(log(oil.price))


###################################################
### code chunk number 20: oil2-acf
###################################################
par(mfrow = c(1, 2))
acf(diff(log(oil.price)))
pacf(diff(log(oil.price)))


###################################################
### code chunk number 21: oil3-acf
###################################################
par(mfrow = c(1, 2))
acf(diff(log(oil.price)), ylim = c(-0.3, 1))
pacf(diff(log(oil.price)), ylim = c(-0.35, 0.25))


###################################################
### code chunk number 22: oil4-acf
###################################################
par(mfrow = c(1, 2))
acf(diff(diff(log(oil.price))), ylim = c(-0.3, 1))
pacf(diff(diff(log(oil.price))), ylim = c(-0.35, 0.25))


###################################################
### code chunk number 23: oil-data
###################################################
data("oil.price", package = "TSA")
oil <- log(oil.price)
oil.ret <- diff(oil)
plot(ts.union(exp(oil), oil, diff(oil)), main = "")


###################################################
### code chunk number 24: DAX
###################################################
png("Ch06-ModelSpecification-oil.png", bg = "transparent", height = 7 * 150, width = 7 * 150, res = 200)
plot(ts.union(exp(oil), oil, diff(oil)), main = "")
dev.off()


###################################################
### code chunk number 25: oil-adf
###################################################
library("tseries")
adf.test(oil)
adf.test(oil.ret)


###################################################
### code chunk number 26: oil-adf2
###################################################
library("CADFtest")
CADFtest(oil ~ 1, max.lag.y = 6)


###################################################
### code chunk number 27: oil-kpss1
###################################################
kpss.test(oil)


###################################################
### code chunk number 28: oil-kpss2
###################################################
kpss.test(oil.ret)


###################################################
### code chunk number 29: oil-ret-acf1
###################################################
par(mfrow = c(1, 2))
acf(oil.ret, ylim = c(-0.25, 1))
pacf(oil.ret)


###################################################
### code chunk number 30: oil-ret-acf2
###################################################
par(mfrow = c(1, 2))
acf(oil.ret, ci.type = "ma", ylim = c(-0.25, 1))
pacf(oil.ret)


###################################################
### code chunk number 31: DAX-data
###################################################
data("EuStockMarkets", package = "datasets")
dax <- log(EuStockMarkets[, "DAX"])
dax.ret <- diff(dax)
plot(ts.union(exp(dax), dax, diff(dax)), main = "")


###################################################
### code chunk number 32: DAX
###################################################
png("Ch06-ModelSpecification-DAX.png", bg = "transparent", height = 7 * 150, width = 7 * 150, res = 200)
plot(ts.union(exp(dax), dax, diff(dax)), main = "")
dev.off()


###################################################
### code chunk number 33: DAX-kpss
###################################################
kpss.test(dax)


###################################################
### code chunk number 34: DAX-adf
###################################################
adf.test(dax.ret)


###################################################
### code chunk number 35: dax-ret-acf1
###################################################
par(mfrow = c(1, 2))
acf(dax.ret)
pacf(dax.ret)


