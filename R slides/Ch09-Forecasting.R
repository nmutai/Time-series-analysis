### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch09-Forecasting.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
library("dynlm")
library("forecast")
set.seed(1071)


###################################################
### code chunk number 2: airline-fit
###################################################
data("AirPassengers", package = "datasets")
ap <- log(AirPassengers)
ap_lm <- dynlm(ap ~ trend(ap) + season(ap))


###################################################
### code chunk number 3: airline-predict
###################################################
cf <- coef(ap_lm)
ap_pred <- cf[1] + cf[2] * (length(ap) + 1:12)/12 + c(0, cf[3:13])
ap_pred <- ts(exp(ap_pred), start = 1961, freq = 12)


###################################################
### code chunk number 4: airline-plot
###################################################
plot(AirPassengers, xlim = c(1949, 1962), ylim = c(100, 700))
lines(ap_pred, col = 4)


###################################################
### code chunk number 5: airline-plot1
###################################################
plot(AirPassengers, xlim = c(1949, 1962), ylim = c(100, 700))
lines(ap_pred, col = 4)


###################################################
### code chunk number 6: gdp-data
###################################################
data("USMacroG", package = "AER")
gdp <- USMacroG[, "gdp"]


###################################################
### code chunk number 7: gdp-plot (eval = FALSE)
###################################################
## plot(ts.union(levels = gdp, logs = log(gdp),
##   growth = diff(log(gdp))), main = "")


###################################################
### code chunk number 8: gdp-acf (eval = FALSE)
###################################################
## acf(diff(log(gdp)))
## pacf(diff(log(gdp)))


###################################################
### code chunk number 9: gdp-plot1
###################################################
plot(ts.union(levels = gdp, logs = log(gdp),
  growth = diff(log(gdp))), main = "")


###################################################
### code chunk number 10: gdp-acf1
###################################################
par(mfrow = c(1, 2))
acf(diff(log(gdp)))
pacf(diff(log(gdp)))


###################################################
### code chunk number 11: gdp-fit
###################################################
gdp <- 100 * diff(log(gdp))
gdp_ar1 <- arima(gdp, c(1, 0, 0))
gdp_ar1


###################################################
### code chunk number 12: gdp-pred
###################################################
cf <- coef(gdp_ar1)
cf[2] + cf[1]^(1:4) * (gdp[length(gdp)] - cf[2])
pred <- predict(gdp_ar1, n.ahead = 4)
pred


###################################################
### code chunk number 13: gdp-predplot (eval = FALSE)
###################################################
## plot(gdp, xlim = c(1991, 2001.75))
## abline(h = cf[2], col = "slategray")
## lines(pred$pred, col = 4)
## lines(pred$pred + qnorm(0.025) * pred$se, col = 4, lty = 2)
## lines(pred$pred + qnorm(0.975) * pred$se, col = 4, lty = 2)


###################################################
### code chunk number 14: gdp-predplot1
###################################################
plot(gdp, xlim = c(1991, 2001.75))
abline(h = cf[2], col = "slategray")
lines(pred$pred, col = 4)
lines(pred$pred + qnorm(0.025) * pred$se, col = 4, lty = 2)
lines(pred$pred + qnorm(0.975) * pred$se, col = 4, lty = 2)


###################################################
### code chunk number 15: gdp-forecast
###################################################
library("forecast")
gdp_fc <- forecast(gdp_ar1, h = 4)
gdp_fc


###################################################
### code chunk number 16: gdp-forecast (eval = FALSE)
###################################################
## plot(gdp_fc, shaded = FALSE)
## plot(gdp_fc, shadecols = gray(c(0.8, 0.6)))


###################################################
### code chunk number 17: gdp-fcplot1
###################################################
plot(gdp_fc, xlim = c(1991, 2001.75), shaded = FALSE)


###################################################
### code chunk number 18: gdp-fcplot2
###################################################
plot(gdp_fc, xlim = c(1991, 2001.75), shadecols = gray(c(0.8, 0.6)))


###################################################
### code chunk number 19: oil-ma0
###################################################
data("oil.price", package = "TSA")
oil <- diff(log(oil.price))
oil_ma1 <- arima(oil, order = c(0, 0, 1), include.mean = FALSE)


###################################################
### code chunk number 20: oil-ma1
###################################################
data("oil.price", package = "TSA")
oil <- diff(log(oil.price))
oil_ma1 <- arima(oil, order = c(0, 0, 1), include.mean = FALSE)
predict(oil_ma1, n.ahead = 6)


###################################################
### code chunk number 21: oil-plot1
###################################################
plot(oil, xlim = c(2003, 2007))
abline(h = 0, col = "slategray")
oil_pred <- predict(oil_ma1, n.ahead = 12)
lines(oil_pred$pred, col = 4)
lines(oil_pred$pred + qnorm(0.025) * oil_pred$se, col = 4, lty = 2)
lines(oil_pred$pred + qnorm(0.975) * oil_pred$se, col = 4, lty = 2)


###################################################
### code chunk number 22: oil-plot2
###################################################
plot(forecast(oil_ma1, h = 12), xlim = c(2003, 2007), shadecols = gray(c(0.8, 0.6)))


