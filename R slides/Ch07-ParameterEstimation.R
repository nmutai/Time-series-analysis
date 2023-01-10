### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch07-ParameterEstimation.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
set.seed(1071)


###################################################
### code chunk number 2: Ch07-ParameterEstimation.Rnw:287-289
###################################################
set.seed(1)
ma1a <- arima.sim(model = list(ma = 0.9), n = 120)


###################################################
### code chunk number 3: Ch07-ParameterEstimation.Rnw:296-301
###################################################
ma1fit <- function(x) {
  r1 <- acf(x, plot = FALSE)$acf[2]
  if(abs(r1) > 0.5) NA else (-1 + sqrt(1 - 4 * r1^2))/(2 * r1)
}
ma1fit(ma1a)


###################################################
### code chunk number 4: Ch07-ParameterEstimation.Rnw:308-312
###################################################
set.seed(1)
ma1b <- arima.sim(model = list(ma = -0.9), n = 120)
ma1fit(ma1b)
acf(ma1b, plot = FALSE)$acf[2]


###################################################
### code chunk number 5: Ch07-ParameterEstimation.Rnw:322-324
###################################################
set.seed(0)
ar1a <- arima.sim(model = list(ar = 0.9), n = 120)


###################################################
### code chunk number 6: Ch07-ParameterEstimation.Rnw:330-332
###################################################
acf(ar1a, plot = FALSE)$acf[2]
ar(ar1a, order.max = 1, aic = FALSE, method = "yule-walker")


###################################################
### code chunk number 7: Ch07-ParameterEstimation.Rnw:342-345
###################################################
set.seed(2)
ar1b <- arima.sim(model = list(ar = -0.8), n = 120)
ar(ar1b, order.max = 1, aic = FALSE, method = "yule-walker")


###################################################
### code chunk number 8: Ch07-ParameterEstimation.Rnw:355-358
###################################################
set.seed(1)
ar2 <- arima.sim(model = list(ar = c(1, -0.6)), n = 120)
ar(ar2, order.max = 2, aic = FALSE, method = "yule-walker")


###################################################
### code chunk number 9: Ch07-ParameterEstimation.Rnw:899-912
###################################################
my_ar <- function(y, order = 1)
{
  rval <- matrix(0, nrow = 3, ncol = order + 1)
  rownames(rval) <- c("yule-walker", "ols", "mle")
  colnames(rval) <- c(paste("ar", 1:order, sep = ""), "var")

  for(i in rownames(rval)) {
    fit <- ar(y, method = i, aic = FALSE, order.max = order)
    rval[i,] <- c(fit$ar, fit$var.pred)
  }
  
  return(rval)
}


###################################################
### code chunk number 10: Ch07-ParameterEstimation.Rnw:922-923
###################################################
my_ar(ar1a)


###################################################
### code chunk number 11: Ch07-ParameterEstimation.Rnw:930-931
###################################################
arima(ar1a, order = c(1, 0, 0))


###################################################
### code chunk number 12: Ch07-ParameterEstimation.Rnw:941-943
###################################################
my_ar(ar1b)
arima(ar1b, order = c(1, 0, 0))


###################################################
### code chunk number 13: Ch07-ParameterEstimation.Rnw:953-955
###################################################
my_ar(ar2, order = 2)
arima(ar2, order = c(2, 0, 0))


###################################################
### code chunk number 14: Ch07-ParameterEstimation.Rnw:965-966
###################################################
ar(ar2, method = "ols")


###################################################
### code chunk number 15: Ch07-ParameterEstimation.Rnw:976-977
###################################################
ar(ar2, method = "mle")


###################################################
### code chunk number 16: Ch07-ParameterEstimation.Rnw:987-989
###################################################
data("oil.price", package = "TSA")
oil <- diff(log(oil.price))


###################################################
### code chunk number 17: Ch07-ParameterEstimation.Rnw:997-999
###################################################
mods <- expand.grid(ar = 0:2, ma = 0:2, mean = c(TRUE, FALSE))
mods$aic <- 0


###################################################
### code chunk number 18: Ch07-ParameterEstimation.Rnw:1006-1009
###################################################
for(i in 1:nrow(mods)) mods$aic[i] <- AIC(
  arima(oil, order = c(mods$ar[i], 0, mods$ma[i]),
    include.mean = mods$mean[i]))


###################################################
### code chunk number 19: Ch07-ParameterEstimation.Rnw:1017-1018
###################################################
mods  


###################################################
### code chunk number 20: Ch07-ParameterEstimation.Rnw:1028-1030
###################################################
mods[which.min(mods$aic),]
arima(oil, order = c(0, 0, 1), include.mean = FALSE)  


###################################################
### code chunk number 21: Ch07-ParameterEstimation.Rnw:1040-1044 (eval = FALSE)
###################################################
## library("forecast")
## auto.arima(oil, ic = "aic", stationary = TRUE,
##   max.p = 2, max.q = 2, max.P = 0, max.Q = 0,
##   stepwise = FALSE, trace = TRUE, approximation = FALSE)


###################################################
### code chunk number 22: Ch07-ParameterEstimation.Rnw:1047-1054
###################################################
library("forecast")
arima_out <- capture.output(
  auto.arima(oil, ic = "aic", stationary = TRUE,
    max.p = 2, max.q = 2, max.P = 0, max.Q = 0,
    stepwise = FALSE, trace = TRUE, approximation = FALSE)
)
writeLines(arima_out[1:15])


###################################################
### code chunk number 23: Ch07-ParameterEstimation.Rnw:1062-1063
###################################################
writeLines(arima_out[-(1:15)])


