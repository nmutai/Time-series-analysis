### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch08-ModelDiagnostics.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
set.seed(1071)


###################################################
### code chunk number 2: oil-fit
###################################################
data("oil.price", package = "TSA")
oil <- diff(log(oil.price))
oil_ma1 <- arima(oil, order = c(0, 0, 1), include.mean = FALSE)  
oil_ma1


###################################################
### code chunk number 3: oil-residuals
###################################################
oil_res <- residuals(oil_ma1) / sqrt(oil_ma1$sigma2) 


###################################################
### code chunk number 4: oil-plot0a (eval = FALSE)
###################################################
## plot(residuals(oil_ma1))
## abline(h = 0, col = "slategray")


###################################################
### code chunk number 5: oil-plot0b (eval = FALSE)
###################################################
## oil_res <- residuals(oil_ma1) / sqrt(oil_ma1$sigma2) 
## plot(oil_res)
## abline(h = 0, col = "slategray")


###################################################
### code chunk number 6: oil-plot0c (eval = FALSE)
###################################################
## plot(oil_res, type = "h")
## abline(h = 0, col = "slategray")


###################################################
### code chunk number 7: oil-plot1
###################################################
plot(residuals(oil_ma1))
abline(h = 0, col = "slategray")


###################################################
### code chunk number 8: oil-plot2
###################################################
oil_res <- residuals(oil_ma1) / sqrt(oil_ma1$sigma2) 
plot(oil_res)
abline(h = 0, col = "slategray")


###################################################
### code chunk number 9: oil-plot3
###################################################
plot(oil_res, type = "h")
abline(h = 0, col = "slategray")


###################################################
### code chunk number 10: oil-plot4a (eval = FALSE)
###################################################
## oil_res <- residuals(oil_ma1) / sqrt(oil_ma1$sigma2) 
## hist(oil_res, breaks = 20)


###################################################
### code chunk number 11: oil-plot5a (eval = FALSE)
###################################################
## qqnorm(oil_res)
## qqline(oil_res)


###################################################
### code chunk number 12: oil-plot4
###################################################
oil_res <- residuals(oil_ma1) / sqrt(oil_ma1$sigma2) 
hist(oil_res, breaks = 20)


###################################################
### code chunk number 13: oil-plot5
###################################################
qqnorm(oil_res)
qqline(oil_res)


###################################################
### code chunk number 14: oil-plot6
###################################################
acf(oil_res)


###################################################
### code chunk number 15: ar1-example0a
###################################################
nvark <- function(k = 1, phi = 0)
  1 - (1 - phi^2) * phi^(2 * k - 2)


###################################################
### code chunk number 16: ar1-example0b
###################################################
cor1k <- function(k = 1, phi = 0) - sign(phi) * 
  ((1 - phi^2) * phi^(k - 2)) / nvark(phi = phi, k = k)


###################################################
### code chunk number 17: myapply
###################################################
myapply <- function(FUN, lag = 1:9,
  ar = c(0.3, 0.5, 0.7, 0.9), digits = 2)
{
  rval <- outer(lag, ar, FUN)
  dimnames(rval) <- list(lag, ar)
  if(digits > 0) rval <- round(rval, digits = digits)
  rval
}


###################################################
### code chunk number 18: ar1-example1
###################################################
myapply(function(x, y) sqrt(nvark(x, y)))
myapply(cor1k, lag = 2:9)


###################################################
### code chunk number 19: ar1-plot1
###################################################
matplot(myapply(function(x, y) sqrt(nvark(x, y)), digits = 0), type = "b", pch = 1)


###################################################
### code chunk number 20: ar1-plot2
###################################################
matplot(myapply(cor1k, lag = 2:9, digits = 0), type = "b", pch = 1)


###################################################
### code chunk number 21: oil-box1
###################################################
Box.test(oil_res, lag = 5, fitdf = 1)
Box.test(oil_res, lag = 5, fitdf = 1, type = "Ljung-Box")


###################################################
### code chunk number 22: oil-box2
###################################################
n <- length(oil)
n * sum(acf(oil_res, plot = FALSE)$acf[2:6]^2)
n * (n + 2) * sum(acf(oil_res, plot = FALSE)$acf[2:6]^2/(n - 1:5))


###################################################
### code chunk number 23: oil-tsdiag
###################################################
tsdiag(oil_ma1)


###################################################
### code chunk number 24: myname
###################################################
myname <- function(x) x$call


###################################################
### code chunk number 25: oil-overfit0
###################################################
oil_arma1 <- arima(oil, order = c(0, 0, 2), include.mean = FALSE)
oil_arma2 <- arima(oil, order = c(1, 0, 1), include.mean = FALSE)
oil_arma3 <- arima(oil, order = c(0, 0, 1), include.mean = TRUE)
oil_arma4 <- arima(oil, order = c(1, 0, 2), include.mean = TRUE)


###################################################
### code chunk number 26: oil-overfit1
###################################################
oil_arma1
coeftest(oil_arma1)


###################################################
### code chunk number 27: oil-overfit1a
###################################################
lrtest(oil_ma1, oil_arma1, name = myname)


###################################################
### code chunk number 28: oil-overfit2
###################################################
oil_arma2
coeftest(oil_arma2)


###################################################
### code chunk number 29: oil-overfit2a
###################################################
lrtest(oil_ma1, oil_arma2, name = myname)


###################################################
### code chunk number 30: oil-overfit3
###################################################
oil_arma3
coeftest(oil_arma3)


###################################################
### code chunk number 31: oil-overfit3a
###################################################
lrtest(oil_ma1, oil_arma3, name = myname)


###################################################
### code chunk number 32: oil-overfit4
###################################################
oil_arma4
coeftest(oil_arma4)


###################################################
### code chunk number 33: oil-overfit4a
###################################################
lrtest(oil_ma1, oil_arma4, name = myname)


