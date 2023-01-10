### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch13-Multivariate.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
library("tseries")
set.seed(1071)


###################################################
### code chunk number 2: PepperPrice
###################################################
data("PepperPrice", package="AER")
rpp <- diff(log(PepperPrice))
plot(rpp, main = "Pepper price returns")


###################################################
### code chunk number 3: PepperPrice-ccf
###################################################
acf(rpp)


###################################################
### code chunk number 4: ConsumerGood-data
###################################################
data("ConsumerGood", package = "AER")
cg <- ConsumerGood[, c(2, 1, 3)]


###################################################
### code chunk number 5: ConsumerGood-plot
###################################################
plot(cg)


###################################################
### code chunk number 6: ConsumerGood-VARselect
###################################################
library("vars")
VARselect(cg, 5)


###################################################
### code chunk number 7: ConsumerGood-VARselect
###################################################
cg_var1 <- VAR(cg, p = 1)


###################################################
### code chunk number 8: ConsumerGood-coef
###################################################
coef(cg_var1)


###################################################
### code chunk number 9: ConsumerGood-roots
###################################################
roots(cg_var1)


###################################################
### code chunk number 10: ConsumerGood-serial
###################################################
serial.test(cg_var1, lags.pt = 10, type = "PT.adjusted")


###################################################
### code chunk number 11: ConsumerGood-diag (eval = FALSE)
###################################################
## arch.test(cg_var1)
## normality.test(cg_var1)


###################################################
### code chunk number 12: ConsumerGood-predict
###################################################
predict(cg_var1, n.ahead = 3)
plot(predict(cg_var1))
fanchart(predict(cg_var1))


###################################################
### code chunk number 13: ConsumerGood-predictplot
###################################################
plot(predict(cg_var1))


###################################################
### code chunk number 14: ConsumerGood-fanchart
###################################################
fanchart(predict(cg_var1))


###################################################
### code chunk number 15: ConsumerGood-causality
###################################################
causality(cg_var1, cause = c("distribution", "price"))$Granger


###################################################
### code chunk number 16: ConsumerGood-irf
###################################################
plot(irf(cg_var1, impulse = "price", response = c("share", "distribution"), boot = FALSE))


###################################################
### code chunk number 17: ConsumerGood-fevd
###################################################
fevd(cg_var1, n.ahead = 2)


###################################################
### code chunk number 18: ConsumerGood-fevd
###################################################
plot(fevd(cg_var1, n.ahead = 5))


