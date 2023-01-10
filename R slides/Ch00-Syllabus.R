### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch00-Syllabus.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
library("strucchange")
set.seed(1071)


###################################################
### code chunk number 2: decompose
###################################################
data("UKDriverDeaths")
x <- decompose(log(UKDriverDeaths))
plot(cbind(
    observed = x$random + x$trend + x$seasonal,
    trend = x$trend,
    season = x$seasonal, 
    remainder = x$random),
  main = "UK driver deaths (in logs)")


###################################################
### code chunk number 3: acf_pacf
###################################################
data("UKNonDurables", package = "AER")
nd <- log(UKNonDurables)
layout(rbind(1, 2:3))
plot(nd, ylab = "log(Consumption)", main = "Consumption of Non-Durables in the UK")
acf(diff(nd), main = "ACF of returns", ylab = "")
pacf(diff(nd), main = "Partial ACF of returns", ylab = "")


###################################################
### code chunk number 4: dynlm
###################################################
data("USMacroG", package = "AER")
library("dynlm")
cons_lm1 <- dynlm(consumption ~ dpi + L(dpi), data = USMacroG)
cons_lm2 <- dynlm(consumption ~ dpi + L(consumption), 
  data = USMacroG)
plot(merge(as.zoo(USMacroG[,"consumption"]), fitted(cons_lm1),
  fitted(cons_lm2), 0, residuals(cons_lm1),
  residuals(cons_lm2)), screens = rep(1:2, c(3, 3)),
  col = rep(c(1, 2, 4), 2), ylab = c("Fitted values", "Residuals"),
  xlab = "Time", main = "US consumption functions")
legend(0.05, 0.95, c("Observed", "Distributed lag", "Autoregressive distributed lag"), 
  col = c(1, 2, 4), lty = 1, bty = "n")


###################################################
### code chunk number 5: urca
###################################################
data("PepperPrice", package = "AER")
plot(PepperPrice, plot.type = "single", col = c("black", "slategray"),
  ylab = "Average monthly spot price",
  main = "European pepper prices")
legend("topleft", c("white", "black"), bty = "n", 
  col = c("slategray", "black"), lty = rep(1,2), lwd = 2)


###################################################
### code chunk number 6: strucchange
###################################################
data("UKDriverDeaths")
dd <- log(UKDriverDeaths)
dd_dat <- ts.intersect(dd, dd1 = lag(dd, k = -1),
  dd12 = lag(dd, k = -12))
dd_bp <- breakpoints(dd ~ dd1 + dd12, data = dd_dat, h = 0.1)
plot(dd, ylab = "UK driver deaths", main = "Change in seatbelt legislation in the UK")
lines(fitted(dd_bp, breaks = 2), col = 4)
lines(confint(dd_bp, breaks = 2))
legend("bottomleft", c("observed", "fitted"), col = c(1, 4), lty = 1, lwd = 1.5, bty = "n")


###################################################
### code chunk number 7: fGarch
###################################################
data("MarkPound", package = "AER")
library("fGarch")
mp_gf <- garchFit(~ garch(1, 1), data = MarkPound, trace = FALSE)

png("Ch0-Syllabus-MarkPound.png", height = 5 * 150, width = 6 * 150, res = 144)
plot(MarkPound, col = "slategray", ylim = c(-3, 3),
  xlab = "Time", ylab = "DEM/GBP exchange rate returns",
  main = "Volatility in DEM/GBP exchange rate returns")
abline(h = coef(mp_gf)[1])
lines(coef(mp_gf)[1] + qnorm(0.975) * volatility(mp_gf))
lines(coef(mp_gf)[1] - qnorm(0.975) * volatility(mp_gf))
dev.off()


