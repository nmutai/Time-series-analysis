### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch14-Cointegration.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
library("tseries")
set.seed(1071)


###################################################
### code chunk number 2: Ch14-Cointegration.Rnw:74-79
###################################################
par(mfrow = c(1, 2))
data("PepperPrice", package = "AER")
plot(log(PepperPrice), col = 1:2, main = "Pepper Prices", ylab = "log(PepperPrice)", plot.type = "single")
plot(log(PepperPrice[, "black"]) - log(PepperPrice[, "white"]), main = "Difference", ylab = "")   
par(mfrow = c(1, 1))


###################################################
### code chunk number 3: Ch14-Cointegration.Rnw:192-197
###################################################
set.seed(4002)  
e1 <- rnorm(250) 
e2 <- rnorm(250) 
y1 <- cumsum(e1) 
y2 <- cumsum(e2)


###################################################
### code chunk number 4: Ch14-Cointegration.Rnw:200-201
###################################################
summary(lm(y1 ~ y2))


###################################################
### code chunk number 5: Ch14-Cointegration.Rnw:212-217
###################################################
par(mfrow = c(1, 2))
plot(ts(cbind(y1, y2)), lty=1:2, plot.type = "single") 
legend("topleft", c("y1", "y2"), lty=1:2, bty = "n")
plot(y1 ~ y2, pch = 20)
par(mfrow=c(1, 1))


###################################################
### code chunk number 6: Ch14-Cointegration.Rnw:270-275
###################################################
par(mfrow = c(1, 2))
data("PepperPrice")
plot(log(PepperPrice), col = 1:2, main = "Pepper Prices", ylab = "log(PepperPrice)", plot.type = "single")
plot(log(PepperPrice[, "black"]) - log(PepperPrice[, "white"]), main = "Difference", ylab = "")   
par(mfrow = c(1, 1))


###################################################
### code chunk number 7: Ch14-Cointegration.Rnw:295-298
###################################################
library("CADFtest")
CADFtest(log(PepperPrice[, "white"]) - log(PepperPrice[, "black"]),
  type = "drift", max.lag.y = 5, criterion = "AIC")


###################################################
### code chunk number 8: Ch14-Cointegration.Rnw:332-333
###################################################
po.test(log(PepperPrice))


###################################################
### code chunk number 9: Ch14-Cointegration.Rnw:350-351
###################################################
po.test(log(PepperPrice[, 2:1]))


###################################################
### code chunk number 10: Ch14-Cointegration.Rnw:771-773 (eval = FALSE)
###################################################
## library("urca")
## summary(ca.jo(log(PepperPrice), ecdet = "const", K = 2))


###################################################
### code chunk number 11: Ch14-Cointegration.Rnw:776-780
###################################################
library("urca")
out <- capture.output(summary(ca.jo(log(PepperPrice), ecdet = "const", K = 2)))
out <- gsub("without", "\n  without", out, fixed = TRUE)
writeLines(out[1:16])


###################################################
### code chunk number 12: Ch14-Cointegration.Rnw:788-789
###################################################
writeLines(out[-(1:16)])


