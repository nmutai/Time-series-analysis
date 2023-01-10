### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch03-Trends.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
set.seed(1071)


###################################################
### code chunk number 2: AirPassengers-plot0 (eval = FALSE)
###################################################
## data("AirPassengers", package = "datasets")
## plot(AirPassengers)
## plot(AirPassengers, log = "y")
## plot(log(AirPassengers))


###################################################
### code chunk number 3: AirPassengers-plot1
###################################################
data("AirPassengers", package = "datasets")
plot(AirPassengers)


###################################################
### code chunk number 4: AirPassengers-plot2
###################################################
plot(AirPassengers, log = "y")


###################################################
### code chunk number 5: AirPassengers-plot3
###################################################
plot(log(AirPassengers))


###################################################
### code chunk number 6: AirPassengers-dynlm1
###################################################
library("dynlm")
ap <- log(AirPassengers)
ap_lm <- dynlm(ap ~ trend(ap) + season(ap))
summary(ap_lm)


###################################################
### code chunk number 7: AirPassengers-dynlm2
###################################################
ap_lm_out <- capture.output(summary(ap_lm))
writeLines(ap_lm_out[1:10])


###################################################
### code chunk number 8: AirPassengers-dynlm3
###################################################
writeLines(ap_lm_out[-(1:10)])


###################################################
### code chunk number 9: AirPassengers-decompose1
###################################################
ap_dec <- decompose(ap)
plot(ap_dec)


###################################################
### code chunk number 10: AirPassengers-decompose1
###################################################
plot(ap_dec)


###################################################
### code chunk number 11: AirPassengers-trend
###################################################
## colors
cols1 <- hcl(c(0, 260), 100, 50)
cols2 <- hcl(c(0, 260), 50, 75)

## season figure
ap_lm_fig <- c(0, coef(ap_lm)[3:13])
ap_lm_fig <- ap_lm_fig - mean(ap_lm_fig)

## trend comparison
plot(ap, ylab = "log(AirPassengers)", main = "Trend")
lines(ap_dec$trend, col = cols1[1], lwd = 2)
lines(fitted(ap_lm) - ap_lm_fig[cycle(ap)], col = cols1[2], lwd = 2)
legend("topleft", c("decompose", "dynlm"), col = cols1, lwd = 2, bty = "n")


###################################################
### code chunk number 12: AirPassengers-season
###################################################
names(ap_lm_fig) <- month.abb
barplot(rbind(ap_dec$figure, ap_lm_fig), beside = TRUE, col = cols2, main = "Season")
legend("topleft", c("decompose", "dynlm"), col = cols2, pch = 15, bty = "n")
legend("topleft", c("decompose", "dynlm"), pch = 0, bty = "n")


###################################################
### code chunk number 13: AirPassengers-error
###################################################
plot(residuals(ap_lm), col = cols1[2], lwd = 2, main = "Error", ylab = "")
lines(ap_dec$random, col = cols1[1], lwd = 2)
abline(h = 0)
legend("topleft", c("decompose", "dynlm"), col = cols1, lwd = 2, bty = "n")


###################################################
### code chunk number 14: AirPassengers-residuals1 (eval = FALSE)
###################################################
## plot(residuals(ap_lm))
## plot(rstudent(ap_lm))
## lines(rollapply(rstudent(ap_lm), 36, mean), col = 2)
## plot(ap_lm, which = 1)


###################################################
### code chunk number 15: AirPassengers-residuals1a
###################################################
plot(residuals(ap_lm))


###################################################
### code chunk number 16: AirPassengers-residuals1b
###################################################
plot(rstudent(ap_lm))


###################################################
### code chunk number 17: AirPassengers-residuals1c
###################################################
plot(rstudent(ap_lm))
lines(rollapply(rstudent(ap_lm), 36, mean), col = 2)


###################################################
### code chunk number 18: AirPassengers-residuals1d
###################################################
plot(ap_lm, which = 1)


###################################################
### code chunk number 19: AirPassengers-residuals2
###################################################
hist(rstudent(ap_lm))
plot(ap_lm, which = 2)
shapiro.test(residuals(ap_lm))


###################################################
### code chunk number 20: AirPassengers-residuals2a
###################################################
hist(rstudent(ap_lm))


###################################################
### code chunk number 21: AirPassengers-residuals2b
###################################################
plot(ap_lm, which = 2)


###################################################
### code chunk number 22: AirPassengers-residuals3b
###################################################
acf(residuals(ap_lm))


###################################################
### code chunk number 23: AirPassengers-residuals3c
###################################################
dwtest(ap_lm)
bgtest(ap_lm)


###################################################
### code chunk number 24: AirPassengers-residuals3d
###################################################
coeftest(ap_lm)


###################################################
### code chunk number 25: AirPassengers-residuals3e
###################################################
coeftest(ap_lm, vcov = kernHAC)


###################################################
### code chunk number 26: AirPassengers-residuals3f
###################################################
coeftest(ap_lm, vcov = NeweyWest)


