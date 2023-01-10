### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch02-FundamentalConcepts.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
set.seed(1071)


###################################################
### code chunk number 2: rwalk-simulation (eval = FALSE)
###################################################
## e <- rnorm(60)
## y <- cumsum(c(0, e))
## y <- ts(y, start = 0)
## plot(y, type = "o")


###################################################
### code chunk number 3: rwalk
###################################################
data("rwalk", package = "TSA")
plot(rwalk, ylab = "Simulated random walk", type = "n")
abline(h = 0, col = "lightgray")
lines(rwalk, type = "o")


###################################################
### code chunk number 4: ma-sim (eval = FALSE)
###################################################
## e <- rnorm(61)
## y <- rep(0, 60)
## for(i in 1:60) y[i] <- 0.5 * e[i] + 0.5 * e[i+1]
## y <- ts(y, start = 1)


###################################################
### code chunk number 5: ma-sim2 (eval = FALSE)
###################################################
## y <- filter(rnorm(61), c(0.5, 0.5))


###################################################
### code chunk number 6: ma-plot1
###################################################
set.seed(2)
e <- ts(rnorm(61), start = 0)
y <- filter(e, c(0.5, 0.5), sides = 1)
plot(e, type = "n", ylab = "e", main = "Standard normal error (i.i.d.)")
abline(h = 0, col = "lightgray")
lines(e, type = "o")


###################################################
### code chunk number 7: ma-plot2
###################################################
plot(e, type = "n", ylab = "y", main = "Moving average")
abline(h = 0, col = "lightgray")
lines(y, type = "o")


