### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch05-NonstationarySeries.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
set.seed(1071)


###################################################
### code chunk number 2: oilprice
###################################################
data("oil.price", package = "TSA")
plot(oil.price, ylab = "Price per barrel")


###################################################
### code chunk number 3: oilprice2
###################################################
plot(log(oil.price), ylab = "Log-price per barrel")


###################################################
### code chunk number 4: Ch05-NonstationarySeries.Rnw:129-134
###################################################
set.seed(44)
e <- ts(rnorm(8))
y <- filter(e, 3, method = "recursive")
ts.intersect(e, y)
plot(y, type = "o")


###################################################
### code chunk number 5: ar1
###################################################
plot(y, type = "o")


###################################################
### code chunk number 6: oilprice3
###################################################
plot(diff(log(oil.price)), ylab = "Returns (log-price changes)")


###################################################
### code chunk number 7: Ch05-NonstationarySeries.Rnw:549-554
###################################################
set.seed(42)
y <- arima.sim(list(order = c(0, 2, 2), ma = c(-1, 0.6)), n = 60)
plot(y, type = "o")
plot(diff(y), type = "o")
plot(diff(y, differences = 2), type = "o")


###################################################
### code chunk number 8: ima22-sim1
###################################################
par(mfrow = c(1, 2))
plot(y, type = "o")
acf(y)


###################################################
### code chunk number 9: ima22-sim2
###################################################
par(mfrow = c(1, 2))
plot(diff(y), type = "o")
acf(diff(y))


###################################################
### code chunk number 10: ima22-sim3
###################################################
par(mfrow = c(1, 2))
plot(diff(y, differences = 2), type = "o")
acf(diff(y, differences = 2))


###################################################
### code chunk number 11: DAX
###################################################
png("Ch05-NonstationarySeries-DAX.png", bg = "transparent", height = 7 * 150, width = 7 * 150, res = 200)
plot(merge(as.zoo(EuStockMarkets[, "DAX"]), log(EuStockMarkets[, "DAX"]), diff(log(EuStockMarkets[, "DAX"]))),
  main = "", xlab = "Time", ylab = c("prices", "log-prices", "returns"))
dev.off()


