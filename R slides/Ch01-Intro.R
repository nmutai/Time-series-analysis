### R code from vignette source '/home/zeileis/svn/projects/AER/Teaching/TimeSeriesAnalysis/Ch01-Intro.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(prompt = "R> ", continue = "+  ", width = 64,
  digits = 4, show.signif.stars = FALSE, useFancyQuotes = FALSE)
library("AER")
set.seed(1071)


###################################################
### code chunk number 2: data
###################################################
data("USMacroG", package = "AER")
data("EuStockMarkets", package = "datasets")
data("UKNonDurables", package = "AER")
data("AirPassengers", package = "datasets")
data("UKDriverDeaths", package = "datasets")
data("bev", package = "tseries")
data("MarkPound", package = "AER")
data("DutchAdvert", package = "AER")
data("PepperPrice", package = "AER")
## data("NelPlo", package = "tseries")
## plot(NelPlo[, "cpi"], main = "US Consumer Price Index", ylab = "")
## data("nile", package = "wavelets")
## tsp(nile) <- c(622, 1284, 1)
## plot(nile, main = "Nile river minima", ylab = "")


###################################################
### code chunk number 3: USMacroG
###################################################
plot(USMacroG[, c("dpi", "consumption")], col = c("black", "slategray"),
  plot.type = "single", ylab = "", main = "US macroeconomic data")
legend("topleft", legend = c("Income", "Consumption"), lty = 1, lwd = 2,
  col = c("black", "slategray"), bty = "n")


###################################################
### code chunk number 4: EuStockMarkets
###################################################
png("Ch01-Intro-EuStockMarkets.png", bg = "transparent", height = 6 * 150, width = 8 * 150, res = 144)
plot(merge(as.zoo(EuStockMarkets[, "DAX"]), diff(log(EuStockMarkets[, "DAX"]))),
  main = "DAX", xlab = "Time", ylab = c("prices", "returns"))
dev.off()


###################################################
### code chunk number 5: UKNonDurables
###################################################
plot(UKNonDurables, ylab = "", main = "Consumption of non-durables in the UK")


###################################################
### code chunk number 6: AirPassengers
###################################################
plot(AirPassengers, main = "Airline passenger totals", ylab = "")


###################################################
### code chunk number 7: UKDriverDeaths
###################################################
plot(UKDriverDeaths, ylab = "", main = "UK car drivers killed or seriously injured")


###################################################
### code chunk number 8: bev
###################################################
plot(bev, main = "Beveridge Wheat Price Index", ylab = "")


###################################################
### code chunk number 9: MarkPound
###################################################
png("Ch01-Intro-MarkPound.png", height = 5 * 150, width = 7 * 150, res = 144)
plot(MarkPound, xlab = "Time", main = "DEM/GBP exchange rate returns", ylab = "")
dev.off()


###################################################
### code chunk number 10: DutchAdvert
###################################################
plot(as.zoo(DutchAdvert), main = "Dutch advertising expenditures",
  xlab = "Time", ylab = c("TV", "Radio"))


###################################################
### code chunk number 11: PepperPrice
###################################################
plot(PepperPrice, plot.type = "single", col = c("black", "slategray"),
  ylab = "Average monthly spot price",
  main = "European pepper prices")
legend("topleft", c("white", "black"), bty = "n", 
  col = c("slategray", "black"), lty = rep(1,2), lwd = 2)


