###################################################
library("fxregime")
library("lmtest")
data("FXRatesCHF", package = "fxregime")


###################################################
cny_usd <- with(FXRatesCHF, CNY/USD)
cny_usd <- window(cny_usd, start = as.Date("2005-05-01"))
plot(window(cny_usd, end = as.Date("2005-10-31")),
  type = "n", xlab = "Time", ylab = "FX rate", main = "CNY/USD")
lines(window(cny_usd, end = as.Date("2005-07-28")))


###################################################
plot(window(cny_usd, end = as.Date("2005-10-31")),
  xlab = "Time", ylab = "FX rate", main = "CNY/USD")


###################################################
cny <- fxreturns("CNY", frequency = "daily", start = as.Date("2005-07-25"), end = as.Date("2009-07-31"),
  other = c("USD", "JPY", "EUR", "GBP"))


###################################################
cny_lm <- fxlm(CNY ~ USD + JPY + EUR + GBP, data = window(cny, end = as.Date("2005-10-31")))
summary(cny_lm)


###################################################
cny_efp <- gefp(cny_lm, fit = NULL)
plot(cny_efp, aggregate = FALSE)
plot(cny_efp, functional = meanL2BB)
plot(cny_efp, functional = supLM(0.1))


###################################################
cny_mon <- fxmonitor(CNY ~ USD + JPY + EUR + GBP, start = as.Date("2005-11-01"),
  data = window(cny, end = as.Date("2006-05-31")), end = 4)
plot(cny_mon, aggregate = FALSE)


###################################################
cny_reg <- fxregimes(CNY ~ USD + JPY + EUR + GBP, data = cny, h = 20, breaks = 10)
plot(cny_reg)
summary(cny_reg)


###################################################
plot(cny_usd, xlab = "Time", ylab = "FX rate", main = "CNY/USD")
abline(v = as.Date("2005-07-25"), lty = 4, col = 4, lwd = 1.5)
abline(v = breakdates(cny_reg), lty = 2, lwd = 1.5)
abline(v = as.Date("2009-07-31"), lty = 4, col = 4, lwd = 1.5)


###################################################
cny2 <- fxreturns("CNY", frequency = "daily", start = as.Date("2009-08-01"), end = as.Date("2010-01-31"),
  other = c("USD", "JPY", "EUR", "GBP"))
cny2_lm <- fxlm(CNY ~ USD + JPY + EUR + GBP, data = cny2)


###################################################
head(FXRatesCHF[, c(1:6, 13)], 3)
inr <- fxreturns("INR", data = FXRatesCHF,
  other = c("USD", "JPY", "DUR", "GBP"), frequency = "weekly", 
  start = as.Date("1993-04-01"), end = as.Date("2008-01-04"))
head(inr, 3)


###################################################
inr_lm <- fxlm(INR ~ USD + JPY + DUR + GBP, data = inr)
coef(inr_lm)


###################################################
inr_efp <- gefp(inr_lm, fit = NULL)
sctest(inr_efp, functional = meanL2BB)
sctest(inr_efp, functional = maxBB)
plot(inr_efp, functional = meanL2BB)
plot(inr_efp, functional = maxBB, aggregate = FALSE,
  ylim = c(-2, 2))


###################################################
inr_reg <- fxregimes(INR ~ USD + JPY + DUR + GBP, data = inr,
  h = 20, breaks = 10)
plot(inr_reg)


###################################################
coef(inr_reg)[, 1:5]
inr_rf <- refit(inr_reg)
sapply(inr_rf, function(x) summary(x)$r.squared)
