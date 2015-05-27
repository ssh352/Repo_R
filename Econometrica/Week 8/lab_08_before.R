library("lubridate") # работа с датами
library(lmtest)
library("zoo") # временные ряды
library("xts") # еще ряды
library("dplyr") # манипуляции с данными
library("ggplot2") # графики
library("forecast")

library("quantmod") # загрузка с finance.google.com
library("sophisthse") # загрузка с sophist.hse.ru

#8.2.1
y <- arima.sim(n=100, list(ar=0.7))
plot(y)
Acf(y)
Pacf(y)
tsdisplay(y)

y2 <- rnorm(100)
tsdisplay(y2)

y3 <- 1:100
tsdisplay(y3)

y4 <- arima.sim(n=100, list(ma=-0.8))
tsdisplay(y4)

y <- arima.sim(n=100, list(ma=-0.8, ar=-0.5))
tsdisplay(y)

#8.2.2
y <- arima.sim(n=100, list(order=c(0,1,0)))
tsdisplay(y)

#Случайное блуждание
y <- arima.sim(n=500, list(order=c(0,1,0)))
tsdisplay(y)

#Тренд
y <- seq(1, 10, length=100) + arima.sim(n=100, list(ar=0.7))
tsdisplay(y)

y <- seq(0, 2, length=100) + arima.sim(n=100, list(ar=0.7))
tsdisplay(y)

#8.2.3
y <- LakeHuron
tsdisplay(y)
dy <- diff(y)
tsdisplay(dy)

m1 <- Arima(y, order=c(2,0,0))
m2 <- Arima(y, order=c(1,0,1))
summary(m1)
summary(m2)
library(memisc)
AIC(m1)
AIC(m2)

m3 <- Arima(y, order=c(2,0,1))
summary(m3)
AIC(m3)

pr <- forecast(m2, h=5)
pr
plot(pr)

m4 <- Arima(y, order=c(1,1,0))
AIC(m4)
pr <- forecast(m4, h=5)
plot(pr)

ma <- auto.arima(y)
summary(ma)
pr <- forecast(ma, h=5)
plot(pr)


#8.2.4
#Sys.setlocale("LC_TIME","C")
getSymbols(Symbols = "GOOG", from="2014-01-01", to="2014-12-01")
head(GOOG)
tail(GOOG)

y <- GOOG$GOOG.Close
tsdisplay(y)

dy <- diff(y)
tsdisplay(dy)

m1 <- Arima(y, order=c(0,1,0))
pr <- forecast(m1, h=20)
plot(pr)

ma <- auto.arima(y)
summary(ma)

y <- sophisthse("POPNUM_Y")
tsdisplay(y)

m1 <- Arima(y, order=c(1,1,0), include.drift = TRUE)
summary(m1)
pr <- forecast(m1, h=5)
plot(pr)

#8.2.5
y <- sophisthse("CPI_M_CHI")
tsdisplay(as.ts(y))

time(y)

ym <- y[97:nrow(y),]
tsdisplay(as.ts(ym))

m1.s <- arima(ym, order=c(1,0,0), seasonal = c(1,0,1))
summary(m1.s)
AIC(m1.s)
pr <- forecast(m1.s, h=12)
plot(pr)

m1.a <- auto.arima(ym)
pr <- forecast(m1.a, h=12)
plot(pr)

#TEST
#Q 3
y <- arima.sim(n=1000, list(ar=0.7))
tsdisplay(y)
y <- arima.sim(n=1000, list(order=c(0,1,0), ar=1, ma=1))
tsdisplay(y)

#Q 12
set.seed(2)
y <- arima.sim(n=100, list(ar=0.99))
tsdisplay(y)

t <- time(y)
m12 <- lm(y ~ t + I(t^2) + I(t^3))
plot(m12)
plot(fitted(m12))
lines(y)
summary(m12)
coeftest(m12)
s <- summary(m12)

F <- s$fstatistic[1]
p.sum <- sum(coeftest(m12)[,4])

F / p.sum

#Q13
set.seed(5)
y <- arima.sim(n=100, list(order=c(0,0,2), ar=0.5, ma=0.5))
tsdisplay(y)

#Q 14
y <- sophisthse("HHI_Q_I")
yc <- head(y[,"HHI_Q_DIRI"], 89)
tail(yc)

m14 <- auto.arima(yc)
summary(m14)

m14.3.1.0 <- Arima(yc, order=c(3,1,0))
m14.1.1.0 <- Arima(yc, order=c(1,1,0))
m14.2.1.0 <- Arima(yc, order=c(2,1,0))
m14.1.1.1 <- Arima(yc, order=c(1,1,1))
AIC(m14.1.1.0)
AIC(m14.1.1.1)
AIC(m14.2.1.0)
AIC(m14.3.1.0)

#Q 15
yc <- y[30:61,"HHI_Q_DIRI"]
m15 <- auto.arima(yc)
AIC(m15)

#Q 16
yc <- head(y[,"HHI_Q_DIRI"], 89)
m16 <- Arima(yc, order=c(2,1,0))
fc <- forecast(m16, h=3)
plot(fc)
fc

#Q 17
y.test <- head(y[,"HHI_Q_DIRI"], 89)
y.pr <- head(y[,"HHI_Q_DIRI"], 86)
m17.0.1.0 <- Arima(y.pr, order=c(0,1,0))
m17.1.1.2 <- Arima(y.pr, order=c(1,1,2))
m17.1.1.3 <- Arima(as.ts(y.pr), order=c(1,1,3))
m17.1.1.3.s <- Arima(as.ts(y.pr), order=c(1,1,3), seasonal = c(0,1,0))
m17.2.1.2 <- Arima(y.pr, order=c(2,1,2))
summary(m17.1.1.3)

pr.0.1.0 <- forecast(m17.0.1.0, h=3)
pr.1.1.2 <- forecast(m17.1.1.2, h=3)
pr.1.1.3 <- forecast(m17.1.1.3, h=3)
pr.1.1.3.s <- forecast(m17.1.1.3.s, h=3)
pr.2.1.2 <- forecast(m17.2.1.2, h=3)

#Разница при введении сезонности
plot(y.pr)
lines(pr.1.1.3$fitted, col="blue")
lines(pr.1.1.3.s$fitted, col="green")
sum((as.numeric(y.pr) - pr.1.1.3$fitted)^2)
sum((as.numeric(y.pr) - pr.1.1.3.s$fitted)^2)

plot(pr.1.1.2)
lines(pr.1.1.2$fitted)

sum((as.numeric(tail(y.test, 3)) - pr.0.1.0$mean)^2)
sum((as.numeric(tail(y.test, 3)) - pr.1.1.2$mean)^2)
sum((as.numeric(tail(y.test, 3)) - pr.1.1.3$mean)^2)
sum((as.numeric(tail(y.test, 3)) - pr.2.1.2$mean)^2)

#Q 18
y <- head(y[,"HHI_Q_DIRI"], 89)
m18.1.1.1 <- Arima(y, order=c(1,1,1))
m18.1.1.1.s <- Arima(as.ts(y), order=c(1,1,1), seasonal = c(1,0,0))
AIC(m18.1.1.1)
AIC(m18.1.1.1.s)

#Q 19
y <- head(y[,"HHI_Q_DIRI"], 89)
y[62:69]
d <- rep(0, 89)
d[62:69] <- 1

m19 <- Arima(y, order=c(1,1,1), xreg = d)
summary(m19)
coef(m19)
pr <- forecast(m19, h=3, xreg=c(0,0,0))

m19 <- Arima(y, order=c(1,1,1))
pr <- forecast(m19, h=3)
plot(y)
lines(pr$fitted, col="green")
lines(pr$fitted, col="red")
lines(pr$fitted, col="blue")
