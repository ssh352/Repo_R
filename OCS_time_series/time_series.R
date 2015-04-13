setwd("D:\\GitHub\\OCS_time_series")
# options(OutDec= ".")
data <- read.csv("LR_cisco_DIFF-RBU.csv", dec=",", header=FALSE, sep = ";", stringsAsFactors=TRUE)
myvector <- as.vector(as.matrix(data[1,c(2:73)]))
myts <- ts(myvector, start=c(2009, 1), end=c(2014, 12), frequency=12)
plot(decompose(myts, type="multiplicative"))

library(tseries)
library(forecast)
adf.test(myts, alternative=c('stationary'))
#С другой стороны, с достаточно высокой степенью уверенности можно утверждать, что разности первого порядка ряда стационарны, т.е. это интегрированный временной ряд первого порядка (этот факт в дальнейшем позволит нам применить методологию Бокса — Дженкинса).
adf.test(diff(myts), alternative=c('stationary'))
ndiffs(myts)

# ARIMA
#test <- ts(myvector[69:71], start=c(2014, 9), end=c(2014, 11), frequency=12)
#h <- length(test)
h <- 4
train <- ts(myvector, start=c(2009, 1), end=c(2014, 11), frequency=12)

fit.arima <- auto.arima(train, lambda=L)
fcast.arima <- forecast(fit.arima, h, lambda=L)

fit.nn <- nnetar(train, size=7, lambda=L)
fcast.nn <- forecast(fit.nn, h, lambda=L)

fit.tbats <-tbats(train, lambda=L)
fcast.tbats <- forecast(fit.tbats, h, lambda=L)

par(mfrow=c(3, 1))
plot(fcast.arima, include=3*h)
plot(fcast.nn, include=3*h)
plot(fcast.tbats, include=3*h)

# par(mfrow=c(1, 1))
#plot(test, type="l", col="red", lwd=5, xlab="Day", ylab="Price, $", main="December prices",
#     ylim=c(min(test, fcast.arima$mean, fcast.tbats$mean, fcast.nn$mean),
#            max(test, fcast.arima$mean, fcast.tbats$mean, fcast.nn$mean)))
#plot(as.numeric(test), type="l", col="red", lwd=5, xlab="Day", ylab="Price, $", main="December prices", ylim(C(10000000,25000000)))
#lines(as.numeric(fcast.nn$mean), col="green", lwd=3,lty=2)
#lines(as.numeric(fcast.tbats$mean), col="magenta", lwd=3,lty=2)
#lines(as.numeric(fcast.arima$mean), col="blue", lwd=3, lty=2)
#lines(fcast.nn$mean, col="green", lwd=3,lty=2)
#lines(fcast.tbats$mean, col="magenta", lwd=3,lty=2)
#lines(fcast.arima$mean, col="blue", lwd=3, lty=2)
#legend("topright", legend=c("Real Data","NeuralNet","TBATS", "ARIMA"), 
#       col=c("red","green", "magenta","blue"), lty=c(1,2,2,2), lwd=c(5,3,3,3))
grid()
