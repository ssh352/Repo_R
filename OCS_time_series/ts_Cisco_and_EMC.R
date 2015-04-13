<<<<<<< HEAD
setwd("D:\\GitHub\\R_project\\OCS_time_series")
=======
setwd("D:\\GitHub\\OCS_time_series")
>>>>>>> 1a4c9426c7f03df2ed6457cdf3ac426f86258f49
# options(OutDec= ".")
#data <- read.csv("LR_cisco_and_emc.csv", dec=",", header=FALSE, sep = ";", stringsAsFactors=TRUE)
data <- read.csv("LR_cisco_and_emc_ALL-RBU.csv", dec=",", header=FALSE, sep = ";", stringsAsFactors=TRUE)
vcisco <- as.vector(as.matrix(data[1,c(1:71)]))
vemc <- as.vector(as.matrix(data[2,c(1:71)]))
train_cisco <- ts(vcisco, start=c(2009, 1), end=c(2014, 11), frequency=12)
train_emc <- ts(vemc, start=c(2009, 1), end=c(2014, 11), frequency=12)
#plot(decompose(myts, type="multiplicative"))

library(tseries)
library(forecast)
h <- 4

# Cisco
#train_cisco <- ts(tscisco, start=c(2009, 1), end=c(2014, 11), frequency=12)
L_cisco <- BoxCox.lambda(train_cisco, method="loglik")
# ARIMA
fit_cisco.arima <- auto.arima(train_cisco, lambda=L_cisco)
fcast_cisco.arima <- forecast(fit_cisco.arima, h, lambda=L_cisco)
# TBATS
fit_cisco.tbats <-tbats(train_cisco, lambda=L_cisco)
fcast_cisco.tbats <- forecast(fit_cisco.tbats, h, lambda=L_cisco)

# EMC
#train_emc <- ts(tsemc, start=c(2009, 2), end=c(2014, 11), frequency=12)
L_emc <- BoxCox.lambda(train_emc, method="loglik")
# ARIMA
fit_emc.arima <- auto.arima(train_emc, lambda=L_emc)
fcast_emc.arima <- forecast(fit_emc.arima, h, lambda=L_emc)
# TBATS
fit_emc.tbats <-tbats(train_emc, lambda=L_emc)
fcast_emc.tbats <- forecast(fit_emc.tbats, h, lambda=L_emc)

fcast_cisco_arima <- ts(c(train_cisco,fcast_cisco.arima$mean), start=c(2009, 1), end=c(2015, 3), frequency=12)
fcast_emc_arima <- ts(c(train_emc,fcast_emc.arima$mean), start=c(2009, 1), end=c(2015, 3), frequency=12)

fcast_cisco_tbats <- ts(c(train_cisco,fcast_cisco.tbats$mean), start=c(2009, 1), end=c(2015, 3), frequency=12)
fcast_emc_tbats <- ts(c(train_emc,fcast_emc.tbats$mean), start=c(2009, 1), end=c(2015, 3), frequency=12)

par(mfrow=c(2, 1))
plot(fcast_cisco_arima, col="red", axes=FALSE, xlab="Years", ylab="MM, $",xaxt="n")
#plot(fcast_cisco_tbats, col="red", axes=FALSE, xlab="Years", ylab="K, $",xaxt="n")
title("Cisco")
#lines(fcast_cisco_tbats, col="green")
lines(train_cisco, col="blue")
ypos <- seq(2008,2016, by=1)
axis(1,las=1, at=ypos, labels=ypos)
xpos <- seq(0, 40000000, by=5000000)
axis(2, at=xpos, labels=xpos/1000000, las=1)
box()
grid()
#legend("topleft", legend=c("Real Data","ARIMA","TBATS"), col=c("blue","red", "green"), lty=c(1,1,1), lwd=c(1,1,1),cex=0.75, pch=1)

plot(fcast_emc_arima, col="red", axes=FALSE, xlab="Years", ylab="MM, $",xaxt="n")
#plot(fcast_emc_tbats, col="red", axes=FALSE, xlab="Years", ylab="K, $",xaxt="n")
title("EMC")
#lines(fcast_emc_tbats, col="green")
lines(train_emc, col="blue")
ypos <- seq(2008,2016, by=1)
axis(1,las=1, at=ypos, labels=ypos)
xpos <- seq(0, 40000000, by=5000000)
axis(2, at=xpos, labels=xpos/1000000, las=1)
box()
grid()
#legend("topleft", legend=c("Real Data","ARIMA","TBATS"), col=c("blue","red", "green"), lty=c(1,1,1), lwd=c(1,1,1),cex=0.75, pch=1)
