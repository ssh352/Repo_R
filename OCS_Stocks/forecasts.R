library(car)
library(openxlsx)
library(xts)
library(ggplot2)
library(sqldf)
library(memisc)
library(lmtest)
library(forecast)

setwd("D:/GitHub/Repo_R/OCS_Stocks")
data.file <- "stocks CDU 2015-07-30.xlsx"

#Функции для экспертной оценки заказа на товар
mystats <- function(x, k = 1, show.cond = FALSE) {
  n <- length(x)
  if (sum(x) > 0) {
    min.v <- min(which(x != 0))
    min.non.zero <- ifelse((n-min.v) >= 12, min.v, (n-12))
  } else {
    min.non.zero <- (n - 12)
  }
  
  x.corr <- x[min.non.zero:n]
  ix <- as.Date(names(x.corr))
  ix.pred <- tail(seq(from=tail(ix,1),by="month",length.out = 7), -1)
  x.df <- as.data.frame(t(rbind(date=ix,data=x.corr)))
  x.df.pred <- data.frame(date=as.numeric(ix.pred))
  m <- sum(x.corr) / n
  
  #ПРоверка первого условия
  #Строим линейную модель через данные
  model <- lm(data=x.df, data~date+I(date^2))
  predict(model, x.df.pred)
  sigma <- summary(model)$sigma
  f <- summary(model)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  #Если p.value маленькое, то значит данные линейно зависимы и их можно предсказывать
  cond.1 <- as.logical(p < (0.02*k))

  #Проверка ВТОРОГО условия
  sigma <- sd(x.corr)
  cond.2 <- (m - k * sigma > 0)
  
  #Проверка ТРЕТЬЕГО условия
  a3 <- table(x.corr > 0)
  t <- a3["TRUE"]
  f <- a3["FALSE"]
  if (is.na(t) == TRUE) {t <- 0}
  if (is.na(f) == TRUE) {f <- 0}
  ifelse((sum(a3) * f / t) < 3, cond.3 <- TRUE, cond.3 <- FALSE)
  
  if (show.cond == TRUE) {
    #print(c(all(abs(x.corr - m)), "<", (k * sigma)))
    print(c((m - k * sigma), ">", "0"))
    print(a3)
  }
  return(c(cond.1=cond.1, cond.2=cond.2, cond.3=cond.3)) 
}
level <- function(conditions) {
  l1 <- "green"
  l2 <- "yellow"
  l3 <- "orange"
  l4 <- "red"
  c1 <- conditions[1]
  c2 <- conditions[2]
  c3 <- conditions[3]
  if (all(c(c1, c2, c3)) == TRUE) {
    return(l1)
  } else if (c3 == TRUE && (c1 == TRUE || c2 == TRUE)) {
    return(l2)
  } else if (c3 == TRUE && (c1 == FALSE && c2 == FALSE)) {
    return(l3)
  } else {
    return(l4)
  }
}

##########
# Stocks #
##########
#Тут грузим остатки по ПН
data <- as.data.frame(read.xlsx(data.file, 1))
data[is.na(data)] <- 0
l <- ncol(data)
r <- nrow(data)

#Создаем список месяцев, сначала неполный, потом полный
stocks.dates <- as.Date(paste("01", data[2, 2:l]), "%d %B %Y")
#dates <- seq(from = head(data.dates, 1), to = tail(data.dates, 1), by="month")
#Запомним ПН оборудования
rows.name <- data[4:r, 1]

#Запоминаем данные о закупках
stocks <- data[4:r, 2:l]
stocks <- as.data.frame(sapply(stocks, as.numeric)) 

#Добавим stocks колонок и строк
dimnames(stocks) <- list(rows.name, stocks.dates)

stocks <- stocks[, -c(1,74:76)]
dim(stocks)

##########
# Orders #
##########
#Тут грузим остатки по ПН
data <- as.data.frame(read.xlsx(data.file, 2))
data[is.na(data)] <- 0
l <- ncol(data)
r <- nrow(data)

#Создаем список месяцев, сначала неполный, потом полный
orders.date <- as.Date(paste("01", data[2, 2:l]), "%d %B %Y")
#dates <- seq(from = head(data.dates, 1), to = tail(data.dates, 1), by="month")
#Запомним ПН оборудования
rows.name <- data[4:r, 1]

#Запоминаем данные о закупках
orders <- data[4:r, 2:l]
orders <- as.data.frame(sapply(orders, as.numeric)) 

#Добавим имена колонок и строк
dimnames(orders) <- list(rows.name, orders.date)

orders <- orders[, -c(1,2,75)]
dim(orders)

##########
# Sales  #
##########
#Тут грузим остатки по ПН
data <- as.data.frame(read.xlsx(data.file, 3))
data[is.na(data)] <- 0
l <- ncol(data)
r <- nrow(data)

#Создаем список месяцев, сначала неполный, потом полный
sales.dates <- as.Date(paste("01", data[2, 2:l]), "%d %B %Y")
#dates <- seq(from = head(data.dates, 1), to = tail(data.dates, 1), by="month")
#Запомним ПН оборудования

#Запоминаем данные о закупках
sales <- data[4:r, 2:l]
rows.name <- data[4:r, 1]
sales <- as.data.frame(sapply(sales, as.numeric)) 
sales2 <- aggregate(. ~ rows.name, cbind(rows.name,sales), sum)
rows.name <- sales2[, 1]
sales <- sales2[,-1]

#Добавим имена колонок и строк
dimnames(sales) <- list(rows.name, sales.dates)

sales <- sales[,-c(73)]
dim(sales)

#Запоним даты
dates <- as.Date(names(sales))
plot(dates, as.numeric(stocks["CP-PWR-CUBE-3=",]), type="l")
lines(dates, as.numeric(orders["CP-PWR-CUBE-3=",]), type="l", col = "green")
lines(dates, as.numeric(sales["CP-PWR-CUBE-3=",]), type="l", col = "blue")
lines(dates, lowess(as.numeric(sales["CP-PWR-CUBE-3=",]))$y, type="l", col = "red")


###########
# Пробуем найти ПН для прогнозов
###########
#Сначала обрежем поквартально наши данные
#stocks["CP-PWR-CUBE-3=",]
#dim(stocks["CP-7821-K9=",-c(97,98)])

#orders["CP-PWR-CUBE-3=",]
#dim(orders["CP-7821-K9=",-c(1)])

#sales["CP-PWR-CUBE-3=",]
#dim(sales["CP-7821-K9=",-c(1:6,103,104)])

#Коэфицент для доверительных интервалов
k <- 1

stats <- sapply(1:nrow(sales), function(i){
#stats <- sapply(1:200, function(i){
  conditions <- mystats(sales[i, ], k)
  return(level(conditions))
})

table(stats)
ix <- which(stats!="red")
pns <- as.data.frame(cbind(dimnames(sales[ix,])[[1]],stats[ix]))
dimnames(pns)[[2]] <- c("pn", "level")

pn.selected <- sqldf("SELECT pn, level,
  CASE 
    WHEN level LIKE 'green' THEN 1
    WHEN level LIKE 'yellow' THEN 2
    WHEN level LIKE 'orange' THEN 3
    WHEN level LIKE 'red' THEN 4
    ELSE 0
  END AS num_level
  FROM pns WHERE 
    pn NOT LIKE '%Bundle%' 
    and pn NOT LIKE 'L-%' 
    and pn NOT LIKE 'R-%' 
    and pn NOT LIKE 'ESA-%' 
    and pn NOT LIKE 'WSA-%' 
    and pn NOT LIKE 'LIC-%' 
    and pn NOT LIKE 'ISE-%' 
    and pn NOT LIKE '%DELIVERY%' 
    and pn NOT LIKE 'CON-%'
  ORDER BY num_level, pn")

i <- 1
#sapply(1:nrow(pn.selected), function(i) {
sapply(1:10, function(i) {
  pn <- as.character(pn.selected$pn[i])
  
  x <- as.numeric(sales[pn,])
  n <- length(x)
  if (sum(x) > 0) {
    min.v <- min(which(x != 0))
    min.non.zero <- ifelse((n-min.v) >= 12, min.v, (n-12))
  } else {
    min.non.zero <- (n - 12)
  }
  x.corr <- x[min.non.zero:n]
  
  date.start <- as.numeric(unlist(strsplit(names(sales[pn,min.non.zero:n])[1],"-")))
  date.end <- as.Date(tail(names(sales[pn,min.non.zero:n]), 1))
  pn.ts <- ts(x.corr, start=date.start, frequency = 12)
  arima.fit <- arima(pn.ts, order=c(1,2,1), seasonal = c(0,2,1))
  f.sales <- data.frame(forecast(arima.fit, 3)$mean)
  names(f.sales) <- tail(seq(date.end, by="month", length.out = 4), 3)
  row <- cbind(pn.selected[i,c(1,2)], f.sales)
  return(f.sales)
})

pn <- "AIR-PWRINJ4="
plot(dates, as.numeric(sales[pn,]), type="l")
lines(dates, as.numeric(stocks[pn,]), type="l", col="green")
lines(dates, as.numeric(orders[pn,]), type="l", col="red")

pn.ts <- ts(as.numeric(x), start=c(2010,05,01), frequency = 12)
arima.fit <- auto.arima(pn.ts)
plot(arima.fit)
forecast(arima.fit, 6)

arima.fit <- arima(pn.ts, order=c(1,2,1), seasonal = c(0,2,1))
forecast(arima.fit, 6)
plot(forecast(arima.fit, 6))
lines(fitted(m.arima), col="green")

pn.ts.2 <- ts(tail(as.numeric(x),-6), start=c(2010,05,01), frequency = 12)
arima.fit.2 <- arima(pn.ts.2, order=c(1,2,1), seasonal = c(0,2,1))
f <- forecast(arima.fit.2, 6)
lines(f$mean, col="green")

t.value <- data.frame(ar=0, i=1, ma=0, s1 = 0, s2 = 0, s3 = 0, aic = 0, v12 = 0)

r <- 0
for (o1 in 0:3) {
  for (o2 in 0:3) {
    for (o3 in 0:1) {
      for (s1 in 0:2) {
        for (s2 in 0:2) {
          for (s3 in 0:2) {
            r <- r + 1
            print(c(r, o1, o2, o3, s1, s2, s3))
            aic <- NA
            v12 <- NA
            try({
              m.arima <- arima(pn.ts, order=c(o1,o2,o3), seasonal = c(s1,s2,s3))
              aic <- AIC(m.arima)
              v12 <- sum(m.arima$residuals^2)
            })
            t.value[r, ] <- c(o1, o2, o3, s1, s2, s3, aic, v12)
          }
        }
      }
    }
  }
}

res <- na.omit(t.value)
sqldf("SELECT * FROM res ORDER BY aic DESC")
sqldf("SELECT * FROM res ORDER BY v12 DESC")
