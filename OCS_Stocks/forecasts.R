library(car)
library(openxlsx)
library(xts)
library(ggplot2)
library(sqldf)
library(memisc)
library(lmtest)
library(forecast)
library(sqldf)
library(lubridate)
library(Hmisc)

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

i <- 64
#sapply(1:nrow(pn.selected), function(i) {
pn.fcasts <- t(sapply(1:nrow(pn.selected), function(i) {
  pn <- as.character(pn.selected$pn[i])
  lvl <- as.character(pn.selected$level[i])
  
  x <- as.numeric(sales[pn,])
  n <- length(x)
  if (sum(x) > 0) {
    min.v <- min(which(x != 0))
    min.non.zero <- ifelse((n-min.v) >= 12, min.v, (n-11))
  } else {
    min.non.zero <- (n - 11)
  }
  x.corr <- x[min.non.zero:n]
  n.corr <- length(x.corr)
  
  date.start <- as.numeric(unlist(strsplit(names(sales[pn,min.non.zero:n])[1],"-")))
  date.end <- as.Date(tail(names(sales[pn,min.non.zero:n]), 1))
  pn.ts <- ts(x.corr, start=date.start, frequency = 12)
  ifelse(n.corr <= 12, i.seasonal <- 0, i.seasonal <- 1)
  arima.fit <- arima(pn.ts, order=c(0,0,1), seasonal = c(0,i.seasonal,0))
  f.sales <- round(forecast(arima.fit, 3)$mean)
  #f.sales[1] <- f.sales[1] - tail(as.numeric(stocks[pn,]), 1)
  #f.dates <- tail(seq(date.end, by="month", length.out = 4), 3)
  row <- c(pn, lvl, f.sales)
  #colnames(row) <- c("pn", "level", f.dates)
  cat(paste(c(i,row,'\r\n'), collapse = '\t\t'))
  return(row)
}))

write.csv(cbind(pn.fcasts,stocks[ix,72]), "fcast.csv")

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


#######
# 10ть пн для FNOW
ixstocks <- as.data.frame(cbind(dimnames(stocks)[[1]], index(stocks)))
names(ixstocks) <- c("pn", "ix")
pn.fnow <- sqldf("SELECT 
  p.pn, p.level, s.ix,
  CASE 
    WHEN p.level LIKE 'green' THEN 1
    WHEN p.level LIKE 'yellow' THEN 2
    WHEN p.level LIKE 'orange' THEN 3
    WHEN p.level LIKE 'red' THEN 4
    ELSE 0
  END AS num_level
  FROM pns AS p
  LEFT JOIN ixstocks AS s ON p.pn = s.pn
  WHERE 
    p.pn NOT LIKE '%Bundle%' 
    and p.pn NOT LIKE 'L-%' 
    and p.pn NOT LIKE 'R-%' 
    and p.pn NOT LIKE 'ESA-%' 
    and p.pn NOT LIKE 'WSA-%' 
    and p.pn NOT LIKE 'LIC-%' 
    and p.pn NOT LIKE 'ISE-%' 
    and p.pn NOT LIKE '%DELIVERY%' 
    and p.pn NOT LIKE 'CON-%'
    and p.level <> 'red'
  ORDER BY num_level, p.pn
")

ix <- as.numeric(as.character(pn.fnow$ix))

pns.fnow <- pn.fnow
pns.fnow <- pn.fnow[11:20,]
n <- ncol(orders)
#df.fnow <- as.matrix(date=numeric(0), sale=numeric(0), other=numeric(0), income=numeric(0),
                      #reserved=numeric(0), order=numeric(0), price=numeric(0), sum=numeric(0),
                      #cost=numeric(0), c.price=numeric(0), stock=numeric(0))

df.fnow <- matrix(nrow=nrow(pns.fnow)*n*31, ncol=12)
count <- 0
for(k in 1:nrow(pns.fnow)){
#for(k in 1:2){
  pn <- as.character(pns.fnow$pn[k])
  ix <- as.numeric(pns.fnow$ix[k])
  cat(c(k, pn, ": "))
  sale.other <- 0
  stock.order <- 0
  for(j in 1:n) {
    cat(".")
    date.start <- as.Date(names(sales[pn,])[j])
    days.in.month <- monthDays(date.start)
    month.sales <- sales[pn, j]
    ifelse(j > 1, month.stock <- stocks[pn, j-1], month.stock <- 0)
    day.sales <- ceiling(month.sales / days.in.month)
    for(i in 1:days.in.month){
      count <- count + 1
      day.date <- date.start + days(i - 1)
      month.sales <- month.sales - day.sales
      ifelse(month.sales >= 0, current.day.sales <- day.sales, current.day.sales <- 0)
      if(month.stock < 0) {
        stock.order <- abs(month.stock)
        month.stock <- 0
      } else {
        month.stock <- month.stock - current.day.sales      
      }
      df.fnow[count, ] <- c(index=ix, date=day.date, sale=as.numeric(current.day.sales), other=sale.other, 
                              income=0, reserved=0, order=stock.order, price=100, sum=100*current.day.sales, 
                              cost=50, c.price=50 * current.day.sales, stock=month.stock)
      stock.order <- 0
      sale.other <- 0
    }
    stock.order <- stocks[pn, j] - month.stock
    if (stock.order > 0) {
      sale.other <- 0
    }else{
      sale.other <- abs(stock.order)
      stock.order <- 0
    }
  }
  cat("\r\n")
}
sales.data <- as.data.frame(na.omit(df.fnow))
pns.fnow$ix <- as.numeric(pns.fnow$ix)
dimnames(sales.data)[[2]] <- c("ix", "date", "sale", "other", "income", "reserved", "order", "price", "sum", "cost", "c.price", "stock")
sales.data$date <- as.Date(sales.data$date, origin = "1970-01-01")
sales.data <- merge(pns.fnow[, c("ix", "pn","level","num_level")], sales.data, by = intersect(names(pns.fnow[, c("ix", "pn","level","num_level")]), names(sales.data)))
sales.data$nn <- sales.data$pn
sales.data$name <- sales.data$pn

#sales.excel.june <- paste.table()
names(sales.excel.june) <- c("pn", "sale")
sales.june <- merge(pns.fnow[ ,"pn"], sales.excel.june, by = 1, all.x = TRUE)
is.na(sales.june$sale) <- 0

write.csv(sales.data[, c("nn","name", "date", "sale", "order", "other", "price", "cost", "stock")], "sales.csv", quote = FALSE)

head(df.fnow[, c("nn", "date", "sale", "order", "other", "stock")], 5)
tail(df.fnow[, c("nn", "date", "sale", "order", "other", "stock")], 500)
df.fnow[c(1:700), c("nn", "date", "sale", "order", "other", "stock")]
all <- rbind(
  sales = as.numeric(sales[pn,]),
  stocks = as.numeric(stocks[pn,])
)
colnames(all) <- names(sales)

#Попробуем построить прогнозы по SB
sb.sales <- sales[ix$V1, ]
plot(sb.sales)
