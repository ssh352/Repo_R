setwd("D:/GitHub/Repo_R/OCS_Stocks")

as.Date("01 Апрель 2008", "%d %B %Y")


library(car)
library(openxlsx)
library(xts)
library(ggplot2)

##########
# Stocks #
##########
#Тут грузим остатки по ПН
data <- as.data.frame(read.xlsx("stocks.xlsx", 1))
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

##########
# Orders #
##########
#Тут грузим остатки по ПН
data <- as.data.frame(read.xlsx("stocks.xlsx", 2))
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

##########
# Sales  #
##########
#Тут грузим остатки по ПН
data <- as.data.frame(read.xlsx("stocks.xlsx", 3))
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

plot(stocks.dates, as.numeric(stocks["CP-PWR-CUBE-3=",]), type="l")
lines(orders.date, as.numeric(orders["CP-PWR-CUBE-3=",]), type="l", col = "green")
lines(sales.dates, as.numeric(sales["CP-PWR-CUBE-3=",]), type="l", col = "blue")
