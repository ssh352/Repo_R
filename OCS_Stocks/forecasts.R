setwd("D:/GitHub/Repo_R/OCS_Stocks")

as.Date("01 Апрель 2008", "%d %B %Y")


library(car)
library(openxlsx)
library(xts)

#Тут грузим остатки по ПН
data <- as.data.frame(read.xlsx("stocks.xlsx", 1))
data[is.na(data)] <- 0
l <- ncol(data)
r <- nrow(data)

#Создаем список месяцев, сначала неполный, потом полный
data.dates <- as.Date(paste("01", data[2, 2:l]), "%d %B %Y")
dates <- seq(from = head(data.dates, 1), to = tail(data.dates, 1), by="month")
#Запомним ПН оборудования
rows.name <- data[4:r, 1]

#Запоминаем данные о закупках
orders <- data[4:r, 2:l]

#Добавим имена колонок и строк
dimnames(orders) <- list(rows.name, data.dates)
#head(orders.data)

plot(orders.data["CP-PWR-CUBE-3=",], type="l")

