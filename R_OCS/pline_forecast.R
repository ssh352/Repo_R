setwd("D:/GitHub/Repo_R/R_OCS")

library(RODBC)
library(dplyr)
library(plyr)
library(zoo)
library(lubridate)
#library(spatstat)
#library(afex)

library(tseries)
library(forecast)

last_day <- function(date) {
  ceiling_date(date, "month") + months(1) - days(1)
}

zoo_series = emc_f04_zoo
h <- 12
b <- 6
#zoo_series = apply.monthly(sales_daily[[1]]
#h <- 3
#b <- 3
main_text = "Klin"
xlab_text = "time"
ylab_text = "sales"
                           
fcast_monthly <- function(zoo_series, fcast.period = 3, test.period = 3,
                          main_text = NULL, xlab_text = NULL, ylab_text = NULL) {
  h <- fcast.period
  b <- test.period
  #Добавляем маленькое значение вместо 0
  zoo_series[zoo_series == 0] <- 0.001
  
  #Создаем векторы с датами, для тестирования и для прогнозирования
  dates <- index(zoo_series)
  if (h-b>0) {
    test_dates <- c(dates,last_day(floor_date(end(zoo_series),"month") + months(seq(1,h-b,1))))
  } else if (h-b<0) {
    test_dates <- tail(dates, h-b)
  } else {
    test_dates <- dates
  }
  fdates <- c(dates, last_day(floor_date(tail(dates,1), "month")+ months(1:(h))))
  
  #Обрезаем неполный месяц в конце
  if (Sys.Date() < tail(dates, 1)) {
    dates <- head(dates,-1)
    zoo_series <- head(zoo_series, -1)
  }
  
  
  #Запоминаем начальные даты в формате вектора и в обычном
  date_start_ts <- as.double(unlist(strsplit(as.character(start(zoo_series)), "-")))
  date_start <- start(zoo_series)
  date_end <- end(zoo_series)
  
  #Создаем TS из zoo серии
  total <- ts(zoo_series, start=date_start_ts, frequency = 12)
  
  ################################
  ##  Обзорные графики          ##
  ################################
  
  #общая картина по продажам циски
  plot(dates, total, 
       main = main_text, sub = NULL, xlab = xlab_text, ylab = ylab_text,
       type="l", col=palette()[1])
  lines(dates, lowess(total)$y, type="l", lty="dashed", col=palette()[2])

  #Добавляем линии для решетки
  abline(h = axTicks(2),col="gray", lty="dashed")
  abline(v = dates[seq(3, length(dates), 3)],col="gray", lty="dashed")

  model <- lm(as.numeric(total) ~ as.numeric(dates))
  abline(model, col = "green")
  
  legend("topleft", 
         c("Real", "Trend line", "lowess"), 
         col=c("black","green", "red"),
         lty=c("solid","solid","dashed"),
         lwd=2,
         cex=1)
  
  #Сначала рисуем общие данные по продажам
  if (length(zoo_series)>=24) {
    plot(stl(total, s.window = "periodic"))
    mtext(paste(main_text, ": ", ylab_text, sep=""),side=3,outer=F, cex=1, line=2, adj=0.5)
  }
  
  ################################
  ##  Проверяем модель          ##
  ################################
  print("Строим тестовую модель")
  #Учим модель и делаем тестовый прогноз
  train <- ts(head(total, -b), start=date_start_ts, frequency = 12)

  #однопараметрическое преобразование Бокса-Кокса (пока не очень понимаю что это)
  Lw <- BoxCox.lambda(train, method="loglik")
  
  fc <- list()
  fc$tbats <- list()
  fc$arima <- list()
  fc$sum <- sum(head(tail(total,b),h))
  
  #Предсказываем на тестовом наборе
  #Посчитаем размер ошибки для обоих предсказаний
  #TBATS  
  fit.tbats <- tbats(train, lambda=Lw)
  fcast.tbats <- forecast(fit.tbats, h, lambda=Lw, level=c(seq(5,95,5)))
  #Соберем вместе все прогнозы и посмотрим какой больше всего подходит
  fcasts.mean <- as.vector(fcast.tbats$mean)
  dim(fcasts.mean) <- c(length(fcasts.mean),1)
  fcasts.all <- data.frame(cbind(fcast.tbats$lower, fcasts.mean, fcast.tbats$upper))
  fcasts.all <- apply(fcasts.all, 2, FUN=as.double)
  #Корректируем сравниваемые величины, чтобы не сравинивали с пустой
  fcasts.all.compare <- fcasts.all[c(1:b),]
  #Смотрим насколько сходится по сумме прогноз
  err <- abs(as.double(colSums(fcasts.all.compare) / fc$sum * 100 - 100))
  #Считаем корреляцию между настоящими данными и прогнозируемыми
  test.vector <- as.double(tail(total,b))
  test.cor <- 100 - as.double(sapply(1:ncol(fcasts.all.compare), function(i){
    cor(test.vector, as.double(fcasts.all.compare[,i])) * 100
  }))
  #Теперь сводим сумму по предсказанию и корреляцию  
  err_cor <- colSums(rbind(err,test.cor))
  names(err) <- c(seq(-5,-95,-5), 0, seq(5,95,5))
  ix <- which(err_cor==min(err_cor))
  #Ищем уровень с наименьшим расхождением
  fc$tbats$level <- as.numeric(names(err)[ix])
  #Запоминаем все эти данные
  fc$tbats$sum <- sum(fcasts.all[,ix])
  fc$tbats$mean <- c(tail(train,1),fcasts.all[,ix])
  fc$tbats$error <- round(err[ix],1)
  
  #ARIMA
  fit.arima <- auto.arima(train, lambda=Lw)
  fcast.arima <- forecast(fit.arima, h, lambda=Lw, level=c(seq(5,95,5)))
  #Соберем вместе все прогнозы и посмотрим какой больше всего подходит
  fcasts.mean <- as.vector(fcast.arima$mean)
  dim(fcasts.mean) <- c(length(fcasts.mean),1)
  fcasts.all <- data.frame(cbind(fcast.arima$lower, fcasts.mean, fcast.arima$upper))
  fcasts.all <- apply(fcasts.all, 2, FUN=as.double)
  #Корректируем сравниваемые величины, чтобы не сравинивали с пустой
  fcasts.all.compare <- fcasts.all[c(1:b),]
  #Смотрим насколько сходится по сумме прогноз
  err <- abs(as.double(colSums(fcasts.all.compare) / fc$sum * 100 - 100))
  #Считаем корреляцию между настоящими данными и прогнозируемыми
  test.vector <- as.double(tail(total,b))
  test.cor <- 100 - as.double(sapply(1:ncol(fcasts.all.compare), function(i){
    cor(test.vector, as.double(fcasts.all.compare[,i])) * 100
  }))
  #Теперь сводим сумму по предсказанию и корреляцию  
  err_corr <- colSums(rbind(err,test.cor))
  names(err) <- c(seq(-5,-95,-5), 0, seq(5,95,5))
  ix <- which(err_corr==min(err_corr))
  #Ищем уровень с наименьшим расхождением
  fc$arima$level <- as.numeric(names(err)[ix])
  #Запоминаем все эти данные
  fc$arima$sum <- sum(fcasts.all[,ix])
  fc$arima$mean <- c(tail(train,1),fcasts.all[,ix])
  fc$arima$error <- round(err[ix],1)
  
  lim_start <- floor_date(date_end, "month") - months(b + 3)
  if (h < 12) {
    lim_end <- lim_start + months(12)
  } else {
    lim_end <- lim_start + months(h + 6)
  }
  plot(dates, total, 
       main = paste("Testing : ", main_text, sep=""), sub = NULL, xlab = xlab_text, ylab = ylab_text,
       xlim=c(lim_start,lim_end), type="l", col=palette()[1])
  lines(tail(test_dates, h+1),fc$arima$mean, type="l", col=palette()[2], lwd=3)
  lines(tail(test_dates, h+1),fc$tbats$mean, type="l", col=palette()[3], lwd=3)
  lines(dates, lowess(total)$y, type="l", lty="dashed", col=palette()[4])
  #Добавляем линии для решетки
  abline(h = axTicks(2),col="gray", lty="dashed")
  abline(v = dates,col="gray", lty="dashed")
  
  legend("topleft", 
         c("Real", 
           paste("ARIMA (",fc$arima$error,"%)", sep=""),
           paste("TBATS (",fc$tbats$error,"%)", sep=""),
           "Lowess"), 
         col=palette()[c(1:4)],
         lty=c("solid","solid","solid","dashed"),
         lwd=2,
         cex=1)
  
  ################################
  ##  Прогноз                   ##
  ################################
  print("Считаем прогноз")
  Lw <- BoxCox.lambda(total, method="loglik")
  
  #Предсказываем на тестовом наборе
  #Посчитаем размер ошибки для обоих предсказаний
  #TBATS  
  fit.tbats <-tbats(total, lambda=Lw)
  fcast.tbats <- forecast(fit.tbats, h, lambda=Lw, level=abs(fc$tbats$level))
  if (fc$tbats$level > 0) {
    fc$tbats$mean <- c(tail(total,1),as.numeric(fcast.tbats$upper))
  } else {
    fc$tbats$mean <- c(tail(total,1),as.numeric(fcast.tbats$lower))
  }
  
  #ARIMA
  fit.arima <- auto.arima(total, lambda=Lw)
  fcast.arima <- forecast(fit.arima, h, lambda=Lw, level=abs(fc$arima$level))
  if (fc$arima$level > 0) {
    fc$arima$mean <- c(tail(total,1),as.numeric(fcast.arima$upper))
  } else {
    fc$arima$mean <- c(tail(total,1),as.numeric(fcast.arima$lower))
  }
  
  lim_start <- date_end - years(1)
  lim_end <- last_day(floor_date(date_end,"month") + months(h+1))
  plot(dates, total, 
       main = paste("Forecast : ", main_text, sep=""), sub = NULL, xlab = xlab_text, ylab = ylab_text,
       axes=FALSE,
       xlim=c(lim_start,lim_end), type="l", col=palette()[1])
  axis(side=2)
  axis(side=1, at = fdates, labels = substr(as.character(fdates),3,7), las=2)
  box()
  #Добавляем линии для решетки
  abline(h = axTicks(2),col="gray", lty="dashed")
  abline(v = fdates,col="gray", lty="dashed")
  
  lines(tail(fdates,h+1),fc$arima$mean, type="l", col=palette()[2], lwd=3)
  lines(tail(fdates,h+1),fc$tbats$mean, type="l", col=palette()[3], lwd=3)
  lines(dates, lowess(total)$y, type="l", lty="dashed", col=palette()[4])
  
  legend("topleft", 
         c("Real", 
           paste("ARIMA (",fc$arima$error,"%, lvl=",fc$arima$level,")", sep=""),
           paste("TBATS (",fc$tbats$error,"%, lvl=",fc$tbats$level,")", sep=""),
           "Lowess"), 
         col=palette()[c(1:4)],
         lty=c("solid","solid","solid","dashed"),
         lwd=2,
         cex=1)
  if (fc$arima$error <= fc$tbats$error) {
    print("Процент ошибки меньшу у ARIMA")
  } else {
    print("Процент ошибки меньшу у TBATS")
  }
  return(
    list(
      dates=tail(fdates,h),
      arima=tail(fc$arima$mean, -1), 
      arima_err=fc$arima$error, 
      tbats=tail(fc$tbats$mean, -1), 
      tbats_error=fc$tbats$error)
    )
}

#Достаем данные о продажам по линейкам из базы
con <- odbcDriverConnect("driver={SQL Server};server=plumbum.ural.ocs.ru;database=cisco;uid=sa;pwd=107109")
res <- sqlQuery(con, 'SELECT * FROM olap_pline')
odbcCloseAll()

res[is.na(res)] <- 0

index <- tail(names(res), length(names(res)) - 2)
ix_dates <- last_day(as.Date(paste(substr(index,1,4),substr(index,5,6),"01", sep="-")))

#Обрезаем неполный месяц в конце
if (Sys.Date() < tail(dates, 1)) {
  dates <- head(ix_dates,-1)
  res_cut <- res[,c(1:(length(res)-1))]
} else {
  dates <- ix_dates  
}

################################
##  Отчет по циско            ##
################################
pdf("cisco.pdf", encoding = "CP1251.enc")
#Теперь по всей компании в целом
ocs <- filter(res_cut, rbu != "Unknown")
ocs <- select(ocs, -pline, -rbu)
ocs_sum <- colSums(ocs) / 1000000
ocs_zoo <- zoo(ocs_sum, dates)

fcast_ocs <- fcast_monthly(zoo_series = ocs_zoo, fcast.period = 12, test.period = 12,
                           main_text = "OCS", ylab_text = "Sales (MM $)", xlab_text = "Time")

#Группируем данные по филиалам
cisco <- filter(res_cut, pline == "CDU" | pline == "CDL" |  pline == "CSD" |  pline == "CTD" | pline == "CNU" | pline == "CNL" |  pline == "CTN" |  pline == "CIP" |  pline == "CIS")
cisco_rbu <- select(cisco, -pline)
cisco_rbu_sum <- ddply(cisco_rbu, "rbu", numcolwise(sum))

#Собираем все данные по продажам
all_rbu <- zoo(,dates)
for (i in 2:nrow(cisco_rbu_sum)) {
  rbu <- zoo(as.integer(select(cisco_rbu_sum[i,],-rbu)), dates)
  all_rbu <- cbind(all_rbu, rbu)
  names(all_rbu)[length(names(all_rbu))] <- as.character(cisco_rbu_sum[i,1])
}

#Делаем прогноз по Cisco департаменту
rbu_fcast <- list()

total_zoo <- zoo(rowSums(all_rbu), dates) / 1000
fcast <- fcast_monthly(zoo_series = total_zoo, fcast.period = 12, test.period = 12,
                       main_text = "Cisco department", ylab_text = "Sales (K $)", xlab_text = "Time")

rbu_fcast[["all"]] <- fcast

for (i in 1:ncol(all_rbu)) {
  rbu_zoo <- zoo(all_rbu[,i], dates) / 1000
  if (tail(rbu_zoo,1) > 0) {
    print(paste("Обрабатываем:", names(all_rbu)[i]))
    rbu_zoo <- rbu_zoo[c(min(which(rbu_zoo > 0)):length(rbu_zoo))]
    rbu_name <- paste("F", substr(names(all_rbu)[i], 2,3), sep="")
    rbu_fcast[[names(all_rbu)[i]]] <- fcast_monthly(zoo_series = rbu_zoo, fcast.period = 6, test.period = 6,
                           main_text = paste(rbu_name, "Cisco"), ylab_text = "Sales (K $)", xlab_text = "Time")
  } else {
    print(paste("РБЮ:", names(all_rbu)[i], "пропущено."))
  }
}
dev.off()

fcast_f04 <- colSums(select(filter(res_cut, rbu == "Ф04"), -rbu, -pline))
fcast_f04_zoo <- zoo(fcast_f04, dates) / 1000
fcast_f04_zoo <- fcast_f04_zoo[c(min(which(fcast_f04_zoo > 0)):length(fcast_f04_zoo))]
fcast <- fcast_monthly(zoo_series = fcast_f04_zoo, fcast.period = 6, test.period = 6,
                       main_text = "Ф04", ylab_text = "Sales (K $)", xlab_text = "Time")


fcast_f04_cisco <- all_rbu[,4]
fcast <- fcast_monthly(zoo_series = fcast_f04_cisco, fcast.period = 6, test.period = 6,
                       main_text = "Ф04 Cisco", ylab_text = "Sales (K $)", xlab_text = "Time")

################################
##  Отчет по EMC              ##
################################
pdf("emc.pdf", encoding = "CP1251.enc")
#Теперь по всей компании в целом
ocs <- filter(res_cut, rbu != "Unknown")
ocs <- select(ocs, -pline, -rbu)
ocs_sum <- colSums(ocs) / 1000000
ocs_zoo <- zoo(ocs_sum, dates)

fcast_ocs <- fcast_monthly(zoo_series = ocs_zoo, fcast.period = 12, test.period = 12,
                           main_text = "OCS", ylab_text = "Sales (MM $)", xlab_text = "Time")

#Посчитаем EMC
emc <- filter(res_cut, pline == "EMC")
#Считаем весь департамент в целом
emc_all<- colSums(select(emc, -rbu, -pline)) / 1000
emc_all_zoo <- zoo(emc_all, dates)
fcast_all_emc <- fcast_monthly(zoo_series = emc_all_zoo, fcast.period = 6, test.period = 6,
                               main_text = "EMC Department", ylab_text = "Sales (K &)", xlab_text = "Time")

#Считаем наш филиал
emc_f04 <- as.numeric(emc[5,c(3:ncol(emc))])
emc_f04_zoo <- zoo(emc_f04, dates)
emc_f04_zoo <- emc_f04_zoo[c(min(which(emc_f04_zoo > 0)):length(emc_f04_zoo))]
emc_f04_zoo <- emc_f04_zoo / 1000
#Скорректируем данные по выбросам
emc_f04_zoo[as.Date("2012-06-30")] <- emc_f04_zoo[as.Date("2012-06-30")] - 3100
emc_f04_zoo[as.Date("2012-07-31")] <- emc_f04_zoo[as.Date("2012-06-30")] - 400
fcast_f04_emc <- fcast_monthly(zoo_series = emc_f04_zoo, fcast.period = 12, test.period = 12,
                           main_text = "F04 EMC", ylab_text = "Sales (K $)", xlab_text = "Time")
dev.off()

names <- c("Depart", "level", "Err %", as.character(fcast_f04_emc$dates))
row_emc_arima <- c("EMC arima", names(fcast_f04_emc$arima_err), fcast_f04_emc$arima_err[1], floor(fcast_f04_emc$arima))
row_emc_tbats <- c("EMC tbats", names(fcast_f04_emc$tbats_err), fcast_f04_emc$tbats_err[1], floor(fcast_f04_emc$tbats))

################################
##  Отчет по HP               ##
################################
pdf("HP.pdf", encoding = "CP1251.enc")
#Теперь по всей компании в целом
ocs <- filter(res_cut, rbu != "Unknown")
ocs <- select(ocs, -pline, -rbu)
ocs_sum <- colSums(ocs) / 1000000
ocs_zoo <- zoo(ocs_sum, dates)

fcast_ocs <- fcast_monthly(zoo_series = ocs_zoo, fcast.period = 12, test.period = 12,
                           main_text = "OCS", ylab_text = "Sales (MM $)", xlab_text = "Time")

#Посчитаем HP
hp <- filter(res_cut, pline == "HPR" | pline == "HPE" | pline == "HPS")
#Считаем весь департамент в целом
hp_all<- colSums(select(hp, -rbu, -pline)) / 1000
hp_all_zoo <- zoo(hp_all, dates)
fcast_all_hp <- fcast_monthly(zoo_series = hp_all_zoo, fcast.period = 6, test.period = 6,
                               main_text = "HP Department (HPR,HPE,HPS)", ylab_text = "Sales (K &)", xlab_text = "Time")

#Считаем наш филиал
hp_f04 <- as.numeric(hp[5,c(3:ncol(hp))])
hp_f04_zoo <- zoo(hp_f04, dates)
hp_f04_zoo <- hp_f04_zoo[c(min(which(hp_f04_zoo > 0)):length(hp_f04_zoo))]
hp_f04_zoo <- hp_f04_zoo / 1000
fcast_f04_hp <- fcast_monthly(zoo_series = hp_f04_zoo, fcast.period = 6, test.period = 6,
                               main_text = "F04 HP (HPR, HPE, HPS)", ylab_text = "Sales (K &)", xlab_text = "Time")
dev.off()

################################
##  Отчет для Новикова        ##
################################
pdf("ocs.pdf", encoding = "CP1251.enc")
#Теперь по всей компании в целом
ocs <- filter(res_cut, rbu != "Unknown")
ocs <- select(ocs, -pline, -rbu)
ocs_sum <- colSums(ocs) / 1000000
ocs_zoo <- zoo(ocs_sum, dates)

fcast_ocs <- fcast_monthly(zoo_series = ocs_zoo, fcast.period = 12, test.period = 12,
                           main_text = "OCS", ylab_text = "Sales (MM $)", xlab_text = "Time")

#Группируем данные по филиалам
cisco <- filter(res_cut, pline == "CDU" | pline == "CDL" |  pline == "CSD" |  pline == "CTD" | pline == "CNU" | pline == "CNL" |  pline == "CTN" |  pline == "CIP" |  pline == "CIS")
cisco_rbu <- select(cisco, -pline)
cisco_rbu_sum <- ddply(cisco_rbu, "rbu", numcolwise(sum))

#Собираем все данные по продажам
all_rbu <- zoo(,dates)
for (i in 2:nrow(cisco_rbu_sum)) {
  rbu <- zoo(as.integer(select(cisco_rbu_sum[i,],-rbu)), dates)
  all_rbu <- cbind(all_rbu, rbu)
  names(all_rbu)[length(names(all_rbu))] <- as.character(cisco_rbu_sum[i,1])
}

#Делаем прогноз по Cisco департаменту
rbu_fcast <- list()

total_zoo <- zoo(rowSums(all_rbu), dates) / 1000
fcast <- fcast_monthly(zoo_series = total_zoo, fcast.period = 12, test.period = 12,
                       main_text = "Cisco department", ylab_text = "Sales (K $)", xlab_text = "Time")

#Посчитаем EMC
emc <- filter(res_cut, pline == "EMC")
#Считаем весь департамент в целом
emc_all<- colSums(select(emc, -rbu, -pline)) / 1000
emc_all_zoo <- zoo(emc_all, dates)
fcast_all_emc <- fcast_monthly(zoo_series = emc_all_zoo, fcast.period = 6, test.period = 6,
                               main_text = "EMC department", ylab_text = "Sales (K $)", xlab_text = "Time")

dev.off()

################################
##  Отчет для кости           ##
################################
pdf("f04_and_emc.pdf", encoding = "CP1251.enc")

#Теперь по нашему филиалу только
f04 <- filter(res_cut, rbu == "Ф04")
f04 <- select(f04, -rbu, -pline)
f04_sum <- colSums(f04) / 1000
f04_zoo <- zoo(f04_sum, dates)
f04_zoo <- f04_zoo[c(min(which(f04_zoo > 0)):length(f04_zoo))]

fcast_all_f04 <- fcast_monthly(zoo_series = f04_zoo, fcast.period = 12, test.period = 12,
                               main_text = "F04", ylab_text = "Sales (K $)", xlab_text = "Time")

#Считаем наш филиал
emc_f04 <- colSums(select(filter(res_cut, rbu == "Ф04" & pline == "EMC"), -rbu, -pline))
emc_f04_zoo <- zoo(emc_f04, dates)
emc_f04_zoo <- emc_f04_zoo[c(min(which(emc_f04_zoo > 0)):length(emc_f04_zoo))]
emc_f04_zoo <- emc_f04_zoo / 1000
#Скорректируем данные по выбросам
emc_f04_zoo[as.Date("2010-12-31")] <- emc_f04_zoo[as.Date("2010-12-31")] - 1500
emc_f04_zoo[as.Date("2012-06-30")] <- emc_f04_zoo[as.Date("2012-06-30")] - 3100
emc_f04_zoo[as.Date("2012-07-31")] <- emc_f04_zoo[as.Date("2012-07-31")] - 400
fcast_f04_emc <- fcast_monthly(zoo_series = emc_f04_zoo, fcast.period = 12, test.period = 12,
                               main_text = "F04 EMC", ylab_text = "Sales (K $)", xlab_text = "Time")
dev.off()

names <- c("Depart", "level", "Err %", as.character(fcast_f04_emc$dates))
row_emc_arima <- c("EMC arima", names(fcast_f04_emc$arima_err), fcast_f04_emc$arima_err[1], floor(fcast_f04_emc$arima))
row_emc_tbats <- c("EMC tbats", names(fcast_f04_emc$tbats_err), fcast_f04_emc$tbats_err[1], floor(fcast_f04_emc$tbats))
row_f04_arima <- c("f04 arima", names(fcast_all_f04$arima_err), fcast_all_f04$arima_err[1], floor(fcast_all_f04$arima))
row_f04_tbats <- c("f04 tbats", names(fcast_all_f04$tbats_err), fcast_all_f04$tbats_err[1], floor(fcast_all_f04$tbats))

df <- as.data.frame(rbind(row_f04_arima,row_f04_tbats,row_emc_arima,row_emc_tbats))
names(df) <- df_names
#install.packages("xlsx")
library(xlsx)
write.xlsx(df, "f04_and_emc.xlsx")

################################
##  CSV                       ##
################################
#Создаем табличку с прогнозами
df <- data.frame(replicate(12 + h,numeric(0), simplify = F))
#names(df) <- as.character(tail(index(sales_weekly[[1]]),46 + h))
for (i in 1:length(shops)) {
  df[i,] <- tail(forecasts[[i]],46 + h)
}

end_date <- end(sales_weekly[[1]]) + h * 7
ix_date_forecast <- index(zoo(,seq(start(sales_weekly[[1]]),end_date,by="week")))

dimnames(df)[[1]] <- names(shops)
dimnames(df)[[2]] <- as.character(tail(ix_date_forecast,46 + h))
dimnames(df)[[2]][c(47:51)] <- paste("P ",tail(dimnames(df)[[2]],h),sep="")

write.csv(df, file="forecasts.csv")
